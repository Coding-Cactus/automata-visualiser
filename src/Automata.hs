{-# LANGUAGE GADTs #-}

module Automata (
  AutomatonBuilder,
  StackTransition (..),
  TuringMachineTransition (..),
  TuringMachineDirection (..),
  AutomatonConfig (..),
  defaultConfig,
  setConfig,
  AcceptStyle (..),
  render,
  state,
  initial,
  final,
  (>--),
  (-->),
  tr,
  tr',
  bendRight,
  bendRight',
  bendLeft,
  bendLeft',
  (~~),
  leftOf,
  rightOf,
  above,
  below,
  position,
  tikz,
  svg,
) where

import Automata.Render
import Automata.Types

import Control.Monad (void)
import Control.Monad.State qualified as S
import Data.Bool (bool)
import Data.List (nub)
import Data.Maybe (fromMaybe)

infixl 5 >--
(>--) :: State s -> [t] -> (State s, [t])
s >-- condition = (s, condition)

infixl 5 -->
(-->) :: (TransitionLabel t) => (State s, [t]) -> State s -> AutomatonBuilder s t
(u, c) --> v = tr u v c

tr :: (TransitionLabel t) => State s -> State s -> [t] -> AutomatonBuilder s t
tr (S u _) (S v _) t = S.modify (\a -> a{transitions = T (length $ transitions a) u v t Nothing : transitions a})

tr' :: (TransitionLabel t) => State s -> State s -> t -> AutomatonBuilder s t
tr' s1 s2 t = tr s1 s2 [t]

bendRight' :: Double -> AutomatonBuilder s t -> AutomatonBuilder s t
bendRight' bend createTransition = do
  old <- S.gets transitions
  createTransition
  current <- S.gets transitions

  let new = filter (`notElem` old) current

  case new of
    [t] -> void $ S.modify (\a -> a{transitions = updateTransition t (transitions a)})
    _ -> pure ()
 where
  updateTransition t = map (\t'@(T tid u v l b) -> bool t' (T tid u v l (Just $ fromMaybe 0 b + bend)) (t' == t))

bendLeft' :: Double -> AutomatonBuilder s t -> AutomatonBuilder s t
bendLeft' bend = bendRight' (-bend)

bendRight :: AutomatonBuilder s t -> AutomatonBuilder s t
bendRight = bendRight' 1

bendLeft :: AutomatonBuilder s t -> AutomatonBuilder s t
bendLeft = bendLeft' 1

infixl 6 ~~
(~~) :: (Label t, Label w) => t -> (w, w) -> StackTransition t w
(~~) = StackT

state :: (Label s) => s -> S.State (Automaton s t) (State s)
state name = do
  a <- S.get
  let newS = S (length $ states a) name
  S.put $ a{states = newS : states a}
  pure newS

initial :: State s -> AutomatonBuilder s t
initial (S sid _) = S.modify $ \a -> a{initialS = sid}

final :: State s -> AutomatonBuilder s t
final (S sid _) = S.modify $ \a -> a{finalS = sid : finalS a}

leftOf :: State s -> State s -> AutomatonBuilder s t
x `leftOf` y = S.modify $ addConstraint (PosCon x y 0 1)

above :: State s -> State s -> AutomatonBuilder s t
x `above` y = S.modify $ addConstraint (PosCon x y (pi / 2) 1)

rightOf :: State s -> State s -> AutomatonBuilder s t
x `rightOf` y = S.modify $ addConstraint (PosCon y x 0 1)

below :: State s -> State s -> AutomatonBuilder s t
x `below` y = S.modify $ addConstraint (PosCon y x (pi / 2) 1)

position :: State s -> State s -> Double -> Double -> AutomatonBuilder s t
position x y theta dist = S.modify $ addConstraint (PosCon x y (normalise $ -(theta / 180 * pi)) dist)
 where
  normalise t
    | t < 0 = normalise $ t + 2 * pi
    | t > 2 * pi = normalise $ t - 2 * pi
    | otherwise = t

addConstraint :: PositionConstraint s -> Automaton s t -> Automaton s t
addConstraint c a
  | validConstraints cons = a{positions = cons}
  | otherwise =
      error $
        "Circular positioning constraint detected due to constraint between states named `"
          <> show (fst $ conToPair c)
          <> "` and `"
          <> show (snd $ conToPair c)
          <> "`."
 where
  cons = c : positions a

validConstraints :: [PositionConstraint s] -> Bool
validConstraints [] = True
validConstraints cons = not $ checkCycle cons [] []
 where
  -- checkCycle edgesRemaining nodesInComponent nodesToBeAddedToComponent
  checkCycle cs [] [] = checkCycle cs [] [fst $ conToPair $ head cs] -- initialise search through next component
  checkCycle [] sts xs = (sts ++ xs) /= nub (sts ++ xs) -- gone through all edges, check for cycle
  checkCycle cs sts [] = (sts /= nub sts) || checkCycle cs [] [] -- check cycle in connected component, if acyclic check next component
  checkCycle cs sts (x : xs) = checkCycle (filter (`notElem` edges) cs) (x : sts) (xs ++ map (`without` x) edges) -- add node to connected compnent + add neighbours to queue
   where
    edges = filter (constrained x) cs
