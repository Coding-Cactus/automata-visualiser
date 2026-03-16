{-# LANGUAGE GADTs #-}

module Automata (
  AutomatonBuilder,
  StackTransition,
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
  (~~),
  leftOf,
  rightOf,
  above,
  below,
  tikz,
  svg
) where

import Automata.Types
import Automata.Render

import qualified Control.Monad.State as S
import Data.List (nub)



infixl 5 >--
(>--) :: State s -> [t] -> (State s, [t])
s >-- condition = (s, condition)

infixl 5 -->
(-->) :: TransitionLabel t => (State s, [t]) -> State s -> AutomatonBuilder s t
(u, c) --> v = tr u v c

tr :: TransitionLabel t => State s -> State s -> [t] -> AutomatonBuilder s t
tr (S u _) (S v _) t = S.modify (\a -> a { transitions  = T (length $ transitions a) u v t : transitions a})

tr' :: TransitionLabel t => State s -> State s -> t -> AutomatonBuilder s t
tr' (S u _) (S v _) t = S.modify (\a -> a { transitions  = T (length $ transitions a) u v [t] : transitions a})

infixl 6 ~~
(~~) :: (Label t, Label w) => t -> (w, w) -> StackTransition t w
(~~) = StackT

state :: Label s => s -> S.State (Automaton s t) (State s)
state name = do
  a <- S.get
  let newS = S (length $ states a) name
  S.put $ a { states = newS : states a }
  pure newS


initial :: State s -> AutomatonBuilder s t
initial (S sid _) = S.modify $ \a -> a { initialS = sid }

final :: State s -> AutomatonBuilder s t
final (S sid _) = S.modify $ \a -> a { finalS = sid : finalS a }


leftOf :: State s -> State s -> AutomatonBuilder s t
x `leftOf` y = S.modify $ addConstraint (Le x y)

above :: State s -> State s -> AutomatonBuilder s t
x `above` y = S.modify $ addConstraint (Ab x y)

rightOf :: State s -> State s -> AutomatonBuilder s t
x `rightOf` y = S.modify $ addConstraint (Le y x)

below :: State s -> State s -> AutomatonBuilder s t
x `below` y = S.modify $ addConstraint (Ab y x)

addConstraint :: PositionConstraint s -> Automaton s t -> Automaton s t
addConstraint c a
  | validConstraints cons = a { positions = cons }
  | otherwise = error $
                  "Circular positioning constraint detected due to constraint between states named `" <>
                  show (fst $ conToPair c) <> "` and `" <> show (snd $ conToPair c) <> "`."
  where cons = c : positions a

validConstraints :: [PositionConstraint s] -> Bool
validConstraints [] = True
validConstraints cons = not $ checkCycle cons [] []
  where
    -- checkCycle edgesRemaining nodesInComponent nodesToBeAddedToComponent
    checkCycle cs [] [] = checkCycle cs [] [fst $ conToPair $ head cs] -- initialise search through next component
    checkCycle [] sts xs = (sts ++ xs) /= nub (sts ++ xs) -- gone through all edges, check for cycle
    checkCycle cs sts [] = (sts /= nub sts) || checkCycle cs [] [] -- check cycle in connected component, if acyclic check next component
    checkCycle cs sts (x:xs) = checkCycle (filter (`notElem` edges) cs) (x:sts) (xs ++ map (`without` x) edges) -- add node to connected compnent + add neighbours to queue
      where edges = filter (constrained x) cs

