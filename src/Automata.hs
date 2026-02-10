{-# LANGUAGE GADTs #-}

module Automata where

import Automata.Types
import Automata.Render

import qualified Control.Monad.State as S
import Data.List (nub)



infixl 5 >--
(>--) :: State s -> [t] -> (State s, [t])
s >-- condition = (s, condition)

infixl 5 -->
(-->) :: TransitionLabel t => (State s, [t]) -> State s -> S.State (Automaton s t) ()
(S u _, c) --> (S v _) = S.modify (\a -> a { transitions = T (length $ transitions a) u v c : transitions a })

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

a1 :: AutomatonBuilder String Int
a1 = do
  a <- state "a"
  b <- state "b"
  c <- state "c"

  initial a
  final a

  a >--[1]--> b
  a >--[0]--> a
  a >--[0]--> c

  b >--[0]--> b
  b >--[1]--> b
  b >--[2]--> b
  a >--[0]--> b

  c >--[0]--> c
  c >--[0]--> b
  b >--[1]--> c
  c >--[0,1]--> b

  c `below` a

  x <- state "x"
  y <- state "y"
  z <- state "z"

  final x

  x >--[1]--> y
  x >--[0]--> x
  x >--[0]--> z

  y >--[0]--> y
  y >--[1]--> y
  y >--[2]--> y
  x >--[0]--> y

  z >--[0]--> z
  z >--[0]--> y
  y >--[1]--> z
  z >--[0,1]--> y

  z `below` x



  h <- state "h"
  i <- state "i"
  j <- state "j"

  final h

  h >--[1]--> i
  h >--[0]--> h
  h >--[0]--> j

  i >--[0]--> i
  i >--[1]--> i
  i >--[2]--> i
  h >--[0]--> i

  j >--[0]--> j
  j >--[0]--> i
  i >--[1]--> j
  j >--[0,1]--> i

  j `below` h
