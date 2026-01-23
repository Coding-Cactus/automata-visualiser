{-# LANGUAGE GADTs #-}

module Automata where

import Automata.Types
import Automata.Render
import Automata.Layout.Energy

import qualified Control.Monad.State as S



infixl 5 >-|
(>-|) :: State s -> t -> (State s, t)
s >-| condition = (s, condition)

infixl 5 |->
(|->) :: TransitionLabel t => (State s, t) -> State s -> S.State (Automaton s t) ()
(s1, c) |-> s2 = S.modify (\a -> a { transitions = T s1 s2 c : transitions a })

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

-- TODO: ensure a node only relates to one other node.
--       i.e. can have a `leftOf` b, b `above` b. But cannot have 'b' as different.
--       Also cannot have contradicting leftOf, rightOf. or a L b, b L a
--       Checking for cycles in realtion graph might be sufficient
leftOf :: State s -> State s -> AutomatonBuilder s t
x `leftOf` y = S.modify $ \a -> a { positions = Le x y : positions a }

above :: State s -> State s -> AutomatonBuilder s t
x `above` y = S.modify $ \a -> a { positions = Ab x y : positions a }

rightOf :: State s -> State s -> AutomatonBuilder s t
x `rightOf` y = S.modify $ \a -> a { positions = Le y x : positions a }

below :: State s -> State s -> AutomatonBuilder s t
x `below` y = S.modify $ \a -> a { positions = Ab y x : positions a }


a1 = do
  a <- state "a"
  b <- state "b"
  c <- state "c"
  d <- state "d"
  e <- state "e"
  f <- state "f"
  -- g <- state "g"
  -- h <- state "h"

  initial a
  final f

  -- a >-|'a'|-> h
  a >-|'b'|-> e
  b >-|'b'|-> a
  c >-|'a'|-> b
  c >-|'b'|-> f
  -- d >-|'b'|-> h
  e >-|'a'|-> c
  f >-|'a'|-> d
  -- f >-|'b'|-> h
  -- g >-|'a'|-> a
  -- h >-|'b'|-> g

  a `leftOf` b
  b `above` d
  c `leftOf` a
  c `above` a
  c `above` f
  e `below` a
