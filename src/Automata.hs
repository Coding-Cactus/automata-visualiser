{-# LANGUAGE GADTs #-}

module Automata where

import Automata.Types
import Automata.Render

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

a1 = do
  a <- state "a"
  b <- state "a"
  c <- state "c"
  d <- state "c"
  e <- state "abc"
  f <- state "hi"

  initial a
  final a
  final b

  a >-|'a'|-> b
  --a >-|(0::Int) ~~ ('a', 'b')|-> b
  b >-|'b'|-> c
  c >-|'c'|-> d
  d >-|'d'|-> f
  e >-|'e'|-> b
