module MyLib (a1) where

import qualified Control.Monad.State as S

data Automaton = Automaton {
  states :: [State],
  transitions :: [Transition],
  initialS :: Int,
  finalS :: [Int]
} deriving Show


type AutomatonBuilder = S.State Automaton ()


data State = S Int String

instance Show State where
  show (S _ name) = show name


type Transition = (State, State, TransitionCondition)


data TransitionCondition = Simple Char | Stack Char (Char, Char)

instance Show TransitionCondition where
  show (Simple c) = show c
  show (Stack c (s1, s2)) = show c <> ", " <> show s1 <> "->" <> show s2


class ToTransition a where
  toTransition :: a -> TransitionCondition

instance ToTransition Char where
  toTransition = Simple

instance ToTransition TransitionCondition where
  toTransition = id


infixl 5 >-|
(>-|) :: ToTransition t => State -> t -> (State, TransitionCondition)
s >-| cond = (s, toTransition cond)

infixl 5 |->
(|->) :: (State, TransitionCondition) -> State -> S.State Automaton ()
(s1, c) |-> s2 = S.modify (\a -> a { transitions = (s1, s2, c) : transitions a })

infixl 6 ~~
(~~) :: Char -> (Char, Char) -> TransitionCondition
(~~)  = Stack


state :: String -> S.State Automaton State
state x = do
  a <- S.get
  let newS = S (length $ states a) x
  S.put $ a { states = newS : states a }
  pure newS


initial :: State -> AutomatonBuilder
initial (S sid _) = S.modify $ \a -> a { initialS = sid }

final :: State -> AutomatonBuilder
final (S sid _) = S.modify $ \a -> a { finalS = sid : finalS a }


a1 :: AutomatonBuilder
a1 = do
  a <- state "a"
  a' <- state "a"
  b <- state "b"

  initial a
  final a
  final b

  a >-|'a'|-> b
  a >-|'a' ~~ ('a', 'b')|-> b
