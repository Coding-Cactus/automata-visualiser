{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Automata.Types  where

import Data.Text (Text, unpack, pack, singleton)
import qualified Control.Monad.State as S
import Data.Bool

data Automaton s t = Automaton {
  states :: [State s],
  transitions :: [Transition t],
  initialS :: Int,
  finalS :: [Int],
  positions :: [PositionConstraint s]
} deriving Show


type AutomatonBuilder s t = S.State (Automaton s t) ()


data State s where
  S :: Label s => Int -> s -> State s

instance Show (State s) where
  show (S _ name) = unpack $ drawLabel name

instance Eq (State s) where
  (S id1 _) == (S id2 _) = id1 == id2


class Label a where
  drawLabel :: a -> Text

instance Label Char where
  drawLabel = singleton

instance Label String where
  drawLabel = pack

instance Label Text where
  drawLabel = id

instance Label Int where
  drawLabel = pack . show

instance Label Float where
  drawLabel = pack . show

instance Label Double where
  drawLabel = pack . show


data Transition t where
  -- T id uId vId label
  T :: TransitionLabel t => Int -> Int -> Int -> [t] -> Transition t

instance Show (Transition t) where
  show (T i s1 s2 t) = show (i, s1, s2, map toTransition t)

instance Eq (Transition t) where
  (T i1 _ _ _) == (T i2 _ _ _) = i1 == i2

class TransitionLabel a where
  toTransition :: a -> Text

instance TransitionLabel Char where
  toTransition = singleton

instance TransitionLabel String where
  toTransition = pack

instance TransitionLabel Text where
  toTransition = id

instance TransitionLabel Int where
  toTransition = pack . show

instance TransitionLabel Float where
  toTransition = pack . show

instance TransitionLabel Double where
  toTransition = pack . show

data StackTransition a b where
  StackT :: (Label a, Label b) => a -> (b, b) -> StackTransition a b

instance TransitionLabel (StackTransition a b) where
  toTransition (StackT token (stack1, stack2)) = drawLabel token <> ", " <> drawLabel stack1 <> "->" <> drawLabel stack2


data PositionConstraint s = Ab (State s) (State s)
                          | Le (State s) (State s)
                          deriving (Show, Eq)

constrained :: State s -> PositionConstraint s -> Bool
constrained s (Ab a b) = s == a || s == b
constrained s (Le a b) = s == a || s == b

without :: PositionConstraint s -> State s -> State s
(Ab a b) `without` s = bool a b (s == a)
(Le a b) `without` s = bool a b (s == a)

conToPair :: PositionConstraint s -> (State s, State s)
conToPair (Ab a b) = (a, b)
conToPair (Le a b) = (a, b)

data AutomatonRender = TextData Text | BinaryData Text


data PositionedState = PS {
  psid :: Int,
  sLabel :: Text,
  x :: Double,
  y :: Double,
  isInitial :: Bool,
  isFinal :: Bool
} deriving (Eq, Show)

data PositionedTransition = PT {
  tLabel :: Text,
  startX :: Double,
  startY :: Double,
  endX :: Double,
  endY :: Double,
  midX :: Double,
  midY :: Double,
  labelX :: Double,
  labelY :: Double
} deriving (Eq, Show)

data AutomatonLayout s t = AL {
  positionedStates :: [[PositionedState]],
  positionedTransitions :: [Transition t]
} deriving (Show)

data AutomatonLayoutAnimation s t = ALA {
  frames :: [[[PositionedState]]],
  transitionsStatic :: [Transition t]
} deriving (Show)
