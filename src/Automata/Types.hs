{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Automata.Types  where

import Data.Text (Text, unpack, pack, singleton)
import qualified Control.Monad.State as S

data Automaton s t = Automaton {
  states :: [State s],
  transitions :: [Transition s t],
  initialS :: Int,
  finalS :: [Int]
} deriving Show


type AutomatonBuilder s t = S.State (Automaton s t) ()


data State s where
  S :: Label s => Int -> s -> State s

instance Show (State s) where
  show (S _ name) = unpack $ drawLabel name


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


data Transition s t where
  T :: TransitionLabel t => State s -> State s -> t -> Transition s t

instance Show (Transition s t) where
  show (T s1 s2 t) = show (s1, s2, toTransition t)



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


data AutomatonRender = TextData Text | BinaryData Text


data PositionedState = PS {
  psid :: Int,
  sLabel :: Text,
  x :: Double,
  y :: Double,
  isInitial :: Bool,
  isFinal :: Bool
}

data PositionedTranistion = PT {
  tLabel :: Text,
  startX :: Double,
  startY :: Double,
  endX :: Double,
  endY :: Double
}

data AutomatonLayout s t = AL {
  positionedStates :: [PositionedState],
  positionedTransitions :: [Transition s t]
}
