{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib (a1, render, svg) where

import qualified Control.Monad.State as S
import Data.Text (Text, unpack, pack, singleton)
import qualified Data.Text.IO as T (writeFile)

import qualified Svg

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




data PositionedState = PS {
  sLabel :: Text,
  x :: Int,
  y :: Int,
  isInitial :: Bool,
  isFinal :: Bool
}

data PositionedTranistions = PT {
  tLabel :: Text,
  startX :: Int,
  startY :: Int,
  endX :: Int,
  endY :: Int
}

data AutomatonLayout = AL {
  positionedStates :: [PositionedState],
  positionedTransitions :: [PositionedTranistions]
}


layout :: Automaton s t -> AutomatonLayout
layout a = AL {
    positionedStates = map positionState $ states a,
    positionedTransitions = []
  }
  where
    positionState (S sid name) = PS {
      sLabel = drawLabel name,
      x = sid,
      y = 0,
      isInitial = sid == initialS a,
      isFinal = sid `elem` finalS a
    }


data AutomatonRender = TextData Text | BinaryData Text

svgStateGap :: Int
svgStateRadius :: Int
svgAcceptStateRadius :: Int
svgPositionScale :: Int
svgStateGap = 30
svgStateRadius = 25
svgAcceptStateRadius = svgStateRadius + 3
svgPositionScale = svgStateGap + svgStateRadius*2

svg :: AutomatonLayout -> AutomatonRender
svg (AL sts _) = TextData $ Svg.render $ Svg.Svg $ concatMap drawStates sts
  where
    drawStates (PS name xPos yPos _ isF) = [Svg.Circle scaleX scaleY svgStateRadius,
                                            Svg.Text scaleX scaleY name]
                                            <> outerCircle
      where
        scaleX = xPos * svgPositionScale
        scaleY = yPos * svgPositionScale
        outerCircle = if isF then [Svg.Circle scaleX scaleY svgAcceptStateRadius] else mempty

saveToFile :: String -> AutomatonRender -> IO ()
saveToFile fname = \case
  TextData txt -> T.writeFile fname txt
  BinaryData _ -> undefined

render :: String -> (AutomatonLayout -> AutomatonRender) -> AutomatonBuilder s t -> IO ()
render file fn a = saveToFile file $ fn $ layout $ S.execState a (Automaton [] [] (-1) [])

a1 = do
  a <- state 'a'
  a' <- state 'b'
  b <- state 'c'
  --_ <- state "c"
  --_ <- state "abc"
  --_ <- state "hi"

  initial a
  final a
  final b

  a >-|'a'|-> b
  --a >-|(0::Int) ~~ ('a', 'b')|-> b
  a >-|'b'|-> b
