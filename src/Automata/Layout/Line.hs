module Automata.Layout.Line (layout) where

import Automata.Types
import Data.List (singleton)


layout :: Automaton s t -> AutomatonLayout s t
layout a = AL {
    positionedStates = map (singleton . positionState) $ states a,
    positionedTransitions = []
  }
  where
    positionState (S sid name) = PS {
      psid = sid,
      sLabel = drawLabel name,
      x = fromIntegral sid,
      y = 0,
      isInitial = sid == initialS a,
      isFinal = sid `elem` finalS a
    }
