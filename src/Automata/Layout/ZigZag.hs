module Automata.Layout.ZigZag (layout) where

import Automata.Types


layout :: Automaton s t -> AutomatonLayout s t
layout a = AL {
    positionedStates = map positionState $ states a,
    positionedTransitions = []
  }
  where
    positionState (S sid name) = PS {
      psid = sid,
      sLabel = drawLabel name,
      x = fromIntegral sid,
      y = if even sid then 1 else -1,
      isInitial = sid == initialS a,
      isFinal = sid `elem` finalS a
    }
