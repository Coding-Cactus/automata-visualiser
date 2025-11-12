module Automata.Layout (layout) where

import Automata.Types


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
