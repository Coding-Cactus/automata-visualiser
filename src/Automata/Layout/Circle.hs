module Automata.Layout.Circle (layout) where

import Automata.Types
import Data.List (singleton)

-- Lay out the states in a circle
layout :: Automaton s t -> AutomatonLayout s t
layout a = AL {
    positionedStates = map singleton posStates,
    positionedTransitions = transitions a
  }
  where
    posStates = map positionState $ states a
    positionState (S sid name) = PS {
      psid = sid,
      sLabel = drawLabel name,
      x = r * sin angle,
      y = r * cos angle,
      isInitial = sid == initialS a,
      isFinal = sid `elem` finalS a
    }
      where angle = 2 * pi * fromIntegral sid / fromIntegral (length (states a))
    r = if length (states a) == 1 then 1
        else sqrt $ 2 / (1 - cos (2 * pi / fromIntegral (length $ states a))) -- radius of circle on which states will be placed
