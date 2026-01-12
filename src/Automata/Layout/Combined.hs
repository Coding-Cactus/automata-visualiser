module Automata.Layout.Combined (layout, layout') where

import Automata.Types

import qualified Automata.Layout.Force as F
import qualified Automata.Layout.Energy as E
import qualified Automata.Layout.ZigZag as Z

layout' :: Automaton s t -> AutomatonLayoutAnimation s t
layout' a = ALA {
    frames = map positionedStates [Z.layout a, firstFrame, F.layoutPositioned firstFrame],
    transitionsStatic = transitions a
  }
  where firstFrame = E.layout a

layout :: Automaton s t -> AutomatonLayout s t
layout = F.layoutPositioned . E.layout
