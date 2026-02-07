module Automata.Layout.Combined (layout, layout') where

import Automata.Types

import qualified Automata.Layout.Force as F
import qualified Automata.Layout.Energy as E
import qualified Automata.Layout.Constrained as C

import Data.Bool (bool)

layout' :: Automaton s t -> AutomatonLayoutAnimation s t
layout' a = ALA {
    frames = map positionedStates [frame1, frame2, frame3],
    transitionsStatic = transitions a
  }
  where
    frame1 = C.layout F.defaultLength a
    frame2 = E.layoutPositioned frame1
    frame3 = F.layoutPositioned frame2

layout :: Automaton s t -> AutomatonLayout s t
layout a = combinedLayout
  where
    -- put nodes into groups based on their constraints
    initial = C.layout F.defaultLength a

    -- layout each connected component separetely
    layouts = map ((F.layoutPositioned . E.layoutPositioned) . (\c -> AL c (gatherTransitions c) )) components
    gatherTransitions comp = filter (\(T _ x _ _) -> x `elem` map psid (concat comp)) (transitions a)

    -- recombine components after laying out separately
    combinedLayout = AL (concat $ zipWith translateComp [1..] comps) (transitions a)
      where
        translateComp i = map (map (\u -> u { x = x u + fst (snd (ends !! i)), y = y u + snd (snd (ends !! i)) }))

        comps = map positionedStates layouts
        headMinY = minimum (map y (concat $ head comps))
        headMaxY = maximum (map y (concat $ head comps))
        yMid = (headMinY + headMaxY) / 2
        ends = (0, (0, 0)) : [
            (endX, (transX, transY)) |
              (groups, i) <- zip comps [0..],
              let minX = minimum $ map x $ concat groups,
              let maxX = maximum $ map x $ concat groups,
              let minY = minimum $ map y $ concat groups,
              let maxY = maximum $ map y $ concat groups,
              let transY = yMid - (minY + maxY) / 2,
              let startX = fst (ends !! i),
              let transX = startX - minX,
              let endX = startX + (maxX - minX) + F.defaultLength
          ]

    -- split groups into connected components
    -- nodes also count as connected if they have a positioning constraint
    components = map reverse $ getComponents [] (positionedStates initial)
    getComponents comps [] = comps
    getComponents comps (g:gs) = getComponents newComps gs
      where
        newComps = bool addToComps ([g] : comps) (addToComps == comps)
        addToComps = map (\c -> bool c (g:c) (connected c g)) comps
        connected comp group = not $ null $ [
            (x, y) |
              (T _ x y _) <- transitions a,
              u <- group,
              vs <- comp, v <- vs,
              (x == psid u && y == psid v) || (x == psid v && y == psid u)
          ]
