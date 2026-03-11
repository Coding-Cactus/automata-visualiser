module Automata.Layout.Combined (layout, layout') where

import Automata.Types

import qualified Automata.Layout.Force as F
import qualified Automata.Layout.Energy as E
import qualified Automata.Layout.Constrained as C

import Data.Bool (bool)
import Data.List (nub, sortBy)

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
    components = map (sortBy (\g1 g2 -> compare (minimum $ map psid g1) (minimum $ map psid g2))) $ getComponents (positionedStates initial) []
    getComponents [] comps = comps
    getComponents (x:xs) comps = getComponents xs' (c:comps) -- getComponents groupQueue foundComponents
      where
        xs' = filter (`notElem` c) xs -- don't create new component if group already in this component (remove it from queue)
        c = gather [x] []
        -- recursively group all groups connected to 'x' into the same component
        gather [] comp = comp
        gather (g:gs) comp = gather (filter (\g' -> g' `notElem` gs && g' `notElem` comp) (nub (map (getGroup . notIn g) (filter (connected g) (positionedTransitions initial)))) ++ gs) (g:comp)
        connected g (T _ u v _) = let psids = map psid g in any (\s -> (s == u && v `notElem` psids) || (s == v && u `notElem` psids)) psids
        notIn g (T _ u v _) = bool u v (u `elem` map psid g) -- get the state which is not in the group, from the transition
        getGroup u = head $ filter (elem u . map psid) (positionedStates initial) -- get group from psid
