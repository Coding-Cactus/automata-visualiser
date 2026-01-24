module Automata.Layout.Constrained (layout) where

import Automata.Types

import System.Random
import Data.Bool
import Data.List (nub, foldl')

layout :: Double -> Automaton s t -> AutomatonLayout s t
layout defaultLength (Automaton states trs initial finals hints) = AL {
    -- group directionally related states and then random layout
    positionedStates = moveRandom (mkStdGen 4) [] $ map distributeNodes groups,
    positionedTransitions = trs
  }
  where
    -- group states into connected components by position constraint relation
    groups = group states []
    group [] gs = gs
    group (x:xs) gs = group xs' (g:gs) -- group stateQueue foundGroups
      where
        xs' = filter (`notElem` g) xs -- don't create new component if state already in this component (remove it from queue)
        g = gather [x] []
        -- recursively group all states connected to 'x' into the same group
        gather [] gr = gr
        gather (s:ss) gr = gather (filter (\s' -> s' `notElem` ss && s' `notElem` gr) (nub (map (`without` s) (filter (constrained s) hints))) ++ ss) (s:gr)

    -- distribute nodes within groups to satisfy position constraints
    distributeNodes grp = map fixedToPositioned $ distribute [ s | s <- grp, h <- hints, constrained s h && constrained (head grp) h && s /= head grp] [(head grp, (0, 0))]
      where
        fixedToPositioned (S sid label, (x, y)) = PS {
          psid = sid,
          sLabel = drawLabel label,
          x = x * defaultLength,
          y = y * defaultLength,
          isInitial = sid == initial,
          isFinal = sid `elem` finals
        }

    -- go though node queue, assigning positions based off the fixed position of previously distributed nodes
    distribute [] fixed = fixed
    distribute (n:ns) fixed = distribute ns' ((n, distribution):fixed)
      where
        -- add, to the queue, the unpositioned nodes connected to current node
        ns' = ns ++ filter (\m -> m /= n && m `notElem` fixedNodes && m `notElem` ns && not (null [h | h <- hints, constrained m h && constrained n h])) states
        distribution = foldl' sumPair (snd fixedConstraintNode) fixedConstraints

        fixedNodes = map fst fixed
        -- guaranteed to be exactly 1 fixed constraining node for `n`
        -- fixedConstraintNode = head (filter (`elem` fixedNodes) (map (`without` n) (filter (constrained n) hints)))
        fixedConstraintNode = head [ m | m <- fixed, h <- hints, constrained (fst m) h && constrained n h ]
        -- fixedConstraintNodePos = snd $ head $ filter (\m -> fst m == fixedConstraintNode) fixed
        fixedConstraints = [(dx, dy) |
                              h <- hints,
                              constrained n h && constrained (fst fixedConstraintNode) h,
                              let (dx, dy) = offset (fst fixedConstraintNode) h]
        offset m (Le a b) = bool (1, 0) (-1, 0) (Le a b == Le n m)
        offset m (Ab a b) = bool (0, 1) (0, -1) (Ab a b == Ab n m)

    moveRandom _ done [] = done
    moveRandom gen done (g:gs) = moveRandom gen3 (g' : done) gs
      where
        g' = map (\s -> s { x = dx + x s, y = dy + y s }) g
        (dx, gen2) = randomR (0, boundary) gen
        (dy, gen3) = randomR (0, boundary) gen2
        boundary = fromIntegral $ length states

sumPair :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumPair (a, b) (x, y) = (a+x, b+y)
