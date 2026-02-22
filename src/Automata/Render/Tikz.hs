module Automata.Render.Tikz where

import Automata.Render.Tikz.Types
import Automata.Types
import Data.List (elemIndex, sort, sortBy)
import Data.Maybe (fromMaybe)
import Data.Bool

tikzCoordinateScale :: Double
tikzLoopWidth :: Double
tikzCoordinateScale = 1.5
tikzLoopWidth = 30

tikz :: AutomatonLayout s t -> AutomatonRender
tikz a = pure $ render (TD nodes transitions)
  where
    nodes = map posStateToTikzNode (concat $ positionedStates a)
    transitions = map transToTikzTrans (positionedTransitions a)

    posStateToTikzNode (PS i label x y isInit isFinal) = N i label (tikzCoordinateScale * x) (-(tikzCoordinateScale * y)) isInit isFinal
    transToTikzTrans t@(T _ u v labels)
      | u == v = Loop u (map toTransition labels) (loopAngle - tikzLoopWidth/2) (loopAngle + tikzLoopWidth/2)
      | otherwise = Straight u v edgeAngle (map toTransition labels) edgeStyle
      where
        -- for edges, determine whether they should be bent
        edgeStyle
          | length sharedEdges == 1 = NoBend
          | edgeN >= 2 = NoBend -- not supported yet
          | (u < v) /= even edgeN = LeftBend
          | otherwise = RightBend
        edgeN = fromMaybe 0 $ elemIndex t sharedEdges
        sharedEdges = filter (\(T _ x y _) -> (x == u && y == v) || (x == v && y == u)) (positionedTransitions a)
        edgeAngle = snd $ head $ filter (\(x, _) -> x == v) edgeAngles


        -- for loops, determine their position on the state's perimeter
        loopAngle = head $ map snd $ filter (\(l, _) -> l == t) findLoopAngles

        -- calculate angles of non-loop edges
        edgeAngles = map (\x -> (x, calculateAngle (getNode u) $ getNode x)) connectedNodes
        sortedAngles = sort $ map snd edgeAngles
        connectedNodes = map (\(T _ x y _) -> bool x y (u == x)) $ filter (\(T _ x y _) -> (x == u || y == u) && x /= y) (positionedTransitions a)
        calculateAngle (PS { x=x1, y=y1 }) (PS { x=x2, y=y2 }) = let (dx, dy) = (x2-x1, y1-y2) in -- (y1-y2) rather than the other way around because up=+ive in tikz
          radToDeg (atan (dy/dx)) + if dx < 0 || dy < 0 then 180 + if dx >= 0 && dy <= 0 then 180 else 0 else 0
        radToDeg theta = theta / pi * 180
        getNode i = head $ concatMap (filter (\x -> psid x == i)) (positionedStates a)

        -- find best empty spaces to put loops
        commonLoops = filter (\(T _ x y _) -> x == u && y == u) (positionedTransitions a)
        findLoopAngles = angles (zipWith (\x y -> ((x, y), [])) (last sortedAngles - 360 : sortedAngles) sortedAngles) commonLoops
          where
            angles zones [] = concatMap (\((x, y), ls) -> zipWith (\l i -> (l, x + ((y - x) / fromIntegral (1 + length ls)) * i)) ls [1..]) zones
            angles zones (l:ls) = angles ((fst $ head sorted, l : snd (head sorted)) : tail sorted) ls
              where
                sorted = sortBy (\x y -> compare (calc y) (calc x)) zones
                calc ((lo, hi), lps) = let n = fromIntegral $ length lps in (hi-lo + n*tikzLoopWidth) / (n+1)
