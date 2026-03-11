{-# LANGUAGE OverloadedStrings #-}

module Automata.Render.Svg (svg, svgAnimation) where

import Automata.Render.Svg.Types
import Automata.Types

import Control.Monad (foldM)
import Data.Bool
import Data.Either (fromRight, isRight)
import Data.List (elemIndex, partition, sort, sortBy)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T

import Data.Map (Map)
import Data.Map qualified as M

import Image.LaTeX.Render (RenderError, defaultEnv, displaymath, imageForFormula)

svgStateGap :: Double
svgStateRadius :: Double
svgAcceptStateRadius :: Double
svgPositionScale :: Double
svgTransLabelGap :: Double
svgStartArrowLen :: Double
loopSeparationAngle :: Double
loopRadius :: Double
curvedEdgeGap :: Double
svgStateGap = 30
svgStateRadius = 25
svgAcceptStateRadius = svgStateRadius + 3
svgPositionScale = svgStateGap + svgStateRadius * 2
svgTransLabelGap = 10
svgStartArrowLen = 25
loopSeparationAngle = pi / 3
loopRadius = 20
curvedEdgeGap = pi / 12

svgAnimation :: AutomatonLayoutAnimation s t -> AutomatonRender
svgAnimation (ALA frames ts) = do
  let textTs = map (\(T i u v l) -> TT i u v (T.intercalate "," $ map toTransition l)) ts
  pure $ renderAnimation $ map ((`buildSvg` textTs) . concat) frames

svg :: AutomatonLayout s t -> AutomatonRender
svg (AL groups ts) = do
  -- check if latex is available
  testImg <- imageForFormula defaultEnv displaymath "x"
  let latexAvailable = isRight testImg

  if latexAvailable then pure () else putStrLn "Warning: LaTeX installation not found. Defaulting to text labels."

  -- convert TransitionLabel to Text
  let textTs = map (\(T i u v l) -> TT i u v (T.intercalate "," $ map (bool toTransition toLatexTransition latexAvailable) l)) ts

  -- build the svg
  let builtSvg = buildSvg (map scale $ concat groups) textTs

  -- render latex labels if possible, then output to Text
  render <$> bool pure renderLatexLabels latexAvailable builtSvg
 where
  scale s = s{x = svgPositionScale * (x s - minX), y = svgPositionScale * (y s - minY)}
  minX = minimum $ map x $ concat groups
  minY = minimum $ map y $ concat groups

buildSvg :: [PositionedState] -> [TextTransition] -> SVG Double
buildSvg sts ts = Svg $ concatMap drawState statesAndAvailableSpaces <> concatMap drawStraightTransition transitionPositions <> concatMap drawLoopTransition loopTransitions
 where
  (transitionPositions, selfLoopInfo) = calculateTransitions
  loopTransitions = concatMap (\(s, loops, _) -> map (s,) loops) selfLoopAngles

  calculateTransitions = (map position straight, loopInfo)
   where
    (loops, straight) = partition (\(TT _ a b _) -> a == b) ts

    position t@(TT _ a b l) = PT l x1 y1 x2 y2 x3 y3 x4 y4
     where
      -- endpoints after rounding edge
      x1 = x aPos + aRadius * cos (edgeAngle aPos t + direction * curveAngle)
      y1 = y aPos + aRadius * sin (edgeAngle aPos t + direction * curveAngle)
      x2 = x bPos + bRadius * cos (edgeAngle bPos t - direction * curveAngle)
      y2 = y bPos + bRadius * sin (edgeAngle bPos t - direction * curveAngle)
      -- quadratic bezier curve turning point
      x3 = midX - midGap * sin (edgeAngle aPos t)
      y3 = midY + midGap * cos (edgeAngle aPos t)
      midX = (x aPos + x bPos) / 2
      midY = (y aPos + y bPos) / 2
      midGap = direction * 2.5 * (sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2) / 2) * tan curveAngle -- the scale factor (2.5) is for extra bendiness
      -- easier to calculate label position now
      x4 = midX - (midGap - labelDir * svgTransLabelGap) * sin (edgeAngle aPos t)
      y4 = midY + (midGap - labelDir * svgTransLabelGap) * cos (edgeAngle aPos t)
      labelDir = bool (-1) 1 (midGap > 0)

      curveAngle
        | even commonEdgeCount = (-1.0) ^ n * curvedEdgeGap * fromIntegral (1 + n `div` 2)
        | otherwise = (-1.0) ^ n * curvedEdgeGap * fromIntegral ((n + 1) `div` 2)
      n = fromMaybe 0 (elemIndex t commonEdges)
      direction = bool (-1) 1 (x aPos < x bPos) -- sync rotation direction between opposite direction arrows
      commonEdgeCount = length commonEdges
      commonEdges = filter (\(TT _ u v _) -> (a == u && b == v) || (b == u && a == v)) straight

      aPos = head $ filter ((==) a . psid) sts
      bPos = head $ filter ((==) b . psid) sts
      aRadius = bool svgStateRadius svgAcceptStateRadius (isFinal aPos)
      bRadius = bool svgStateRadius svgAcceptStateRadius (isFinal bPos)

    loopInfo = map (\s -> (s, (edgeAngles s, loopList s))) sts
     where
      edgeAngles u = sort $ map (edgeAngle u) $ filter (\(TT _ a b _) -> psid u == a || psid u == b) straight
      loopList u = map (\(TT _ _ _ l) -> l) $ filter (\(TT _ a _ _) -> a == psid u) loops

    edgeAngle u (TT _ a b _) = atan (dy / dx) + if dx < 0 || dy < 0 then pi + if dx >= 0 && dy <= 0 then pi else 0 else 0
     where
      v = head $ filter (\(PS{psid = i}) -> psid u /= i && (i == a || i == b)) sts
      (dx, dy) = (x v - x u, y v - y u)

  selfLoopAngles = map calculateSelfLoops selfLoopInfo
  calculateSelfLoops (s, (edgeAngles, labels)) = (s, angles (zipWith (\a b -> ((a, b), [])) (last edgeAngles - 2 * pi : edgeAngles) edgeAngles) labels, edgeAngles)
   where
    angles zones [] = concatMap (\((a, b), ls) -> zipWith (\l i -> (l, a + ((b - a) / fromIntegral (1 + length ls)) * i)) ls [1 ..]) zones
    angles zones (l : ls) = angles ((fst $ head sorted, l : snd (head sorted)) : tail sorted) ls
     where
      sorted = sortBy (\a b -> compare (calc b) (calc a)) zones
      calc ((lo, hi), lps) = let n = fromIntegral $ length lps in (hi - lo + n * loopSeparationAngle) / (n + 1)

  statesAndAvailableSpaces = map calculateAvailable selfLoopAngles
   where
    calculateAvailable (s, loopAngles, edgeAngles) = (s, invert $ mergeLoops edgeAngles $ map snd loopAngles)
    mergeLoops eAngles lAngles = mergeRanges [] $ sort (map (\a -> (a, a)) eAngles ++ map (\x -> (x - loopSeparationAngle / 2, x + loopSeparationAngle / 2)) lAngles)
     where
      mergeRanges xs [] = reverse xs
      mergeRanges [] (y : ys) = mergeRanges [y] ys
      mergeRanges ((a, b) : xs) ((x, y) : ys)
        | x < b = mergeRanges ((a, y) : xs) ys
        | otherwise = mergeRanges ((x, y) : (a, b) : xs) ys
    invert ranges = pairUp $ (last boundaries - 2 * pi) : init boundaries -- convert list of forbidden angles into list of available angles
     where
      boundaries = concatMap (\(a, b) -> [a, b]) ranges
      pairUp [] = []
      pairUp [_] = []
      pairUp [a, b] = [(a, b)]
      pairUp (a : b : cs) = (a, b) : pairUp cs

drawState :: (PositionedState, [(Double, Double)]) -> [SVG Double]
drawState (PS _ name xPos yPos isS isF, angles) =
  [ Circle xPos yPos svgStateRadius
  , Text xPos yPos name
  ]
    <> outerCircle
    <> startArrow
 where
  outerCircle = if isF then [Circle xPos yPos svgAcceptStateRadius] else mempty
  startArrow = if isS then [makeStart] else mempty
  makeStart =
    Line
      (xPos + (outerCircleRadius + svgStartArrowLen) * cos startAngle)
      (yPos + (outerCircleRadius + svgStartArrowLen) * sin startAngle)
      (xPos + outerCircleRadius * cos startAngle)
      (yPos + outerCircleRadius * sin startAngle)
  startAngle = snd $ maximum $ map (\(a, b) -> (b - a, (a + b) / 2)) angles
  outerCircleRadius = bool svgStateRadius svgAcceptStateRadius isF

drawStraightTransition :: PositionedTransition -> [SVG Double]
drawStraightTransition (PT label x1 y1 x2 y2 x3 y3 x4 y4) =
  [ Curve x1 y1 x2 y2 x3 y3
  , Text x4 y4 label
  ]

drawLoopTransition :: (PositionedState, (T.Text, Double)) -> [SVG Double]
drawLoopTransition (state, (label, orientation)) =
  [ Arc x1 y1 x2 y2 loopRadius True True
  , Text labelX labelY label
  ]
 where
  -- arc origin
  x1 = x state + stateRadius * cos (orientation - loopSeparationAngle / 2)
  y1 = y state + stateRadius * sin (orientation - loopSeparationAngle / 2)
  -- arc end
  x2 = x state + stateRadius * cos (orientation - loopSeparationAngle / 2 + loopSeparationAngle)
  y2 = y state + stateRadius * sin (orientation - loopSeparationAngle / 2 + loopSeparationAngle)
  -- label positioning: find loop centre and offset by radius+gap
  loopAngle = acos $ (loopRadius ** 2 - stateRadius ** 2 * (1 - cos loopSeparationAngle)) / loopRadius ** 2
  centreOffsetAngle = orientation + loopAngle / 2 - pi / 2
  centreX = x1 - loopRadius * sin centreOffsetAngle
  centreY = y1 + loopRadius * cos centreOffsetAngle
  labelX = centreX + (loopRadius + svgTransLabelGap) * cos orientation
  labelY = centreY + (loopRadius + svgTransLabelGap) * sin orientation
  -- gather state information
  stateRadius = if isFinal state then svgAcceptStateRadius else svgStateRadius

renderLatexLabels :: SVG Double -> IO (SVG Double)
renderLatexLabels s = snd <$> renderLabels M.empty s
 where
  renderLabels cache (Svg elems) = do
    (cache', elems') <- foldM (\(c, es) e -> fmap (\(c', e') -> (c', e' : es)) (renderLabels c e)) (cache, []) elems
    pure (cache', Svg elems')
  renderLabels cache (Text x y txt) = renderLabel cache x y txt
  renderLabels cache e = pure (cache, e)

  renderLabel cache x y label = do
    renderAttempt <-
      if label `M.member` cache
        then
          pure $ Right $ cache M.! label
        else do
          img <- imageForFormula defaultEnv displaymath $ T.unpack label
          pure $ Latex . T.unlines . drop 2 . T.lines . T.pack <$> img -- remove document declaration lines
    pure $ case renderAttempt of
      Left _ -> (cache, Text x y label)
      Right img -> (M.insert label img cache, Wrapper x y img)
