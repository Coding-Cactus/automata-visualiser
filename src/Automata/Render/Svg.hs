{-# LANGUAGE OverloadedStrings #-}

module Automata.Render.Svg (svg, svgAnimation) where

import Automata.Render.Svg.Types
import Automata.Types

import Control.Monad (foldM)
import Data.Bool
import Data.Either (isRight)
import Data.List (elemIndex, partition, sort, sortBy)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T

import Data.Map qualified as M

import Data.Bifunctor (second)
import Data.Ord (Down (Down), comparing)
import Image.LaTeX.Render (FormulaOptions (environment), defaultEnv, defaultFormulaOptions, displaymath, imageForFormula)

stateGap :: Double
transLabelGap :: Double
startArrowLen :: Double
curvedEdgeGap :: Double
stateGap = 30
transLabelGap = 10
startArrowLen = 25
curvedEdgeGap = pi / 12

stateRadius :: Double -> Double
acceptStateRadius :: Double -> Double
positionScale :: Double -> Double
loopSeparationAngle :: Double -> Double
loopRadius :: Double -> Double
positionScale x = stateGap + stateRadius x * 2
stateRadius x = x * 25
acceptStateRadius x = stateRadius x + 3
loopRadius x = x * 20
loopSeparationAngle x = x * pi / 3

svgAnimation :: AutomatonConfig -> AutomatonLayoutAnimation s t -> AutomatonRender
svgAnimation config (ALA frames ts) = do
  let textTs = map (\(T i u v l) -> SvgT i u v (Text 0 0 $ T.intercalate "," $ map toTransition l)) ts
  pure $ renderAnimation $ map ((\t s -> buildSvg config s (map (Text 0 0 . sLabel) s) t) textTs . concat) frames

svg :: AutomatonConfig -> AutomatonLayout s t -> AutomatonRender
svg config (AL groups ts) = do
  -- check if latex is available
  testImg <- imageForFormula defaultEnv displaymath "x"
  let latexAvailable = isRight testImg

  if latexAvailable then pure () else putStrLn "Warning: LaTeX installation not found. Defaulting to text labels."

  -- convert TransitionLabel to SVG
  let textLs = map (\(T _ _ _ l) -> bool joinLabels joinLatexLabels latexAvailable l) ts
  let latexTransitions ls = zipWith (\(T i u v _) l -> SvgT i u v l) ts <$> renderLatexLabels ls
  let textTransitions = zipWith (\(T i u v _) l -> SvgT i u v (Text 0 0 l)) ts
  renderedTs <- bool (pure . textTransitions) latexTransitions latexAvailable textLs

  -- convert state labels to svgs
  let states = map scale $ concat groups
  stateLabels <- bool (pure . map (Text 0 0)) renderLatexLabels latexAvailable (map sLabel $ concat groups)

  -- build the svg
  let builtSvg = buildSvg config states stateLabels renderedTs

  -- output to Text
  pure $ render builtSvg
 where
  scale s = s{x = positionScale (svgLoopRadius config) * (x s - minX), y = positionScale (svgLoopRadius config) * (y s - minY)}
  minX = minimum $ map x $ concat groups
  minY = minimum $ map y $ concat groups

buildSvg :: AutomatonConfig -> [PositionedState] -> [SVG Double] -> [SvgTransition] -> SVG Double
buildSvg config sts stsLabels ts =
  Svg $
    concatMap (drawState config) statesAndAvailableSpaces
      <> concatMap (drawStraightTransition config) transitionPositions
      <> concatMap (drawLoopTransition config) loopTransitions
 where
  (transitionPositions, selfLoopInfo) = calculateTransitions
  loopTransitions = concatMap (\(s, loops, _) -> map (s,) loops) selfLoopAngles

  calculateTransitions = (positionedStraights, loopInfo)
   where
    positionedStraights = map position straight
    (loops, straight) = partition (\(SvgT _ a b _) -> a == b) ts

    position t@(SvgT i a b l) = PT i l x1 y1 x2 y2 x3 y3 x4 y4
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
      x4 = midX - (midGap / 2 + labelDir * (transLabelGap + labelWidth / 2)) * sin (edgeAngle aPos t)
      y4 = midY + (midGap / 2 + labelDir * (transLabelGap + labelHeight / 2)) * cos (edgeAngle aPos t)
      labelDir = bool (-1) 1 (midGap > 0)
      (labelHeight, labelWidth) = let (BB a1 a2 b1 b2) = boundingBox l in (b2 - b1, a2 - a1)

      curveAngle
        | even commonEdgeCount = (-1.0) ^ n * curvedEdgeGap * fromIntegral (1 + n `div` 2)
        | otherwise = (-1.0) ^ n * curvedEdgeGap * fromIntegral ((n + 1) `div` 2)
      n = fromMaybe 0 (elemIndex t commonEdges)
      direction = bool (-1) 1 (x aPos < x bPos) -- sync rotation direction between opposite direction arrows
      commonEdgeCount = length commonEdges
      commonEdges = filter (\(SvgT _ u v _) -> (a == u && b == v) || (b == u && a == v)) straight

      aPos = head $ filter ((==) a . psid) sts
      bPos = head $ filter ((==) b . psid) sts
      aRadius = bool stateRadius acceptStateRadius (isFinal aPos && acceptanceStyle config == DoubleCircle) (svgStateRadius config)
      bRadius = bool stateRadius acceptStateRadius (isFinal bPos && acceptanceStyle config == DoubleCircle) (svgStateRadius config)

    loopInfo = map (\s -> (s, (edgeAngles s, loopList s))) sts
     where
      edgeAngles u = sort $ map (uncurry $ positionedEdgeAngle u) positionedEdges
       where
        positionedEdges = map (\(i, direction) -> (head $ filter (\t -> ptid t == i) positionedStraights, direction)) edges
        edges = map (\(SvgT i a _ _) -> (i, psid u == a)) $ filter (\(SvgT _ a b _) -> psid u == a || psid u == b) straight
      loopList u = map (\(SvgT _ _ _ l) -> l) $ filter (\(SvgT _ a _ _) -> a == psid u) loops

    edgeAngle u (SvgT _ a b _) = angleBetween (x u) (y u) (x v) (y v)
     where
      v = head $ filter (\(PS{psid = i}) -> psid u /= i && (i == a || i == b)) sts

    positionedEdgeAngle (PS{x, y}) (PT{startX = x1, startY = y1, endX = x2, endY = y2}) direction = angleBetween x y (bool x2 x1 direction) (bool y2 y1 direction)

    angleBetween x1 y1 x2 y2 = atan (dy / dx) + if dx < 0 || dy < 0 then pi + if dx >= 0 && dy <= 0 then pi else 0 else 0
     where
      (dx, dy) = (x2 - x1, y2 - y1)

  selfLoopAngles = map calculateSelfLoops selfLoopInfo
  calculateSelfLoops (s, (edgeAngles, labels)) = (s, angles (zipWith (\a b -> ((a, b), [])) (last edgeAngles - 2 * pi : edgeAngles) edgeAngles) labels, edgeAngles)
   where
    angles zones [] = concatMap (\((a, b), ls) -> zipWith (\l i -> (l, a + ((b - a) / fromIntegral (1 + length ls)) * i)) ls [1 ..]) zones
    angles zones (l : ls) = angles ((fst $ head sorted, l : snd (head sorted)) : tail sorted) ls
     where
      sorted = sortBy (\a b -> compare (calc b) (calc a)) zones
      calc ((lo, hi), lps) = let n = fromIntegral $ length lps in (hi - lo + n * loopSeparationAngle (svgLoopSepAngle config)) / (n + 1)

  statesAndAvailableSpaces = map calculateAvailable selfLoopAngles
   where
    calculateAvailable (s, loopAngles, edgeAngles) = (s, getLabel s, invert $ mergeLoops edgeAngles $ map snd loopAngles)
    mergeLoops eAngles lAngles = mergeRanges [] $ sort (map (\a -> (a, a)) eAngles ++ map (\x -> (x - loopSep / 2, x + loopSep / 2)) lAngles)
     where
      mergeRanges xs [] = reverse xs
      mergeRanges [] (y : ys) = mergeRanges [y] ys
      mergeRanges ((a, b) : xs) ((x, y) : ys)
        | x < b = mergeRanges ((a, y) : xs) ys
        | otherwise = mergeRanges ((x, y) : (a, b) : xs) ys
      loopSep = loopSeparationAngle (svgLoopSepAngle config)
    invert ranges = pairUp $ (last boundaries - 2 * pi) : init boundaries -- convert list of forbidden angles into list of available angles
     where
      boundaries = concatMap (\(a, b) -> [a, b]) ranges
      pairUp [] = []
      pairUp [_] = []
      pairUp [a, b] = [(a, b)]
      pairUp (a : b : cs) = (a, b) : pairUp cs

  getLabel s = stsLabels !! fromMaybe 0 (elemIndex (psid s) (map psid sts))

drawState :: AutomatonConfig -> (PositionedState, SVG Double, [(Double, Double)]) -> [SVG Double]
drawState config (PS _ _ xPos yPos isS isF, label, angles) =
  [ Circle xPos yPos (stateRadius (svgStateRadius config))
  , setLabelPosition xPos yPos label
  ]
    <> outerCircle
    <> startArrow
    <> acceptArrow
 where
  -- create an outer circle if acceptance state and enabled in condfig
  outerCircle = if isDoubleCircle then [Circle xPos yPos (acceptStateRadius (svgLoopRadius config))] else mempty
  outerCircleRadius = bool stateRadius acceptStateRadius isDoubleCircle (svgStateRadius config)
  isDoubleCircle = isF && acceptanceStyle config == DoubleCircle

  -- create a start arrow if initial state
  startArrow = if isS then [makeStart] else mempty
  makeStart =
    Line
      (xPos + (outerCircleRadius + startArrowLen) * cos startAngle)
      (yPos + (outerCircleRadius + startArrowLen) * sin startAngle)
      (xPos + outerCircleRadius * cos startAngle)
      (yPos + outerCircleRadius * sin startAngle)

  -- create an acceptance arrow if acceptance state and enabled in config
  isAcceptArrow = isF && acceptanceStyle config == Arrow
  acceptArrow = if isAcceptArrow then [makeAccept] else mempty
  makeAccept =
    Line
      (xPos + outerCircleRadius * cos acceptAngle)
      (yPos + outerCircleRadius * sin acceptAngle)
      (xPos + (outerCircleRadius + startArrowLen) * cos acceptAngle)
      (yPos + (outerCircleRadius + startArrowLen) * sin acceptAngle)

  -- determine angles of start and accept arrows
  (startAngle, acceptAngle)
    | isS && not isAcceptArrow = (singleAngle, undefined)
    | not isS && isAcceptArrow = (undefined, singleAngle)
    | otherwise = (angle1, angle2)
   where
    singleAngle = snd largestGap
    (angle1, angle2)
      | fst secondLargestGap / 2 > fst largestGap / 3 = (snd largestGap, snd secondLargestGap)
      | otherwise = (snd largestGap - fst largestGap / 6, snd largestGap + fst largestGap / 6)
    secondLargestGap = bool (0, 0) (head $ tail gaps) (length gaps >= 2)
    largestGap = head gaps
    gaps = sortBy (comparing Down) $ map (\(a, b) -> (b - a, (a + b) / 2)) angles

drawStraightTransition :: AutomatonConfig -> PositionedTransition -> [SVG Double]
drawStraightTransition _ (PT _ label x1 y1 x2 y2 x3 y3 x4 y4) =
  [ Curve x1 y1 x2 y2 x3 y3
  , setLabelPosition x4 y4 label
  ]

drawLoopTransition :: AutomatonConfig -> (PositionedState, (SVG Double, Double)) -> [SVG Double]
drawLoopTransition config (state, (label, orientation)) =
  [ Arc x1 y1 x2 y2 (loopRadius (svgLoopRadius config)) True True
  , setLabelPosition labelX labelY label
  ]
 where
  -- arc origin
  x1 = x state + sRadius * cos (orientation - separationAngle / 2)
  y1 = y state + sRadius * sin (orientation - separationAngle / 2)
  -- arc end
  x2 = x state + sRadius * cos (orientation - separationAngle / 2 + separationAngle)
  y2 = y state + sRadius * sin (orientation - separationAngle / 2 + separationAngle)
  -- label positioning: find loop centre and offset by radius+gap
  loopAngle = acos $ 1 - (sRadius ** 2 * (1 - cos separationAngle)) / lRadius ** 2
  centreOffsetAngle = orientation + loopAngle / 2 - pi / 2
  centreX = x1 - lRadius * sin centreOffsetAngle
  centreY = y1 + lRadius * cos centreOffsetAngle
  labelX = centreX + (lRadius + transLabelGap + labelWidth / 2) * cos orientation
  labelY = centreY + (lRadius + transLabelGap + labelHeight / 2) * sin orientation
  (labelHeight, labelWidth) = let (BB a1 a2 b1 b2) = boundingBox label in (b2 - b1, a2 - a1)
  -- gather state information
  sRadius = (if isFinal state && acceptanceStyle config == DoubleCircle then acceptStateRadius else stateRadius) (svgStateRadius config)
  lRadius = loopRadius (svgLoopRadius config)
  separationAngle = loopSeparationAngle (svgLoopSepAngle config)

renderLatexLabels :: [T.Text] -> IO [SVG Double]
renderLatexLabels labels = reverse . snd <$> foldM (\(c, ts) t -> second (: ts) <$> renderLabel c t) (M.empty, []) labels
 where
  renderLabel cache label = do
    renderAttempt <-
      if label `M.member` cache
        then
          pure $ Right $ cache M.! label
        else do
          img <- imageForFormula defaultEnv (defaultFormulaOptions{environment = Just "align*"}) $ T.unpack label
          pure $ Latex . T.unlines . drop 2 . T.lines . T.pack <$> img -- remove document declaration lines
    pure $ case renderAttempt of
      Left _ -> (cache, Text 0 0 label)
      Right img -> (M.insert label img cache, Wrapper 0 0 img)
