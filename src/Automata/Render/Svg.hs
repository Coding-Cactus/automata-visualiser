{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Automata.Render.Svg (svg) where

import Automata.Types

import Data.Foldable (foldl')
import qualified Data.Text as T

data BoundingBox a where
  BB :: (Show a, Num a, Ord a) => {
    xmin :: a,
    xmax :: a,
    ymin :: a,
    ymax :: a
  } -> BoundingBox a

data SVG a where
  Svg    :: (Show a, Num a, Ord a) => [SVG a] -> SVG a
  Text   :: (Show a, Num a, Ord a) => a -> a -> T.Text -> SVG a -- Text x y content
  Circle :: (Show a, Num a, Ord a) => a -> a -> a -> SVG a      -- Circle x y radius
  Line   :: (Show a, Num a, Ord a) => a -> a -> a -> a -> SVG a -- Line startX startY endX endY

render :: SVG a -> T.Text
render (Svg elems) = let (BB minX maxX minY maxY) = boundingBox $ Svg elems in
  "<svg "
  <> "viewBox=\"" <> T.intercalate " " (map (T.pack . show) [minX, minY, maxX - minX, maxY - minY])
  <> "\" xmlns=\"http://www.w3.org/2000/svg\">"
  <> arrowMarkerDef
  <> T.concat (map render elems)
  <> "</svg>"
render (Text x y txt) = let (x', y') = mapPair (T.pack . show) (x, y) in
  "<text "
  <> "text-anchor=\"middle\" dominant-baseline=\"middle\" "
  <> "x=\"" <> x' <> "\" y=\"" <> y'
  <> "\">" <> txt <> "</text>"
render (Circle x y r) = let (x', y', r') = mapTriple (T.pack . show) (x, y, r) in
  "<circle "
  <> "cx=\"" <> x' <> "\" cy=\"" <> y' <> "\" r=\"" <> r'
  <> "\" stroke=\"black\" fill=\"none\" />"
render (Line x1 y1 x2 y2) = let (x1', y1', x2', y2') = mapQuadruple (T.pack . show) (x1, y1, x2, y2) in
  "<line "
  <> "x1=\"" <> x1' <> "\" x2=\"" <> x2' <> "\" y1=\"" <> y1' <> "\" y2=\"" <> y2'
  <> "\" stroke=\"black\" marker-end=\"url(#arrow)\" />"

arrowMarkerDef :: T.Text
arrowMarkerDef = -- https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/marker
  "<defs>"
  <> "<marker id=\"arrow\" viewBox=\"0 0 10 10\" "
  <> "refX=\"10\" refY=\"5\" markerWidth=\"6\" markerHeight=\"6\" "
  <> "orient=\"auto-start-reverse\">"
  <> "<path d=\"M 0 0 L 10 5 L 0 10 z\" />"
  <> "</marker>"
  <> "</defs>"

boundingBox :: SVG a -> BoundingBox a
boundingBox (Svg []) = BB 0 0 0 0
boundingBox (Svg elems) = foldl' findBox initBox $ map boundingBox (tail elems)
  where
    findBox (BB minX maxX minY maxY) (BB minX' maxX' minY' maxY') = BB {
      xmin = min minX minX',
      xmax = max maxX maxX',
      ymin = min minY minY',
      ymax = max maxY maxY'
    }
    initBox = boundingBox $ head elems
boundingBox (Text x y _) = BB x x y y
boundingBox (Circle x y r) = BB (x-r) (x+r) (y-r) (y+r)
boundingBox (Line x1 y1 x2 y2) = BB (min x1 x2) (max x1 x2) (min y1 y2) (max y1 y2)



mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)
mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (x, y, z) = (f x, f y, f z)
mapQuadruple :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
mapQuadruple f (w, x, y, z) = (f w, f x, f y, f z)


svgStateGap :: Double
svgStateRadius :: Double
svgAcceptStateRadius :: Double
svgPositionScale :: Double
svgTransLabelGap :: Double
svgStateGap = 30
svgStateRadius = 25
svgAcceptStateRadius = svgStateRadius + 3
svgPositionScale = svgStateGap + svgStateRadius*2
svgTransLabelGap = 10

svg :: AutomatonLayout s t -> AutomatonRender
svg (AL sts ts) = TextData $ render $ Svg $ concatMap drawState sts <> concatMap drawTransition ts
  where
    drawState (PS _ name xPos yPos _ isF) = [Circle scaleX scaleY svgStateRadius,
                                            Text scaleX scaleY name]
                                            <> outerCircle
      where
        scaleX = svgPositionScale * xPos
        scaleY = svgPositionScale * yPos
        outerCircle = if isF then [Circle scaleX scaleY svgAcceptStateRadius] else mempty
    drawTransition (T (S aId _) (S bId _) label) = [Line x1' y1' x2' y2',
                                                    Text xMid yMid (toTransition label)]
      where
        x1 = svgPositionScale * x aState -- centre of the states
        y1 = svgPositionScale * y aState
        x2 = svgPositionScale * x bState
        y2 = svgPositionScale * y bState
        x1' = x1 + (aRadius / hypLength) * adjLength -- edge of the states
        y1' = y1 + (aRadius / hypLength) * opLength
        x2' = x2 - (bRadius / hypLength) * adjLength
        y2' = y2 - (bRadius / hypLength) * opLength
        xMid = (x1 + x2) / 2 + midFactor * gradient
        yMid = (y1 + y2) / 2 - midFactor
        midFactor = svgTransLabelGap / (-sqrt (1 + gradient**2))
        aState = head $ filter ((==) aId . psid) sts
        bState = head $ filter ((==) bId . psid) sts
        aRadius = if isFinal aState then svgAcceptStateRadius else svgStateRadius
        bRadius = if isFinal bState then svgAcceptStateRadius else svgStateRadius
        opLength = y2 - y1
        adjLength = x2 - x1
        hypLength = sqrt ((x1 - x2)**2 + (y1 - y2)**2)
        gradient = (y2 - y1) / (x2 - x1)
