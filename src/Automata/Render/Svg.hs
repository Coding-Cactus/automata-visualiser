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

render :: SVG a -> T.Text
render (Svg elems) = let (BB minX maxX minY maxY) = boundingBox $ Svg elems in
  "<svg "
  <> "viewBox=\"" <> T.intercalate " " (map (T.pack . show) [minX, minY, maxX - minX, maxY - minY])
  <> "\" xmlns=\"http://www.w3.org/2000/svg\">"
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



mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)
mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (x, y, z) = (f x, f y, f z)


svgStateGap :: Int
svgStateRadius :: Int
svgAcceptStateRadius :: Int
svgPositionScale :: Int
svgStateGap = 30
svgStateRadius = 25
svgAcceptStateRadius = svgStateRadius + 3
svgPositionScale = svgStateGap + svgStateRadius*2

svg :: AutomatonLayout -> AutomatonRender
svg (AL sts _) = TextData $ render $ Svg $ concatMap drawStates sts
  where
    drawStates (PS name xPos yPos _ isF) = [Circle scaleX scaleY svgStateRadius,
                                            Text scaleX scaleY name]
                                            <> outerCircle
      where
        scaleX = xPos * svgPositionScale
        scaleY = yPos * svgPositionScale
        outerCircle = if isF then [Circle scaleX scaleY svgAcceptStateRadius] else mempty
