{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Automata.Render.Svg (svg, svgAnimation) where

import Automata.Types

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)
import Text.Blaze (toValue, text)

import Data.Foldable (foldl', forM_)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Bool

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
render = toStrict . renderSvg . toSvg


renderAnimation :: [SVG a] -> T.Text
renderAnimation = T.intercalate "\n" . map (T.intercalate "" . tail.tail.tail . T.lines . toStrict . renderSvg . toSvg)

toSvg :: SVG a -> S.Svg
toSvg (Svg elems) = let (BB minX maxX minY maxY) = boundingBox $ Svg elems in
  S.docTypeSvg ! A.version "1.1" ! A.viewbox (toValue (unwords $ map show [minX, minY, maxX - minX, maxY - minY])) $ do
    arrowMarkerDef
    forM_ elems toSvg
toSvg (Text x y txt) = let (x', y') = mapPair (toValue . show) (x, y) in
  S.text_ ! A.textAnchor "middle" ! A.dominantBaseline "middle"
    ! A.x x' ! A.y y' $
      text txt
toSvg (Circle x y r) = let (x', y', r') = mapTriple (toValue . show) (x, y, r) in
  S.circle ! A.stroke "black" ! A.fill "none"
    ! A.cx x' ! A.cy y' ! A.r r'
toSvg (Line x1 y1 x2 y2) = let (x1', y1', x2', y2') = mapQuadruple (toValue . show) (x1, y1, x2, y2) in
  S.line ! A.stroke "black" ! A.markerEnd "url(#arrow)"
    ! A.x1 x1' ! A.x2 x2' ! A.y1 y1' ! A.y2 y2'

arrowMarkerDef :: S.Svg
arrowMarkerDef = -- https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/marker
  S.defs $
    S.marker ! A.id_ "arrow"
      ! A.viewbox "0 0 10 10" ! A.refx "10" ! A.refy "5"
      ! A.markerwidth "6" ! A.markerheight "6"
      ! A.orient "auto-start-reverse" $
        S.path ! A.d "M 0 0 L 10 5 L 0 10 z"


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
svgStartArrowLen :: Double
svgStateGap = 30
svgStateRadius = 25
svgAcceptStateRadius = svgStateRadius + 3
svgPositionScale = svgStateGap + svgStateRadius*2
svgTransLabelGap = 10
svgStartArrowLen = 25

svgAnimation :: AutomatonLayoutAnimation s t -> AutomatonRender
svgAnimation (ALA frames ts) = TextData $ renderAnimation $ map ((`buildSvg` ts) . concat) frames

svg :: AutomatonLayout s t -> AutomatonRender
svg (AL groups ts) = TextData $ render $ buildSvg (concat groups) ts

buildSvg :: [PositionedState] -> [Transition s t] -> SVG Double
buildSvg sts ts = Svg $ concatMap drawState sts <> concatMap drawTransition ts
  where
    drawState (PS _ name xPos yPos isS isF) = [Circle scaleX scaleY svgStateRadius,
                                            Text scaleX scaleY name]
                                            <> outerCircle
                                            <> startArrow
      where
        scaleX = svgPositionScale * xPos
        scaleY = svgPositionScale * yPos
        outerCircleRadius = bool svgStateRadius svgAcceptStateRadius isF
        outerCircle = if isF then [Circle scaleX scaleY svgAcceptStateRadius] else mempty
        startArrow = if isS then [Line (scaleX - outerCircleRadius - svgStartArrowLen) scaleY (scaleX - outerCircleRadius) scaleY] else mempty
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
        xMid = (x1 + x2) / 2 + if x2 - x1 == 0 then svgTransLabelGap else midFactor * gradient
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
