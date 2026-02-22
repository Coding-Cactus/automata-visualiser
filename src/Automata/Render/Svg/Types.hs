{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Automata.Render.Svg.Types where

import Text.Blaze (preEscapedText, text, toValue)
import Text.Blaze.Svg.Renderer.Text (renderSvg)
import Text.Blaze.Svg11 (aa, m, mkPath, q, (!))
import Text.Blaze.Svg11 qualified as S
import Text.Blaze.Svg11.Attributes qualified as A

import Data.Foldable (foldl', forM_)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)

import Text.Regex.TDFA ((=~))

import Data.Bool

data TextTransition = TT Int Int Int T.Text

instance Eq TextTransition where
  (TT a _ _ _) == (TT b _ _ _) = a == b

data BoundingBox a where
  BB ::
    (Show a, Num a, Ord a) =>
    { xmin :: a
    , xmax :: a
    , ymin :: a
    , ymax :: a
    } ->
    BoundingBox a

data SVG a where
  Svg :: (Show a, Fractional a, Ord a) => [SVG a] -> SVG a
  Text :: (Show a, Fractional a, Ord a) => a -> a -> T.Text -> SVG a -- Text x y content
  Circle :: (Show a, Fractional a, Ord a) => a -> a -> a -> SVG a -- Circle x y radius
  Line :: (Show a, Fractional a, Ord a) => a -> a -> a -> a -> SVG a -- Line startX startY endX endY
  Curve :: (Show a, Fractional a, Ord a) => a -> a -> a -> a -> a -> a -> SVG a -- Curve startX startY endX endY curveX curveY
  Arc :: (Show a, Fractional a, Ord a) => a -> a -> a -> a -> a -> Bool -> Bool -> SVG a -- Arc x1 y1 x2 y2 radius largeArcFlag sweepFlag
  Wrapper :: (Show a, Fractional a, Ord a) => a -> a -> SVG a -> SVG a -- Wrapper x y innerContent
  Raw :: T.Text -> SVG Double

mapSvg :: (Show a, Fractional a, Ord a, Show b, Fractional b, Ord b) => (SVG a -> SVG b) -> SVG a -> SVG b
mapSvg f (Svg elems) = Svg $ map (mapSvg f) elems
mapSvg f s = f s

render :: SVG a -> T.Text
render = toStrict . renderSvg . toSvg

renderAnimation :: [SVG a] -> T.Text
renderAnimation = T.intercalate "\n" . map (T.intercalate "" . drop 3 . T.lines . toStrict . renderSvg . toSvg)

toSvg :: SVG a -> S.Svg
toSvg (Svg elems) =
  let (BB minX maxX minY maxY) = boundingBox $ Svg elems
   in S.docTypeSvg ! A.version "1.1" ! A.viewbox (toValue (unwords $ map show [minX, minY, maxX - minX, maxY - minY])) $ do
        arrowMarkerDef
        forM_ elems toSvg
toSvg (Text x y txt) =
  let (x', y') = mapPair (toValue . show) (x, y)
   in S.text_
        ! A.textAnchor "middle"
        ! A.dominantBaseline "middle"
        ! A.x x'
        ! A.y y'
        $ text txt
toSvg (Circle x y r) =
  let (x', y', r') = mapTriple (toValue . show) (x, y, r)
   in S.circle
        ! A.stroke "black"
        ! A.fill "none"
        ! A.cx x'
        ! A.cy y'
        ! A.r r'
toSvg (Line x1 y1 x2 y2) =
  let (x1', y1', x2', y2') = mapQuadruple (toValue . show) (x1, y1, x2, y2)
   in S.line
        ! A.stroke "black"
        ! A.markerEnd "url(#arrow)"
        ! A.x1 x1'
        ! A.x2 x2'
        ! A.y1 y1'
        ! A.y2 y2'
toSvg (Curve x1 y1 x2 y2 x3 y3) = S.path ! A.fill "none" ! A.stroke "black" ! A.markerEnd "url(#arrow)" ! A.d path
 where
  path = mkPath $ do
    m x1 y1
    q x3 y3 x2 y2
toSvg (Arc x1 y1 x2 y2 radius largeArc sweep) = S.path ! A.d path ! A.fill "none" ! A.stroke "black" ! A.markerEnd "url(#arrow)"
 where
  path = mkPath $ do
    m x1 y1
    aa radius radius 0 largeArc sweep x2 y2
toSvg (Wrapper x y inner) =
  S.svg
    ! A.x (toValue $ show $ x - w / 2)
    ! A.y (toValue $ show $ y - h / 2)
    ! A.height (toValue $ show h)
    ! A.width (toValue $ show w)
    ! A.viewbox (toValue (unwords $ map show [minX, minY, maxX - minX, maxY - minY]))
    $ toSvg inner
 where
  (BB minX maxX minY maxY) = boundingBox inner
  (h, w) = (maxY - minY, maxX - minX)
toSvg (Raw txt) = preEscapedText txt

arrowMarkerDef :: S.Svg
arrowMarkerDef =
  -- https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/marker
  S.defs
    $ S.marker
      ! A.id_ "arrow"
      ! A.viewbox "0 0 10 10"
      ! A.refx "10"
      ! A.refy "5"
      ! A.markerwidth "6"
      ! A.markerheight "6"
      ! A.orient "auto-start-reverse"
    $ S.path ! A.d "M 0 0 L 10 5 L 0 10 z"

boundingBox :: SVG a -> BoundingBox a
boundingBox (Svg []) = BB 0 0 0 0
boundingBox (Svg elems) = foldl' findBox initBox $ map boundingBox (tail elems)
 where
  findBox (BB minX maxX minY maxY) (BB minX' maxX' minY' maxY') =
    BB
      { xmin = min minX minX'
      , xmax = max maxX maxX'
      , ymin = min minY minY'
      , ymax = max maxY maxY'
      }
  initBox = boundingBox $ head elems
boundingBox (Text x y t) = let l = fromIntegral $ T.length t in BB (x - 10 * l) (x + 10 * l) (y - 15) (y + 15)
boundingBox (Circle x y r) = BB (x - r) (x + r) (y - r) (y + r)
boundingBox (Line x1 y1 x2 y2) = BB (min x1 x2) (max x1 x2) (min y1 y2) (max y1 y2)
boundingBox (Curve x1 y1 x2 y2 x3 y3) = BB (minimum [x1, x2, x3]) (maximum [x1, x2, x3]) (minimum [y1, y2, y3]) (maximum [y1, y2, y3])
boundingBox (Arc x1 y1 x2 y2 r _ _) = BB (min x1 x2 - (2 * r)) (max x1 x2 + (2 * r)) (min y1 y2 - (2 * r)) (max y1 y2 + (2 * r)) -- estimate, not actual bounding box
boundingBox (Wrapper x y inner) = let (BB x1 x2 y1 y2) = boundingBox inner in BB (x - (x2 - x1) / 2) (x + (x2 - x1) / 2) (y - (y2 - y1) / 2) (y + (y2 - y1) / 2)
boundingBox (Raw txt) = BB 0 width 0 height -- read height and width from svg text
 where
  width = bool (96 / 72 * read (T.unpack $ head widthMatches)) 0 (null widthMatches)
  height = bool (96 / 72 * read (T.unpack $ head heightMatches)) 0 (null heightMatches)
  widthResult = txt =~ rawSvgWidthRegex :: (T.Text, T.Text, T.Text, [T.Text])
  heightResult = txt =~ rawSvgHeightRegex :: (T.Text, T.Text, T.Text, [T.Text])
  (_, _, _, widthMatches) = widthResult
  (_, _, _, heightMatches) = heightResult

rawSvgWidthRegex :: T.Text
rawSvgWidthRegex = "\\`<[^>]*width=['\"]([0-9.]+)pt"
rawSvgHeightRegex :: T.Text
rawSvgHeightRegex = "\\`<[^>]*height=['\"]([0-9.]+)pt"

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)
mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (x, y, z) = (f x, f y, f z)
mapQuadruple :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
mapQuadruple f (w, x, y, z) = (f w, f x, f y, f z)
