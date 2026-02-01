{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Automata.Render.Svg (svg, svgAnimation) where

import Automata.Types

import Text.Blaze.Svg11 ((!), mkPath, m, aa)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)
import Text.Blaze (toValue, text)

import Data.Foldable (foldl', forM_)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Bool
import Data.List (sort, partition, sortBy)
import Data.Maybe (fromMaybe)

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
  Arc    :: (Show a, Num a, Ord a) => a -> a -> a -> a -> a -> Bool -> Bool -> SVG a -- Arc x1 y1 x2 y2 radius largeArcFlag sweepFlag

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
toSvg (Arc x1 y1 x2 y2 radius largeArc sweep) = S.path ! A.d path ! A.fill "none" ! A.stroke "black" ! A.markerEnd "url(#arrow)"
    where
      path = mkPath $ do
        m x1 y1
        aa radius radius 0 largeArc sweep x2 y2

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
boundingBox (Text x y t) = let l = fromIntegral $ T.length t in BB (x - 10*l) (x + 10*l) (y-15) (y+15)
boundingBox (Circle x y r) = BB (x-r) (x+r) (y-r) (y+r)
boundingBox (Line x1 y1 x2 y2) = BB (min x1 x2) (max x1 x2) (min y1 y2) (max y1 y2)
boundingBox (Arc x1 y1 x2 y2 r _ _) = BB (min x1 x2 - (2*r)) (max x1 x2 + (2*r)) (min y1 y2 - (2*r)) (max y1 y2 + (2*r)) -- estimate, not actual bounding box



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
loopSeparationAngle :: Double
loopRadius :: Double
svgStateGap = 30
svgStateRadius = 25
svgAcceptStateRadius = svgStateRadius + 3
svgPositionScale = svgStateGap + svgStateRadius*2
svgTransLabelGap = 10
svgStartArrowLen = 25
loopSeparationAngle = pi / 3
loopRadius = 20

svgAnimation :: AutomatonLayoutAnimation s t -> AutomatonRender
svgAnimation (ALA frames ts) = TextData $ renderAnimation $ map ((`buildSvg` ts) . concat) frames

svg :: AutomatonLayout s t -> AutomatonRender
svg (AL groups ts) = TextData $ render $ buildSvg (map scale $ concat groups) ts
  where scale s = s { x = svgPositionScale * x s, y = svgPositionScale * y s }



buildSvg :: [PositionedState] -> [Transition t] -> SVG Double
buildSvg sts ts = Svg $ concatMap drawState statesAndAvailableSpaces <> concatMap drawStraightTransition transitionPositions <> concatMap drawLoopTransition loopTransitions
  where
    (transitionPositions, selfLoopInfo) = calculateTransitions
    loopTransitions = concatMap (\(s, loops, _) -> map (s,) loops) selfLoopAngles

    calculateTransitions = (map position straight, loopInfo)
      where
        (loops, straight) = partition (\(T a b _) -> a == b) ts

        position (T a b l) = PT (toTransition l) x1 y1 x2 y2
          where
            x1 = x aPos + (aRadius / hypLength) * adjLength
            y1 = y aPos + (aRadius / hypLength) * oppLength
            x2 = x bPos - (bRadius / hypLength) * adjLength
            y2 = y bPos - (bRadius / hypLength) * oppLength

            adjLength = x bPos - x aPos
            oppLength = y bPos - y aPos
            hypLength = sqrt $ adjLength**2 + oppLength**2

            aPos = head $ filter ((==) a . psid) sts
            bPos = head $ filter ((==) b . psid) sts
            aRadius = bool svgStateRadius svgAcceptStateRadius (isFinal aPos)
            bRadius = bool svgStateRadius svgAcceptStateRadius (isFinal bPos)

        loopInfo = map (\s -> (s, (edgeAngles s, loopList s))) sts
          where
            edgeAngles u = sort $ map (angle u) $ filter (\(T a b _) -> psid u == a || psid u == b) straight
            angle u (T a b _) = atan (dy/dx) + if dx < 0 || dy < 0 then pi + if dx > 0 && dy < 0 then pi else 0 else 0
              where
                v = head $ filter (\(PS {psid=i}) -> psid u /= i && (i == a || i == b)) sts
                (dx, dy) = (x v - x u, y v - y u)

            loopList u = map (\(T _ _ l) -> toTransition l) $ filter (\(T a _ _) -> a == psid u) loops

    selfLoopAngles = map calculateSelfLoops selfLoopInfo
    calculateSelfLoops (s, (edgeAngles, labels)) = (s, angles (zipWith (\a b -> ((a, b), [])) (last edgeAngles - 2*pi : edgeAngles) edgeAngles) labels, edgeAngles)
      where
        angles zones [] = concatMap (\((a, b), ls) -> zipWith (\l i -> (l, a + ((b - a) / fromIntegral (1 + length ls)) * i)) ls [1..]) zones
        angles zones (l:ls) = angles ((fst $ head sorted, l : snd (head sorted)) : tail sorted) ls
          where
            sorted = sortBy (\a b -> compare (calc b) (calc a)) zones
            calc ((lo, hi), lps) = let n = fromIntegral $ length lps in (hi-lo + n*loopSeparationAngle) / (n+1)

    statesAndAvailableSpaces = map calculateAvailable selfLoopAngles
      where
        calculateAvailable (s, loopAngles, edgeAngles) = (s, invert $ mergeLoops edgeAngles $ map snd loopAngles)
        mergeLoops eAngles lAngles = mergeRanges [] $ sort (map (\a -> (a, a)) eAngles ++ map (\x -> (x - loopSeparationAngle/2, x + loopSeparationAngle/2)) lAngles)
          where
            mergeRanges xs [] = reverse xs
            mergeRanges [] (y:ys) = mergeRanges [y] ys
            mergeRanges ((a, b):xs) ((x, y):ys)
              | x < b = mergeRanges ((a, y) : xs) ys
              | otherwise = mergeRanges ((x, y) : (a, b) : xs) ys
        invert ranges = pairUp $ (last boundaries - 2*pi) : init boundaries -- convert list of forbidden angles into list of available angles
          where
            boundaries = concatMap (\(a, b) -> [a, b]) ranges
            pairUp [] = []
            pairUp [_] = []
            pairUp [a, b] = [(a, b)]
            pairUp (a:b:cs) = (a, b) : pairUp cs


drawState :: (PositionedState, [(Double, Double)]) -> [SVG Double]
drawState (PS _ name xPos yPos isS isF, angles) = [Circle xPos yPos svgStateRadius,
                                                   Text xPos yPos name]
                                                   <> outerCircle
                                                   <> startArrow
  where
    outerCircle = if isF then [Circle xPos yPos svgAcceptStateRadius] else mempty
    startArrow = if isS then [makeStart] else mempty
    makeStart = Line (xPos + (outerCircleRadius + svgStartArrowLen) * cos startAngle)
                     (yPos + (outerCircleRadius + svgStartArrowLen) * sin startAngle)
                     (xPos + outerCircleRadius * cos startAngle)
                     (yPos + outerCircleRadius * sin startAngle)
    startAngle = snd $ maximum $ map (\(a, b) -> (b - a, (a + b) / 2)) angles
    outerCircleRadius = bool svgStateRadius svgAcceptStateRadius isF

drawStraightTransition :: PositionedTransition -> [SVG Double]
drawStraightTransition (PT label x1 y1 x2 y2) = [Line x1 y1 x2 y2,
                                                 Text textX textY label]
  where
    textX = (x1 + x2) / 2 + if x2 - x1 == 0 then svgTransLabelGap else offsetFactor * gradient
    textY = (y1 + y2) / 2 - offsetFactor
    offsetFactor = svgTransLabelGap / (-sqrt (1 + gradient**2))
    gradient = (y2 - y1) / (x2 - x1)

drawLoopTransition :: (PositionedState, (T.Text, Double)) -> [SVG Double]
drawLoopTransition (state, (label, orientation)) = [Arc x1 y1 x2 y2 loopRadius True True,
                                                    Text labelX labelY label]
  where
    -- arc origin
    x1 = x state + stateRadius * cos (orientation - loopSeparationAngle/2)
    y1 = y state + stateRadius * sin (orientation - loopSeparationAngle/2)
    -- arc end
    x2 = x state + stateRadius * cos (orientation - loopSeparationAngle/2 + loopSeparationAngle)
    y2 = y state + stateRadius * sin (orientation - loopSeparationAngle/2 + loopSeparationAngle)
    -- label positioning: find loop centre and offset by radius+gap
    loopAngle = acos $ (loopRadius**2 - stateRadius**2 * (1 - cos loopSeparationAngle)) / loopRadius**2
    centreOffsetAngle = orientation + loopAngle/2 - pi/2
    centreX = x1 - loopRadius * sin centreOffsetAngle
    centreY = y1 + loopRadius * cos centreOffsetAngle
    labelX = centreX + (loopRadius + svgTransLabelGap) * cos orientation
    labelY = centreY + (loopRadius + svgTransLabelGap) * sin orientation
    -- gather state information
    stateRadius = if isFinal state then svgAcceptStateRadius else svgStateRadius
