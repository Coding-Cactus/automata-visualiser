
module Automata.Render.Svg (svg, svgAnimation) where

import Automata.Types
import Automata.Render.Svg.Types

import Data.Bool
import Data.List (sort, partition, sortBy)
import qualified Data.Text as T

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
