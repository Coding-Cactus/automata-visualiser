module Automata.Layout.Force (layout, layout', layoutPositioned, defaultLength) where

import Automata.Types
import qualified Automata.Layout.Constrained as Constrained

import Data.Bool

stepSize :: Double
springNatLen :: Double
springConstant :: Double
electricalConstant :: Double
gravitationalConstant :: Double
gravOffset :: Double
stepSize = 0.1
springNatLen = 2
springConstant = 2
electricalConstant = 2.25
gravitationalConstant = 1.5
gravOffset = 1

layout' :: Automaton s t -> AutomatonLayoutAnimation s t
layout' a = ALA {
    frames = map positionedStates $ take 1000 $ iterate simulate (Constrained.layout defaultLength a),
    transitionsStatic = transitions a
  }

layout :: Automaton s t -> AutomatonLayout s t
layout = layoutPositioned . Constrained.layout defaultLength

layoutPositioned :: AutomatonLayout s t -> AutomatonLayout s t
layoutPositioned = untilStableOrCount 1000 simulate
  where
    untilStableOrCount 0 _ state = state
    untilStableOrCount n f state = bool (untilStableOrCount (n-1) f (f state)) state (all (\(dx, dy) -> abs dx < 0.001 && abs dy < 0.001) $ movement state)

simulate :: AutomatonLayout s t -> AutomatonLayout s t
simulate a@(AL groups trns) = AL (zipWith (\g (dx, dy) -> map (\u -> u { x = dx * stepSize + x u, y = dy * stepSize + y u }) g) groups (movement a)) trns

movement :: AutomatonLayout s t -> [(Double, Double)]
movement (AL groups trns) = map (\u -> (groupMotionX u, groupMotionY u)) groups
  where
    groupMotionX = sum . map motionX
    groupMotionY = sum . map motionY

    motionX u = gravityX u + sum (map (\v -> electicX u v + transitionTensionX u v) $ concatMap (filter (u /=)) groups)
    motionY u = gravityY u + sum (map (\v -> electicY u v + transitionTensionY u v) $ concatMap (filter (u /=)) groups)

    transitionTensionX u v = if hasTransition u v then tensionX u v else 0
    transitionTensionY u v = if hasTransition u v then tensionY u v else 0
    hasTransition u v = any (\(T _ id1 id2 _) -> (id1, id2) == (psid u, psid v) || (id1, id2) == (psid v, psid u)) trns

    tensionX u v = springConstant * (dist u v - springNatLen) * ((x v - x u) / dist u v)
    tensionY u v = springConstant * (dist u v - springNatLen) * ((y v - y u) / dist u v)
    electicX u v = -((electricalConstant / (dist u v ** 2)) * ((x v - x u) / dist u v))
    electicY u v = -((electricalConstant / (dist u v ** 2)) * ((y v - y u) / dist u v))
    gravityX u = gravForceLeft u * ((gravLeft - x u) / gravDistLeft u) + gravForceRight u * ((gravRight - x u) / gravDistRight u)
    gravityY u = gravForceLeft u * ((gravY - y u) / gravDistLeft u) + gravForceRight u * ((gravY - y u) / gravDistRight u)

    gravForceLeft  u = gravitationalConstant / gravDistLeft u
    gravForceRight u = gravitationalConstant / gravDistRight u
    gravDistLeft  u = pythag (gravLeft  - x u) (gravY - y u) ** 2
    gravDistRight u = pythag (gravRight - x u) (gravY - y u) ** 2
    gravLeft  = minimum (map x $ concat groups) - gravOffset
    gravRight = maximum (map x $ concat groups) + gravOffset
    gravY = sum (map y $ concat groups) / fromIntegral (length $ concat groups)

    dist u v = pythag (xDist u v) (yDist u v)
    xDist u v = x v - x u
    yDist u v = y v - y u
    pythag x y = max 0.01 (sqrt $ x ** 2 + y ** 2)

-- edge length estimate between two connected nodes
defaultLength :: Double
defaultLength = iterate estimate 1 !! 10
  where estimate d = electricalConstant / (springConstant * d**2) + springNatLen
