module Automata.Layout.Energy (layout, layout') where

import Automata.Types
import qualified Automata.Layout.ZigZag as ZigZag

import Data.Bool
import System.Random

lambda1 :: Double
lambda2 :: Double
lambda3 :: Double
lambda1 = 1
lambda2 = 1
lambda3 = 15

iterPerStep :: Int
iterPerStep = 10

gamma :: Double
initialTemp :: Double
gamma = 0.95
initialTemp = 10


layout' :: Automaton s t -> AutomatonLayoutAnimation s t
layout' a = ALA {
    frames = map (positionedStates . \(a', _, _) -> a') $ take 50 $ iterate runStep (ZigZag.layout a, initialTemp, mkStdGen 8607857038680846995),
    transitionsStatic = transitions a
  }

layout :: Automaton s t -> AutomatonLayout s t
layout a = let (a', _, _) = untilStableOrCount 50 runStep (ZigZag.layout a, initialTemp, mkStdGen 8607857038680846995) in a'
  where
    untilStableOrCount 0 _ x = x
    untilStableOrCount n f x@(AL sts trs, _, _) = bool (untilStableOrCount (n-1) f (f x)) x stable
      where stable = let (AL fsts ftrs, _, _) = f x in abs (totalEnergy sts trs - totalEnergy fsts ftrs) < 0.0001

runStep :: (AutomatonLayout s t, Double, StdGen) -> (AutomatonLayout s t, Double, StdGen)
runStep (a, tmp, gen) = (
    AL {
      positionedStates = sts2,
      positionedTransitions = trs2
    },
    tmp * gamma,
    gen2
  )
  where (_, _, AL sts2 trs2, gen2) = step (tmp, iterPerStep, a, gen)

step :: (Double, Int, AutomatonLayout s t, StdGen) -> (Double, Int, AutomatonLayout s t, StdGen)
step (temp, 0, a, gen) = (temp, 0, a, gen)
step (temp, iter, AL states t, gen) = step (temp, iter-1, AL nextStates t, gen2)
  where
    (proposed, gen1) = nextState temp states gen
    delta = totalEnergy proposed t - totalEnergy states t
    (r, gen2) = randomR (0, 1) gen1
    nextStates = bool states proposed (delta < 0 || r < exp ((-delta) / temp))

nextState :: Double -> [PositionedState] -> StdGen -> ([PositionedState], StdGen)
nextState temp states gen = (xs ++ v' : tail ys, gen3)
  where
    v = head ys
    v' = v { x = x v + dx, y = y v + dy }
    (xs, ys) = splitAt i states
    (i, gen1) = randomR (0, length states - 1) gen
    stepSize = (fromIntegral (length states) * (temp / initialTemp)**2) / 2
    (dx, gen2) = randomR (-stepSize, stepSize) gen1
    (dy, gen3) = randomR (-stepSize, stepSize) gen2

totalEnergy :: [PositionedState] -> [Transition s t] -> Double
totalEnergy s t = lambda1 * nodeDistances s + lambda2 * edgeLengths s t + lambda3 * nodeEdgeDistances s t

nodeDistances :: [PositionedState] -> Double
nodeDistances states = sum $ map energy states
  where energy u = sum $ map (\v -> 1 / dist u v ** 2) $ filter (\v -> psid v > psid u) states

edgeLengths :: [PositionedState] -> [Transition s t] -> Double
edgeLengths states transitions = sum $ map energy transitions
  where
    getPState (S sid _) = head $ filter ((==) sid . psid) states
    energy (T u v _) = dist (getPState u) (getPState v) ** 2

nodeEdgeDistances :: [PositionedState] -> [Transition s t] -> Double
nodeEdgeDistances states transitions = sum $ map energy states
  where
    getPState (S sid _) = head $ filter ((==) sid . psid) states
    energy s@PS { psid, x=sx, y=sy } = sum $ map distCost $ filter (\(T (S uId _) (S vId _) _) -> uId /= psid && vId /= psid) transitions
      where
        distCost (T u v _) =  1 / max 0.001 distance ** 2
          where
            (u'@PS { x=ux, y=uy }, v'@PS { x=vx, y=vy }) = (getPState u, getPState v)
            a = ux - sx
            b = vx - ux
            c = uy - sy
            d = vy - uy
            lambda = (a*b + c*d) / (b**2 + d**2)
            distance = if betweenAnyOrder ux vx (sx + a + lambda*b) && betweenAnyOrder uy vy (sy + c + lambda*d)
              then sqrt ((a + lambda*b)**2 + (c + lambda*d)**2)
              else min (dist s u') (dist s v')




betweenAnyOrder :: Ord a => a -> a -> a -> Bool
betweenAnyOrder a b x = lo <= x && x <= hi
  where (lo, hi) = (min a b, max a b)

dist :: PositionedState -> PositionedState -> Double
dist (PS { x=ux, y=uy }) (PS { x=vx, y=vy }) = sqrt ((vx - ux)**2 + (vy - uy)**2)
