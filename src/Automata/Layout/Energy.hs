module Automata.Layout.Energy (layout, layout') where

import Automata.Types
import qualified Automata.Layout.Constrained as Constrained
import Automata.Layout.Force (defaultLength)

import Data.Bool
import System.Random

lambda1 :: Double
lambda2 :: Double
lambda3 :: Double
lambda4 :: Double
lambda1 = 1
lambda2 = 2
lambda3 = 25
lambda4 = 1.5

iterPerStep :: Int
iterPerStep = 10

gamma :: Double
initialTemp :: Double
gamma = 0.95
initialTemp = 20


layout' :: Automaton s t -> AutomatonLayoutAnimation s t
layout' a = ALA {
    frames = map (positionedStates . \(a', _, _) -> a') $ take 50 $ iterate runStep (Constrained.layout defaultLength a, initialTemp, mkStdGen 8607857038680846995),
    transitionsStatic = transitions a
  }

layout :: Automaton s t -> AutomatonLayout s t
layout a = let (a', _, _) = untilStableOrCount 50 runStep (Constrained.layout defaultLength a, initialTemp, mkStdGen 8607857038680846995) in a'
  where
    untilStableOrCount 0 _ x = x
    untilStableOrCount n f x@(AL grps trs, _, _) = bool (untilStableOrCount (n-1) f (f x)) x stable
      where stable = let (AL fgrps ftrs, _, _) = f x in abs (totalEnergy grps trs - totalEnergy fgrps ftrs) < 0.0001


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
step (temp, iter, AL groups t, gen) = step (temp, iter-1, AL nextGroups t, gen2)
  where
    (proposed, gen1) = nextState temp groups gen
    delta = totalEnergy proposed t - totalEnergy groups t
    (r, gen2) = randomR (0, 1) gen1
    nextGroups = bool groups proposed (delta < 0 || r < exp ((-delta) / temp))

nextState :: Double -> [[PositionedState]] -> StdGen -> ([[PositionedState]], StdGen)
nextState temp groups gen = (xs ++ g' : tail ys, gen3)
  where
    g = head ys
    g' = map (\s -> s { x = x s + dx, y = y s + dy }) g
    (xs, ys) = splitAt i groups
    (i, gen1) = randomR (0, length groups - 1) gen
    stepSize = (fromIntegral (length groups) * (temp / initialTemp)**2) * 2
    (dx, gen2) = randomR (-stepSize, stepSize) gen1
    (dy, gen3) = randomR (-stepSize, stepSize) gen2

totalEnergy :: [[PositionedState]] -> [Transition s t] -> Double
totalEnergy g t = lambda1 * nodeDistances g + lambda2 * edgeLengths g t + lambda3 * nodeEdgeDistances g t + lambda4 * horizontality g

nodeDistances :: [[PositionedState]] -> Double
nodeDistances groups = sum $ map energy $ concat groups
  where energy u = sum $ map (\v -> 1 / dist u v ** 2) $ concatMap (filter (\v -> psid v > psid u)) groups

edgeLengths :: [[PositionedState]] -> [Transition s t] -> Double
edgeLengths groups transitions = sum $ map energy transitions
  where
    getPState (S sid _) = head $ concatMap (filter ((==) sid . psid)) groups
    energy (T u v _) = dist (getPState u) (getPState v) ** 2

nodeEdgeDistances :: [[PositionedState]] -> [Transition s t] -> Double
nodeEdgeDistances groups transitions = sum $ map energy $ concat groups
  where
    getPState (S sid _) = head $ concatMap (filter ((==) sid . psid)) groups
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

horizontality :: [[PositionedState]] -> Double
horizontality groups = sum $ map energy $ concat groups
  where
    energy (PS { y=y }) = (middle - y) ** 2
    middle = sum (map y $ concat groups) / fromIntegral (length $ concat groups)


betweenAnyOrder :: Ord a => a -> a -> a -> Bool
betweenAnyOrder a b x = lo <= x && x <= hi
  where (lo, hi) = (min a b, max a b)

dist :: PositionedState -> PositionedState -> Double
dist (PS { x=ux, y=uy }) (PS { x=vx, y=vy }) = sqrt ((vx - ux)**2 + (vy - uy)**2)
