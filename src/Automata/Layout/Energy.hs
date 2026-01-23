module Automata.Layout.Energy (layout, layout') where

import Automata.Types

import Data.Bool
import System.Random
import Data.List (nub, foldl')

lambda1 :: Double
lambda2 :: Double
lambda3 :: Double
lambda1 = 1
lambda2 = 2
lambda3 = 25

iterPerStep :: Int
iterPerStep = 10

gamma :: Double
initialTemp :: Double
gamma = 0.95
initialTemp = 20


layout' :: Automaton s t -> AutomatonLayoutAnimation s t
layout' a = ALA {
    frames = map (positionedStates . \(a', _, _) -> a') $ take 50 $ iterate runStep (initialLayout a, initialTemp, mkStdGen 8607857038680846995),
    transitionsStatic = transitions a
  }

layout :: Automaton s t -> AutomatonLayout s t
layout a = let (a', _, _) = untilStableOrCount 50 runStep (initialLayout a, initialTemp, mkStdGen 8607857038680846995) in a'
  where
    untilStableOrCount 0 _ x = x
    untilStableOrCount n f x@(AL sts trs, _, _) = bool (untilStableOrCount (n-1) f (f x)) x stable
      where stable = let (AL fsts ftrs, _, _) = f x in abs (totalEnergy sts trs - totalEnergy fsts ftrs) < 0.0001

initialLayout :: Automaton s t -> AutomatonLayout s t
initialLayout (Automaton states trs initial finals hints) = AL {
    -- group directionally related states and then random layout
    positionedStates = concatMap (moveRandom . distributeNodes) groups,
    positionedTransitions = trs
  }
  where
    -- group states into connected components by position constraint relation
    groups = group states []
    group [] gs = gs
    group (x:xs) gs = group xs' (g:gs) -- group stateQueue foundGroups
      where
        xs' = filter (`notElem` g) xs -- don't create new component if state already in this component (remove it from queue)
        g = gather [x] []
        -- recursively group all states connected to 'x' into the same group
        gather [] gr = gr
        gather (s:ss) gr = gather (filter (\s' -> s' `notElem` ss && s' `notElem` gr) (nub (map (`without` s) (filter (constrained s) hints))) ++ ss) (s:gr)

    -- distribute nodes within groups to satisfy position constraints
    distributeNodes grp = map fixedToPositioned $ distribute [ s | s <- grp, h <- hints, constrained s h && constrained (head grp) h && s /= head grp] [(head grp, (1, 1))]
      where
        fixedToPositioned (S sid label, (x, y)) = PS {
          psid = sid,
          sLabel = drawLabel label,
          x = x, y = y,
          isInitial = sid == initial,
          isFinal = sid `elem` finals
        }

    -- go though node queue, assigning positions based off the fixed position of previously distributed nodes
    distribute [] fixed = fixed
    distribute (n:ns) fixed = distribute ns' ((n, distribution):fixed)
      where
        -- add, to the queue, the unpositioned nodes connected to current node
        ns' = ns ++ filter (\m -> m /= n && m `notElem` fixedNodes && m `notElem` ns && not (null [h | h <- hints, constrained m h && constrained n h])) states
        distribution = foldl' sumPair (snd fixedConstraintNode) fixedConstraints

        fixedNodes = map fst fixed
        -- guaranteed to be exactly 1 fixed constraining node for `n`
        -- fixedConstraintNode = head (filter (`elem` fixedNodes) (map (`without` n) (filter (constrained n) hints)))
        fixedConstraintNode = head [ m | m <- fixed, h <- hints, constrained (fst m) h && constrained n h ]
        -- fixedConstraintNodePos = snd $ head $ filter (\m -> fst m == fixedConstraintNode) fixed
        fixedConstraints = [(dx, dy) |
                              h <- hints,
                              constrained n h && constrained (fst fixedConstraintNode) h,
                              let (dx, dy) = offset (fst fixedConstraintNode) h]
        offset m (Le a b) = bool (1, 0) (-1, 0) (Le a b == Le n m)
        offset m (Ab a b) = bool (0, 1) (0, -1) (Ab a b == Ab n m)

    moveRandom = id


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

-- choose from groups or single satets. Move groups as one unit
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

sumPair :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumPair (a, b) (x, y) = (a+x, b+y)
