{-# LANGUAGE OverloadedStrings #-}

module Automata.Render.Tikz.Types where


import qualified Data.Text as T (Text, intercalate, pack, unlines)
import Data.List (maximumBy, sort)
import Data.Bool

data TikzDrawing = TD [Node] [Transition]

data Node = N {
  nodeId :: Int,
  nodeLabel :: T.Text,
  nodeX :: Double,
  nodeY :: Double,
  nodeIsInitial :: Bool,
  nodeIsFinal :: Bool
}

data Transition = Straight Int Int Double [T.Text] EdgeStyle
                | Loop Int [T.Text] Double Double
                deriving Show

data EdgeStyle = NoBend | LeftBend | RightBend deriving Show

writeEdgeStyle :: EdgeStyle -> T.Text
writeEdgeStyle NoBend = ""
writeEdgeStyle LeftBend = "[bend left=22]"
writeEdgeStyle RightBend = "[bend right=22]"

writeLoopPosition :: Double -> Double -> T.Text
writeLoopPosition a1 a2 = "[in=" <> T.pack (show a1) <> ",out=" <> T.pack (show a2) <> ",loop]"

angleFrom :: Int -> Transition -> Double
angleFrom _ (Loop _ _ a1 a2) = normaliseAngle $ (a1 + a2) / 2
angleFrom u (Straight x _ a _ _) = normaliseAngle $ bool (pi+a) a (u == x)

normaliseAngle :: Double -> Double
normaliseAngle theta
  | theta < 0 = normaliseAngle $ theta + 360
  | theta > 360 = normaliseAngle $ theta - 360
  | otherwise = theta



render :: TikzDrawing -> T.Text
render (TD nodes transitions) = boilerplate initialPos $ T.intercalate "\n\n" [writeNodes nodes, writeTransitions transitions]
  where
    -- calculate position of start state arrow
    angles = [0, 90, 180, 270]
    initialPos
      | null edgeAngles = "left"
      | otherwise =  snd $ head $ filter (\(a, _) -> a == bestAngle) $ zip angles ["right", "above", "left", "below"]
    bestAngle = maximumBy (\a1 a2 -> compare (spaceAround a1) (spaceAround a2)) angles
    spaceAround angle = min (spaceLeft angle) (spaceRight angle)
    spaceLeft  angle = minimum $ map (abs . (-) angle) $ filter (>= angle) (head edgeAngles + 360 : edgeAngles)
    spaceRight angle = minimum $ map (abs . (-) angle) $ filter (<= angle) (last edgeAngles - 360 : edgeAngles)
    edgeAngles = sort $ map (angleFrom initialNode) $ filter edgeOnInitial transitions
    initialNode = nodeId $ head $ filter nodeIsInitial nodes
    edgeOnInitial (Straight u v _ _ _) = u == initialNode || v == initialNode
    edgeOnInitial (Loop u _ _ _) = u == initialNode


boilerplate :: T.Text -> T.Text -> T.Text
boilerplate initialPos content = T.unlines [
    "\\usetikzlibrary{automata,positioning}",
    "\\begin{tikzpicture}[auto,initial text={},initial where=" <> initialPos <> "]",
    content,
    "\\end{tikzpicture}"
  ]

writeNodes :: [Node] -> T.Text
writeNodes = T.unlines . map writeNode
  where
    writeNode (N i label x y isI isF) = T.intercalate " " [
        "\\node[state" <> bool "" ",initial" isI <> bool "" ",accepting" isF <> "]",
        "(" <> T.pack (show i) <> ")",
        "at (" <> T.pack (show x) <> ", " <> T.pack (show y) <> ")",
        "{" <> label <> "}"
      ] <> ";"

writeTransitions :: [Transition] -> T.Text
writeTransitions ts = "\\path[->]\n" <> T.intercalate "\n" (map writeTransition ts) <> ";"
  where
    writeTransition (Straight u v _ labels edgeStyle) = write u v (T.intercalate "," labels) edgeStyle 0 0
    writeTransition (Loop u labels theta1 theta2) = write u u (T.intercalate "," labels) NoBend theta1 theta2
    write u v label edgeStyle theta1 theta2 = T.intercalate " " [
        "(" <> T.pack (show u) <> ")",
        "edge",
        bool (writeEdgeStyle edgeStyle) (writeLoopPosition theta1 theta2) (u == v),
        "node",
        "{" <> label <> "}",
        "(" <> T.pack (show v) <> ")"
      ]
