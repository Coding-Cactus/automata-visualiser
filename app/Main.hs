module Main where

import Automata
import Control.Monad (forM_)

a1 :: AutomatonBuilder String String
a1 = do
  a <- state "q_0"
  b <- state "q_1"
  c <- state "q_2"
  d <- state "q_3"

  initial a
  final a

  a >--[""]--> b
  a >--["0"]--> a
  a >--["0"]--> c
  a >--["\\epsilon"]--> c

  b >--["0"]--> b
  b >--["0"]--> b
  b >--["0"]--> b
  b >--["0"]--> b
  b >--["0"]--> b
  b >--["0"]--> b
  b >--["1"]--> b
  b >--["2"]--> b
  a >--["0"]--> b

  c >--["0"]--> c
  c >--["0"]--> b
  b >--["1"]--> c
  c >--["0","1"]--> b

  c `below` a

a2 = do
  a <- state "a"
  b <- state "b"
  c <- state "c"
  d <- state "d"
  e <- state "e"

  initial a
  final a

  a >--[""]--> d
  a >--[""]--> b
  b >--[""]--> e
  d >--[""]--> c
  c >--[""]--> e

a3 :: AutomatonBuilder String Int
a3 = do
  a <- state "a"
  b <- state "b"
  c <- state "c"

  initial a
  final a

  a >--[1]--> b
  a >--[0]--> a
  a >--[0]--> c

  b >--[0]--> b
  b >--[1]--> b
  b >--[2]--> b
  a >--[0]--> b

  c >--[0]--> c
  c >--[0]--> b
  b >--[1]--> c
  c >--[0,1]--> b

  c `below` a

  x <- state "x"
  y <- state "y"
  z <- state "z"

  final x

  x >--[1]--> y
  x >--[0]--> x
  x >--[0]--> z

  y >--[0]--> y
  y >--[1]--> y
  y >--[2]--> y
  x >--[0]--> y

  z >--[0]--> z
  z >--[0]--> y
  y >--[1]--> z
  z >--[0,1]--> y

  z `below` x



  h <- state "h"
  i <- state "i"
  j <- state "j"

  final h

  h >--[1]--> i
  h >--[0]--> h
  h >--[0]--> j

  i >--[0]--> i
  i >--[1]--> i
  i >--[2]--> i
  h >--[0]--> i

  j >--[0]--> j
  j >--[0]--> i
  i >--[1]--> j
  j >--[0,1]--> i

  j `below` h

a4 :: AutomatonBuilder String Int
a4 = do
  a <- state "q_{even}"
  b <- state "q_{odd}"

  initial a
  final a

  a >--[0]--> a
  a >--[1]--> b

  b >--[0]--> a
  b >--[1]--> b

examplePDA :: AutomatonBuilder String (StackTransition String String)
examplePDA = do
  a <- state "a"
  b <- state "b"
  c <- state "c"

  initial a
  final c

  a >--["\\epsilon" ~~ ("\\epsilon", "\\$")]--> b

  b >--["(" ~~ ("\\epsilon", "(")]--> b
  b >--[")" ~~ ("(", "\\epsilon")]--> b

  b >--["\\epsilon" ~~ ("\\$", "\\epsilon")]--> c


shorthand :: AutomatonBuilder String Int
shorthand = do
  a <- state "a"
  b <- state "b"
  c <- state "c"

  initial a
  final a

  tr' a a 0
  tr' a b 1
  tr' b a 1
  tr' b c 0
  tr' c b 0
  tr' c c 1


positioned :: AutomatonBuilder Char Int
positioned = do
  a <- state 'a'
  b <- state 'b'
  c <- state 'c'
  d <- state 'd'
  e <- state 'e'

  a `above` b
  a `leftOf` c
  position c d 45 1
  position e c 135 1.3

  x <- state 'x'
  y <- state 'y'
  z <- state 'z'

  position y x 60 2
  position z x 120 2

turing :: AutomatonBuilder String (TuringMachineTransition Int)
turing = do
  a <- state "q_a"
  b <- state "q_b"
  c <- state "q_c"
  d <- state "q_d"
  e <- state "q_e"

  initial a

  a >--[TmT 0 1 L]--> b
  a >--[TmT 1 1 R]--> c

  b >--[TmT 1 1 L]--> b
  b >--[TmT 0 1 L]--> c

  c >--[TmT 0 1 L]--> d
  c >--[TmT 1 0 R]--> e

  d >--[TmT 1 1 R]--> d
  d >--[TmT 0 1 R]--> a

  e >--[TmT 1 0 R]--> a

  position a b 45 1
  position a d (-45) 1
  position d c 45 1

  e `below` d


generalDivisibility :: Int -> AutomatonBuilder String Int
generalDivisibility n = do
  states <- mapM (\i -> state $ "q_{" <> show i <> "}") [0 .. n-1]
  forM_ [0 .. n-1 ] $ \i -> do
    let s = states !! i
    let s0 = states !! ((i*2) `mod` n)
    let s1 = states !! ((i*2+1) `mod` n)

    s >--[0]--> s0
    s >--[1]--> s1

  initial (head states)
  final (head states)

config :: AutomatonConfig
config = setConfig {
  acceptanceStyle = Arrow
}

main :: IO ()
main = do
  let a = generalDivisibility 4

  putStrLn "Rendering SVG..."
  render config "renders/testing.svg" svg a

  putStrLn "Rendering TikZ..."
  render config "renders/tikz/testing.tex" tikz a
