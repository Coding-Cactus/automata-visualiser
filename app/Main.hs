module Main where

import Automata

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

  a `leftOf` b

main :: IO ()
main = do
  let a = a1

  putStrLn "Rendering SVG..."
  render "renders/testing.svg" svg a

  putStrLn "Rendering TikZ..."
  render "renders/tikz/testing.tex" tikz a
