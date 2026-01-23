{-# LANGUAGE LambdaCase #-}

module Automata.Render (render, render', saveToFile, svg, svgAnimation) where

import Automata.Types
import Automata.Layout.Energy
import Automata.Render.Svg

import qualified Control.Monad.State as S
import qualified Data.Text.IO as T

saveToFile :: String -> AutomatonRender -> IO ()
saveToFile fname = \case
  TextData txt -> T.writeFile fname txt
  BinaryData _ -> undefined

render :: String -> (AutomatonLayout s t -> AutomatonRender) -> AutomatonBuilder s t -> IO ()
render file fn a = saveToFile file $ fn $ layout $ S.execState a (Automaton [] [] (-1) [] [])

render' :: String -> (AutomatonLayoutAnimation s t -> AutomatonRender) -> AutomatonBuilder s t -> IO ()
render' file fn a = saveToFile file $ fn $ layout' $ S.execState a (Automaton [] [] (-1) [] [])
