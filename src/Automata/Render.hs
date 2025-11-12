{-# LANGUAGE LambdaCase #-}

module Automata.Render (render, saveToFile, svg) where

import Automata.Types
import Automata.Layout
import Automata.Render.Svg

import qualified Control.Monad.State as S
import qualified Data.Text.IO as T

saveToFile :: String -> AutomatonRender -> IO ()
saveToFile fname = \case
  TextData txt -> T.writeFile fname txt
  BinaryData _ -> undefined

render :: String -> (AutomatonLayout -> AutomatonRender) -> AutomatonBuilder s t -> IO ()
render file fn a = saveToFile file $ fn $ layout $ S.execState a (Automaton [] [] (-1) [])
