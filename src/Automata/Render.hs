module Automata.Render (render, render', svg, svgAnimation, tikz) where

import Automata.Types
import Automata.Layout.Combined
import Automata.Render.Svg
import Automata.Render.Tikz

import qualified Control.Monad.State as S
import qualified Data.Text.IO as T


render :: AutomatonConfig -> String -> (AutomatonConfig -> AutomatonLayout s t -> AutomatonRender) -> AutomatonBuilder s t -> IO ()
render config file fn a = do
  let automata = S.execState a (Automaton [] [] (-1) [] [])
  let positioned = layout automata
  rendered <- fn config positioned
  T.writeFile file rendered

render' :: AutomatonConfig -> String -> (AutomatonConfig -> AutomatonLayoutAnimation s t -> AutomatonRender) -> AutomatonBuilder s t -> IO ()
render' config file  fn a = do
  let automata = S.execState a (Automaton [] [] (-1) [] [])
  let positioned = layout' automata
  rendered <- fn config positioned
  T.writeFile file rendered
