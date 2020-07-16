module Main (main) where

import Data.Maybe (fromMaybe)
import Sfinfo (proposeUpdate)
import Turtle (argText, need, options)

main :: IO ()
main = do
  outdatedList <- options "Propose spec files update" (argText "outdated" "Pkgtreediff outdated list")
  home <- need "HOME"
  proposeUpdate (fromMaybe "/home/fedora" home) outdatedList
