module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Sfinfo (proposeUpdate)
import Turtle (Parser, argText, need, options)

usage :: Parser (Text, Text)
usage =
  (,) <$> argText "outdated-list" "Pkgtreediff outdated output file"
    <*> argText "gerrit-user" "Gerrit ssh name user to push review"

main :: IO ()
main = do
  (outdatedList, gerritUser) <- options "Propose spec files update" usage
  home <- need "HOME"
  proposeUpdate (fromMaybe "/home/fedora" home) outdatedList gerritUser
