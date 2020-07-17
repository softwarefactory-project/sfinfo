module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Sfinfo (comparePipAndRpm, proposeUpdate)
import Turtle

data Command = ComputeDiff Text | ProposeUpdate (Text, Text)
  deriving (Show)

usage :: Parser Command
usage =
  fmap
    ComputeDiff
    ( subcommand
        "compute-diff"
        "Compare package between rpm and pypi"
        (argText "outdated-list" "the output file name")
    )
    <|> fmap
      ProposeUpdate
      ( subcommand
          "propose-update"
          "Generate git reviews to bump outdated packages"
          ( (,) <$> argText "outdated-list" "gen-diff output file name"
              <*> argText "gerrit-user" "Gerrit ssh name user to push review"
          )
      )

main :: IO ()
main = do
  command <- options "SFInfo toolkit" usage
  home' <- need "HOME"
  case command of
    ComputeDiff outputFile -> comparePipAndRpm outputFile
    ProposeUpdate (outdatedList, gerritUser) -> proposeUpdate (fromMaybe "/home/fedora" home') gerritUser outdatedList
