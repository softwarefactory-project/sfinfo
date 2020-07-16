module Sfinfo
  ( proposeUpdate,
  )
where

import Control.Monad (void)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Gerrit (GerritClient)
import qualified Gerrit
import Sfinfo.Cloner
import Sfinfo.RpmSpec

-- import Turtle

-- | A breakOn that drops the seprator
-- >>> breakOn' " -> " "a -> b"
-- ("a","b")
breakOn' :: Text -> Text -> (Text, Text)
breakOn' sep txt = (a, T.drop (T.length sep) b)
  where
    (a, b) = T.breakOn sep txt

-- | Read pkgtree diff output
-- >>> readOutdatedLine "ansible: 2.6.19-2.el7 -> 2.9.10-1.el7"
-- ("ansible","2.9.10")
readOutdatedLine :: Text -> (Text, Text)
readOutdatedLine line = (package, version)
  where
    (package, versions) = breakOn' ": " line
    (_currentVersion, desiredVersion) = breakOn' " -> " versions
    (version, _release) = breakOn' "-" desiredVersion

author :: Text
author = "sfinfo <softwarefactory-dev@redhat.com>"

committer :: Text
committer = "TristanCacqueray"

updatePackage :: GerritClient -> Text -> (Text, Text) -> IO ()
updatePackage client home (name, version) =
  do
    gitDir <- clone gitBase projectUrl
    (specPath, specContent) <- getSpec gitDir
    date <- getDate
    T.writeFile specPath (bumpVersion version author date specContent)
    void $ commit gitDir commitTitle
    gerritChanges <- Gerrit.queryChanges gerritQuery client
    -- print $ gitDir <> " -> " <> (T.pack $ show gerritChanges)
    case gerritChanges of
      [] -> do
        putStrLn ("== submitting review with> " <> show gitDir)
        Sfinfo.Cloner.gitReview committer projectName gitDir
      [x] ->
        putStrLn ("== review already exit> " <> show x)
      _ -> print $ "Humm, multiple change proposed: " <> show gerritChanges
  where
    gitBase = home <> "/src/"
    projectName = "rpms/" <> T.replace "python3-" "python-" name
    projectUrl = "https://softwarefactory-project.io/r/" <> projectName
    commitTitle = "Bump to " <> version
    gerritQuery = [Gerrit.Project projectName, Gerrit.CommitMessage commitTitle]

proposeUpdate :: Text -> Text -> IO ()
proposeUpdate home fn = Gerrit.withClient "https://softwarefactory-project.io/r/" $ \client -> do
  print $ "Proposing update using: " <> fn
  fcontent <- T.readFile (T.unpack fn)
  mapM_ (updatePackage client home . readOutdatedLine) $ T.lines fcontent
