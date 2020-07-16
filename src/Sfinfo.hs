module Sfinfo
  ( proposeUpdate,
  )
where

import Control.Monad (void)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T

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

updatePackage :: Text -> (Text, Text) -> IO ()
updatePackage _home (name, _version) = print $ "Updating..." <> name -- TODO HERE

proposeUpdate :: Text -> Text -> IO ()
proposeUpdate home fn = do
  print $ "Proposing update using: " <> fn
  fcontent <- T.readFile (T.unpack fn)
  mapM_ (updatePackage home . readOutdatedLine) $ T.lines fcontent
