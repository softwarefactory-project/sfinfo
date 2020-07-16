-- | A clone function
module Sfinfo.Cloner
  ( clone,
  )
where

import Data.Maybe
import Data.Text
import Network.URI
import Turtle

-- | Strip scheme, port, auth and query fragments
urlToDir :: Text -> Maybe Text
urlToDir url = do
  uri <- parseURI (unpack url)
  uriAuth <- uriAuthority uri
  return (pack (uriRegName uriAuth <> uriPath uri))

clone :: MonadIO io => Text -> Text -> io ()
clone base url =
  case urlToDir url of
    Nothing -> err "Invalid url"
    Just urlDir -> do
      gitDirExist <- testdir (fromText (dest <> "/.git"))
      unless gitDirExist (procs "/bin/git" ["clone", url, dest] mempty)
      where
        dest = base <> fromMaybe urlDir (stripSuffix ".git" urlDir)
