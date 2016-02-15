module Development.Iridium.Hackage
  ( retrieveLatestVersion
  )
where



import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad ( mzero, when )
import           Data.Maybe ( listToMaybe )
import           Control.Monad.Trans.MultiRWS
import           Control.Monad

import qualified Network.HTTP           as HTTP
import qualified Text.XmlHtml           as Html
import qualified Network.URI            as URI
import qualified Data.Text              as Text

import           Development.Iridium.Logging
import           Development.Iridium.Types



retrieveLatestVersion
  :: ( MonadIO m
     , MonadMultiState LogState m
     , MonadPlus m
     )
  => String -> String -> m String
retrieveLatestVersion remoteUrl pkgName = do
  let urlStr :: String = remoteUrl ++ "/package/" ++ pkgName ++ "/preferred"
  pushLog LogLevelInfo $ "Looking up latest version from hackage via url " ++ urlStr
  url <- case URI.parseURI urlStr of
    Nothing -> do
      pushLog LogLevelError "bad URI"
      mzero
    Just u -> return u
  result <- liftIO $ HTTP.simpleHTTP (HTTP.mkRequest HTTP.GET url)
  rawHtml <- case result of
    Left _ -> do
      pushLog LogLevelError "Error: Could not retrieve hackage version"
      mzero
    Right x -> return $ HTTP.rspBody x
  case Html.parseHTML "hackage:response" rawHtml of
    Left e -> do
      pushLog LogLevelError e
      mzero
    Right x -> do
      let mStr  = fmap (Text.unpack . Html.nodeText)
                $ ( listToMaybe . Html.childNodes )
              =<< listToMaybe
                  ( reverse
                  $ Html.descendantElementsTag (Text.pack "a")
                    ( head
                    $ Html.docContent
                    $ x
                    )
                  )
      case mStr of
        Nothing -> do
          pushLog LogLevelError "Error: Could not decode hackage response."
          mzero
        Just s -> return s
