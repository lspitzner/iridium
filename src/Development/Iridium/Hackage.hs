module Development.Iridium.Hackage
  ( retrieveLatestVersion
  , uploadPackage
  , uploadDocs
  )
where



import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad ( mzero, when )
import           Data.Maybe ( listToMaybe, maybeToList )
import           Control.Monad.Trans.MultiRWS
import           Control.Monad
import           Data.Version
import           Distribution.Package ( PackageName(..) )
import qualified Turtle                 as Turtle

import qualified Network.HTTP.Conduit   as HTTP
import qualified Text.XmlHtml           as Html
import qualified Data.Text              as Text

import           System.Process hiding ( cwd )

import           Development.Iridium.UI.Console
import           Development.Iridium.Types
import           Development.Iridium.Config
import           Development.Iridium.Utils

import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Lazy          as ByteStringL



retrieveLatestVersion
  :: ( MonadIO m
     , MonadMultiState LogState m
     , MonadPlus m
     )
  => String -> String -> m String
retrieveLatestVersion remoteUrl pkgName = do
  let urlStr :: String = remoteUrl ++ "/package/" ++ pkgName ++ "/preferred"
  pushLog LogLevelInfo $ "Looking up latest version from hackage via url " ++ urlStr
  -- url <- case URI.parseURI urlStr of
  --   Nothing -> do
  --     pushLog LogLevelError "bad URI"
  --     mzero
  --   Just u -> return u
  -- result <- liftIO $ HTTP.simpleHTTP (HTTP.mkRequest HTTP.GET url)
  -- rawHtml <- case result of
  --   Left _ -> do
  --     pushLog LogLevelError "Error: Could not retrieve hackage version"
  --     mzero
  --   Right x -> return $ HTTP.rspBody x

  -- TODO: error handling
  rawHtml <- HTTP.simpleHttp urlStr
  case Html.parseHTML "hackage:response"
     $ ByteString.concat
     $ ByteStringL.toChunks rawHtml of
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
        Just s -> do
          pushLog LogLevelInfoVerbose $ "got: " ++ s
          return s

uploadPackage
  :: forall m
   . ( MonadIO m
     , MonadPlus m
     , MonadMultiReader Config m
     , MonadMultiReader Infos m
     , MonadMultiState LogState m
     )
  => m ()
uploadPackage = do
  buildtool <- configReadStringM ["setup", "buildtool"]
  pushLog LogLevelPrint "Performing upload.."
  case buildtool of
    "cabal" -> do
      (PackageName pname) <- askPackageName
      pvers <- askPackageVersion
      username <- configReadStringMaybeM ["setup", "hackage", "username"]
      password <- configReadStringMaybeM ["setup", "hackage", "password"]
      let mzeroIfNonzero :: m Turtle.ExitCode -> m ()
          mzeroIfNonzero k = do
            r <- k
            case r of
              Turtle.ExitSuccess   -> return ()
              Turtle.ExitFailure _ -> mzero

      let filePath = "dist/" ++ pname ++ "-" ++ showVersion pvers ++ ".tar.gz"
      mzeroIfNonzero $ liftIO $
        runProcess "cabal" ["sdist"] Nothing Nothing Nothing Nothing Nothing
        >>= waitForProcess
      mzeroIfNonzero $ liftIO $
        runProcess "cabal"
                   ( [ "upload"
                     , filePath
                     ]
                   ++ ["-u" ++ u | u <- maybeToList username]
                   ++ ["-p" ++ p | p <- maybeToList password]
                   )
                   Nothing Nothing Nothing Nothing Nothing
        >>= waitForProcess
      pushLog LogLevelPrint "Upload successful."
    "stack" -> do
      pushLog LogLevelError "TODO: stack upload"
      mzero
    _ -> mzero

uploadDocs
  :: forall m
   . ( MonadIO m
     , MonadPlus m
     , MonadMultiReader Config m
     , MonadMultiReader Infos m
     , MonadMultiState LogState m
     )
  => m ()
uploadDocs = do
  buildtool <- configReadStringM ["setup", "buildtool"]
  pushLog LogLevelPrint "Performing doc upload.."
  case buildtool of
    "cabal" -> do
      username <- configReadStringMaybeM ["setup", "hackage", "username"]
      password <- configReadStringMaybeM ["setup", "hackage", "password"]
      let mzeroIfNonzero :: m Turtle.ExitCode -> m ()
          mzeroIfNonzero k = do
            r <- k
            case r of
              Turtle.ExitSuccess   -> return ()
              Turtle.ExitFailure _ -> mzero
      mzeroIfNonzero $ liftIO $
        runProcess "cabal"
                   ( [ "upload"
                     , "--doc"
                     ]
                   ++ ["-u" ++ u | u <- maybeToList username]
                   ++ ["-p" ++ p | p <- maybeToList password]
                   )
                   Nothing Nothing Nothing Nothing Nothing
        >>= waitForProcess
      pushLog LogLevelPrint "Documentation upload successful."
    "stack" -> do
      pushLog LogLevelError "TODO: stack upload"
      mzero
    _ -> mzero


