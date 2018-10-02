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
import           Control.Exception
import           Data.Version
import           Distribution.Package ( PackageName(..) )
import qualified Turtle                 as Turtle
import           System.Exit

import qualified Network.HTTP           as HTTP
import qualified Network.URI            as URI
import qualified Data.Text              as Text
import qualified Data.Aeson             as Aeson
import qualified Data.HashMap.Strict    as HM
import qualified Text.ParserCombinators.ReadP as ReadP
import           Data.List (find)

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
  => String -> String -> m (Maybe Version)
retrieveLatestVersion remoteUrl pkgName = do
  let urlStr :: String = remoteUrl ++ "/package/" ++ pkgName ++ "/preferred"
  pushLog LogLevelInfo $ "Looking up latest version from hackage via url " ++ urlStr

  uri <- case URI.parseURI urlStr of
    Nothing -> do
      pushLog LogLevelError "bad URI"
      mzero
    Just u -> return u
  let request = HTTP.insertHeader HTTP.HdrAccept "application/json"
        $ HTTP.mkRequest HTTP.GET uri

  rawHtmlE <- liftIO $ HTTP.simpleHTTP request
  let parseError = do
        pushLog LogLevelError "Could not decode hackage response."
        mzero
  case rawHtmlE of
    Left{} -> return Nothing
    Right r -> case Aeson.decode $ HTTP.rspBody r of
      Just m -> case HM.lookup (Text.pack "normal-version") m of
        Nothing -> parseError
        Just [] -> pure Nothing
        Just (vs :: [String]) -> do
          let v :: Version = maximum $ parseVersionF <$> vs
          pure $ Just v
      Nothing -> parseError
 where
  parseVersionF s = case find (null . snd) $ ReadP.readP_to_S parseVersion s of
    Nothing -> error "parseVersionF"
    Just (v, _) -> v


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

      let filePath = "dist/" ++ pname ++ "-" ++ showVersion pvers ++ ".tar.gz"
      mzeroIfNonzero $ liftIO $
        runProcess "cabal" ["sdist"] Nothing Nothing Nothing Nothing Nothing
        >>= waitForProcess
      mzeroIfNonzero $ liftIO $
        runProcess "cabal"
                   ( [ "upload"
                     , "--publish"
                     , filePath
                     ]
                   ++ ["-u" ++ u | u <- maybeToList username]
                   ++ ["-p" ++ p | p <- maybeToList password]
                   )
                   Nothing Nothing Nothing Nothing Nothing
        >>= waitForProcess
      pushLog LogLevelPrint "Upload successful."
    "cabal-new" -> do
      (PackageName pname) <- askPackageName
      pvers <- askPackageVersion
      username <- configReadStringMaybeM ["setup", "hackage", "username"]
      password <- configReadStringMaybeM ["setup", "hackage", "password"]

      let filePath = "dist/" ++ pname ++ "-" ++ showVersion pvers ++ ".tar.gz"
      mzeroIfNonzero $ liftIO $
        runProcess "cabal" ["sdist"] Nothing Nothing Nothing Nothing Nothing
        >>= waitForProcess
      mzeroIfNonzero $ liftIO $
        runProcess "cabal"
                   ( [ "upload"
                     , "--publish"
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
      infoVerbEnabled <- isEnabledLogLevel LogLevelInfoVerbose
      mzeroIfNonzero $ liftIO $
        runProcess "cabal"
                   ( [ "upload"
                     , "--doc"
                     , "--publish"
                     ]
                   ++ ["-u" ++ u | u <- maybeToList username]
                   ++ ["-p" ++ p | p <- maybeToList password]
                   ++ ["-v0" | not infoVerbEnabled]
                   )
                   Nothing Nothing Nothing Nothing Nothing
        >>= waitForProcess
      pushLog LogLevelPrint "Documentation upload successful."
    "stack" -> do
      pushLog LogLevelError "TODO: stack upload"
      mzero
    _ -> error "uploadDocs not supported in cabal-new"
