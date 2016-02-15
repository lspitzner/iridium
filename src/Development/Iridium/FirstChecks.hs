{-# LANGUAGE TypeFamilies #-}

module Development.Iridium.FirstChecks
  ( packageCheck
  , hlint
  , changelog
  , upperBounds
  )
where



import qualified Data.Text     as Text
import qualified Turtle        as Turtle
import qualified Control.Foldl as Foldl

import           Data.Text ( Text )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Control.Monad
import           Distribution.PackageDescription

import           Control.Monad.Trans.MultiRWS

import           Data.Maybe ( maybeToList )

-- well, fuck Turtle, apparently.
-- no way to retrieve stdout, stderr and exitcode.
-- the most generic case, not supported? psshhh.
import           System.Process hiding ( cwd )
import           Data.List ( nub )
import           Filesystem.Path.CurrentOS hiding ( null )
import qualified Distribution.Package
import           Data.Version ( showVersion )
import           Distribution.Version

import           Development.Iridium.Types
import           Development.Iridium.Logging
import           Development.Iridium.Config
import           Development.Iridium.Prompt
import           Development.Iridium.Utils




packageCheck
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiReader Config m
     , MonadMultiState LogState m
     )
  => m ()
packageCheck = do
  buildtool <- configReadStringM ["setup", "buildtool"]
  case buildtool of
    "cabal" -> ignoreBool $ falseToConfirm $ runCheck "Checking package validity" $ do
      (exitCode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode
        "cabal"
        ["check"]
        ""
      case exitCode of
        Turtle.ExitSuccess -> return True
        Turtle.ExitFailure _ -> do
          lines stdOut `forM_` \l ->
            putLog LogLevelPrint $ l
          lines stdErr `forM_` \l ->
            putLog LogLevelPrint $ l
          return False  
    "stack" -> do
      -- stack has no "check".
      -- and no "upload --dry-run either."
      putLog LogLevelWarn "stack has no `check` command!"
      putLog LogLevelWarn "package validity could not be determined."
      return ()
    _ -> error "bad config setup.buildtool"

hlint
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiReader Infos m
     )
  => m ()
hlint = ignoreBool
      $ falseToConfirm
      $ runCheck "Running hlint on hsSourceDirs"
      $ do
  buildInfos <- askAllBuildInfo
  -- putLog LogLevelDebug $ show buildInfos
  let sourceDirs = nub $ buildInfos >>= hsSourceDirs
  putLog LogLevelInfoVerboser $ "hsSourceDirs: " ++ show sourceDirs
  liftM and $ sourceDirs `forM` \path -> do
    (exitCode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode
      "hlint"
      [path]
      ""
    case exitCode of
      Turtle.ExitSuccess -> return True
      Turtle.ExitFailure _ -> do
        lines stdOut `forM_` \l ->
          putLog LogLevelPrint $ l
        lines stdErr `forM_` \l ->
          putLog LogLevelPrint $ l
        putLog LogLevelInfoVerbose $ " `hlint " ++ path ++ "` failed."
        return False  

changelog
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiReader Infos m
     , MonadMultiReader Config m
     )
  => m ()
changelog = ignoreBool
          $ falseToConfirm
          $ runCheck "Testing if the changelog mentions the latest version"
          $ do
  pathRaw <- configReadStringM ["checks", "changelog", "location"]
  Infos cwd pdesc _ _ <- mAsk
  let path = cwd </> decodeString pathRaw
  exists <- Turtle.testfile path
  if (not exists)
    then do
      putLog LogLevelPrint $ "changelog file (" ++ show path ++ ") does not exist!"
      return False
    else do
      changelogContentLines <- Turtle.fold (Turtle.input path) Foldl.list
      let currentVersionStr :: String
            = showVersion
            $ Distribution.Package.pkgVersion
            $ package
            $ packageDescription pdesc
      if any (Text.pack currentVersionStr `Text.isInfixOf`) changelogContentLines
        then return True
        else do
          putLog LogLevelError $ "changelog does not contain " ++ currentVersionStr
          return False

upperBounds
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiReader Config m
     , MonadMultiReader Infos m
     )
  => m ()
upperBounds = ignoreBool
            $ falseToConfirm
            $ runCheck "Checking that all dependencies have upper bounds"
            $ do
  buildInfos <- askAllBuildInfo
  pName <- askPackageName
  let missingUpperBounds
        = [ name
          | info <- buildInfos
          , Distribution.Package.Dependency name range <- targetBuildDepends info
          , name /= pName -- ignore dependencies on the package's library
          , let intervals = asVersionIntervals range
          , case intervals of
              []                  -> True
              [(_, NoUpperBound)] -> True
              _                   -> False
          ]
  if null missingUpperBounds
    then return True
    else do
      putLog LogLevelError $ "Found dependencies without upper bounds:"
      missingUpperBounds `forM_` \(Distribution.Package.PackageName n) ->
        putLog LogLevelError $ "    " ++ n
      return False
