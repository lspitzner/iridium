{-# LANGUAGE TypeFamilies #-}

module Development.Iridium.Checks
  ( packageCheck
  , hlint
  , changelog
  , upperBounds
  , remoteVersion
  , compile
  , documentation
  , compileVersions
  , upperBoundsStackage
  )
where



#include "qprelude/bundle-gamma.inc"

import qualified Turtle               as Turtle
import qualified Control.Foldl        as Foldl
import qualified Network.HTTP.Conduit as HTTP

import           Control.Monad.Trans.Control

import           Distribution.PackageDescription
import           Filesystem.Path.CurrentOS hiding ( null )

import qualified Distribution.Package
import           Distribution.Version

import           System.Process hiding ( cwd )

import           Development.Iridium.CheckState
import           Development.Iridium.Config
import           Development.Iridium.Types
import           Development.Iridium.UI.Console
import           Development.Iridium.UI.Prompt
import           Development.Iridium.Utils
import           Development.Iridium.ExternalProgWrappers



packageCheck
  :: ( MonadIO m
     , MonadMultiReader Config m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     )
  => m ()
packageCheck = do
  buildtool <- configReadStringM ["setup", "buildtool"]
  case buildtool of
    "cabal" -> boolToError $ runCheck "Checking package validity" $ do
      mzeroToFalse $
        runCommandSuccessCabal ["check"]
    "stack" -> do
      -- stack has no "check".
      -- and no "upload --dry-run either."
      pushLog LogLevelWarn "stack has no `check` command!"
      pushLog LogLevelWarn "package validity could not be determined."
      return ()
    _ -> error "bad config setup.buildtool"

hlint
  :: ( MonadIO m
     , MonadMultiReader Config m
     , MonadMultiReader Infos m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     )
  => m ()
hlint = boolToWarning
      $ runCheck "Running hlint on hsSourceDirs"
      $ do
  buildInfos <- askAllBuildInfo
  -- pushLog LogLevelDebug $ show buildInfos
  let sourceDirs = nub $ buildInfos >>= hsSourceDirs
  pushLog LogLevelInfoVerboser $ "hsSourceDirs: " ++ show sourceDirs
  liftM and $ sourceDirs `forM` \path -> do
    mzeroToFalse $
      runCommandSuccessHLint [path]

changelog
  :: ( MonadIO m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     , MonadMultiReader Infos m
     , MonadMultiReader Config m
     )
  => m ()
changelog = boolToWarning
          $ runCheck "Testing if the changelog mentions the latest version"
          $ do
  pathRaw <- configReadStringM ["checks", "changelog", "location"]
  Infos cwd pdesc _ _ <- mAsk
  let path = cwd </> decodeString pathRaw
  exists <- Turtle.testfile path
  if (not exists)
    then do
      pushLog LogLevelPrint $ "changelog file (" ++ show path ++ ") does not exist!"
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
          pushLog LogLevelError $ "changelog does not contain " ++ currentVersionStr
          return False

upperBounds
  :: ( MonadIO m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     , MonadMultiReader Config m
     , MonadMultiReader Infos m
     )
  => m ()
upperBounds = boolToWarning
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
      pushLog LogLevelError $ "Found dependencies without upper bounds:"
      missingUpperBounds `forM_` \(Distribution.Package.PackageName n) ->
        pushLog LogLevelError $ "    " ++ n
      return False

remoteVersion
  :: ( MonadIO m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     , MonadMultiReader Infos m
     )
  => m ()
remoteVersion = boolToError
              $ runCheck "Comparing local version to hackage version"
              $ do
  infos <- mAsk
  localVersion <- askPackageVersion
  -- pushLog LogLevelDebug $ show $ _i_remote_version infos
  case _i_remote_version infos of
    Nothing -> return True
    Just remoteVers ->
      if localVersion == remoteVers
        then do
          pushLog LogLevelError $ "This package version (" ++ showVersion localVersion ++ ") is already on hackage; needs bump?"
          return False
        else if localVersion < remoteVers
          then do
            pushLog LogLevelWarn $ "The version on hackage ("
                                ++ showVersion remoteVers
                                ++ ") is greater than the local version ("
                                ++ showVersion localVersion
                                ++ ")."
            return False
          else return True

compile
  :: forall m
   . ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     , MonadMultiReader Config m
     )
  => m ()
compile = withStack "basic compilation" $ boolToError $ do

  warningsEnabled <- configIsEnabledM ["checks", "compiler-warnings"]
  if warningsEnabled
    then fallbackCheck
      (do
        b <- runCheck "Checking basic compilation" (checks True)
        unless b $ do
          incWarningCounter
          addNotWallClean "<default>"
        return b
      )
      (do
        pushLog LogLevelPrint "Falling back on compilation without warnings."
        runCheck "Checking basic compilation -w" (checks False)
      )
    else
      runCheck "Checking basic compilation" (checks False)

 where
  checks :: Bool -> m Bool
  checks werror = do
    buildtool <- configReadStringM ["setup", "buildtool"]
    testsEnabled <- configIsEnabledM ["checks", "testsuites"]
    case buildtool of
      "cabal" ->
        mzeroToFalse $ do
          let testsArg = ["--enable-tests" | testsEnabled]
          let werrorArg = ["--ghc-options=\"-Werror\"" | werror]
                       ++ ["--ghc-options=\"-w\"" | not werror]
          runCommandSuccessCabal ["clean"]
          runCommandSuccessCabal $ ["install", "--dep"] ++ testsArg
          runCommandSuccessCabal $ ["configure"] ++ testsArg ++ werrorArg
          runCommandSuccessCabal ["build"]
          when testsEnabled $
            runCommandSuccessCabal ["test"]
          return True
      "stack" -> do
        pushLog LogLevelError "TODO: stack build"
        mzero
      _ -> mzero

documentation
  :: ( MonadIO m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     , MonadMultiReader Config m
     )
  => m ()
documentation = boolToError
              $ runCheck "Checking documentation"
              $ withStack "documentation check"
              $ do
  buildtool <- configReadStringM ["setup", "buildtool"]
  case buildtool of
    "cabal" ->
      mzeroToFalse $ do
        runCommandSuccessCabal ["clean"]
        runCommandSuccessCabal ["install", "--dep"]
        runCommandSuccessCabal ["configure"]
        runCommandSuccessCabal ["haddock"]
    "stack" -> do
      pushLog LogLevelError "TODO: stack build"
      return False
    _ -> error "lkajsdlkjasd"

compileVersions
  :: forall m
   . ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     , MonadMultiReader Config m
     )
  => m ()
compileVersions = withStack "compiler checks" $ do

  buildtool       <- configReadStringM ["setup", "buildtool"]
  testsEnabled    <- configIsEnabledM ["checks", "testsuites"]
  warningsEnabled <- configIsEnabledM ["checks", "compiler-warnings"]

  case () of {
    () -> do
      if testsEnabled
        then pushLog LogLevelPrint "Checking compilation and tests with different compiler versions"
        else pushLog LogLevelPrint "Checking compilation with different compiler versions"
      withIndentation $ do
        rawList <- configReadListM ["checks", "compiler-versions", "compilers"]
        rawList `forM_` \val -> boolToError $ do
          let compilerStr = configReadString ["compiler"] val
                         ++ "-"
                         ++ configReadString ["version"] val
          let checkBaseName = "Checking with compiler " ++ compilerStr
          withStack compilerStr $
            if warningsEnabled
              then
                fallbackCheck
                  (do
                    b <- runCheck checkBaseName $ checks compilerStr True
                    unless b $ do
                      incWarningCounter
                      addNotWallClean compilerStr
                    return b
                  )
                  (do
                    pushLog LogLevelPrint "Falling back on compilation without warnings."
                    runCheck (checkBaseName ++ " -w") $ checks compilerStr False
                  )
              else
                runCheck checkBaseName $ checks compilerStr False

    where
      checks :: String -> Bool -> m Bool
      checks compilerStr werror = case buildtool of
        "cabal" -> do
          let confList = ["setup", "compiler-paths", compilerStr]
          compilerPathMaybe <- configReadStringMaybeM confList
          compilerPath <- case compilerPathMaybe of
            Nothing -> do
              pushLog LogLevelError $ "Expected string in config for " ++ show confList
              mzero
            Just x -> return x
          mzeroToFalse $ do
            let testsArg = ["--enable-tests" | testsEnabled]
            let werrorArg = ["--ghc-options=\"-Werror\"" | werror]
                         ++ ["--ghc-options=\"-w\"" | not werror]
            runCommandSuccessCabal ["clean"]
            runCommandSuccessCabal $ ["install", "--dep", "-w" ++ compilerPath]
                                     ++ testsArg
            runCommandSuccessCabal $ ["configure", "-w" ++ compilerPath]
                                     ++ testsArg
                                     ++ werrorArg
            runCommandSuccessCabal ["build"]
            when testsEnabled $
              runCommandSuccessCabal ["test"]
        "stack" -> do
          pushLog LogLevelError "TODO: stack build"
          mzero
        _ -> mzero
  }

upperBoundsStackage
  :: forall m
   . ( MonadIO m
     , MonadPlus m
     , MonadBaseControl IO m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     , MonadMultiReader Infos m
     , MonadMultiReader Config m
     )
  => m ()
upperBoundsStackage = withStack "stackage upper bound" $ boolToError $ do

  runCheck "Checking upper bounds using stackage" $ do
    buildtool    <- configReadStringM ["setup", "buildtool"]
    testsEnabled <- configIsEnabledM  ["checks", "testsuites"]
    case buildtool of
      "cabal" -> do
          cabalConfigPath       <- getLocalFilePath "cabal.config"
          cabalConfigBackupPath <- getLocalFilePath "cabal.config.backup"
          alreadyExists <- Turtle.testfile cabalConfigPath
          -- TODO: make this safe against ctrl-c again.
          -- TODO: make sure the backup does not exist yet. (!)
          pushLog LogLevelInfo $ "Preparing cabal.config"
          when alreadyExists $ do
            pushLog LogLevelInfoVerbose $ "Renaming existing cabal.config to cabal.config.backup"
            Turtle.mv cabalConfigPath cabalConfigBackupPath
          result <- mzeroToFalse $ do
            useNightly <- configIsTrueM ["checks", "upper-bounds-stackage", "use-nightly"]
            let urlStr = if useNightly
                  then "http://www.stackage.org/nightly/cabal.config"
                  else "http://www.stackage.org/lts/cabal.config"
            cabalConfigContents <- fetchCabalConfig urlStr
            pName <- liftM (Text.pack . (\(Distribution.Package.PackageName n) -> n)) askPackageName
            let filteredLines = filter (not . (pName `Text.isInfixOf`))
                              $ Text.lines
                              $ Text.Encoding.decodeUtf8 cabalConfigContents
            -- pushLog LogLevelDebug $ "Writing n lines to cabal.config: " ++ show (length filteredLines)
            liftIO $ Text.IO.writeFile (encodeString cabalConfigPath) (Text.unlines filteredLines)
            let testsArg = ["--enable-tests" | testsEnabled]
            runCommandSuccessCabal ["clean"]
            runCommandSuccessCabal $ ["install", "--dep", "--force-reinstalls", "--dry-run"] ++ testsArg
          pushLog LogLevelInfo $ "Cleanup (cabal.config)"
          unless alreadyExists $ Turtle.rm cabalConfigPath
          when alreadyExists $ Turtle.mv cabalConfigBackupPath cabalConfigPath
          -- let
          --   act :: MaybeT m () = do
          --     pushLog LogLevelInfo $ "Preparing cabal.config"
          --     when alreadyExists $ do
          --       pushLog LogLevelInfoVerbose $ "Renaming existing cabal.config to cabal.config.backup"
          --       Turtle.mv cabalConfigPath cabalConfigBackupPath
          --     useNightly <- configIsTrueM ["checks", "upper-bounds-stackage", "use-nightly"]
          --     let urlStr = if useNightly
          --           then "http://www.stackage.org/nightly/cabal.config"
          --           else "http://www.stackage.org/lts/cabal.config"
          --     cabalConfigContents <- fetchCabalConfig urlStr
          --     pName <- liftM (Text.pack . (\(Distribution.Package.PackageName n) -> n)) askPackageName
          --     let filteredLines = filter (pName `Text.isInfixOf`)
          --                       $ Text.lines
          --                       $ decodeUtf8 cabalConfigContents
          --     liftIO $ Text.IO.writeFile (encodeString cabalConfigPath) (Text.unlines filteredLines)
          --     let testsArg = ["--enable-tests" | testsEnabled]
          --     runCommandSuccessCabal ["clean"]
          --     runCommandSuccessCabal $ ["install", "--dep"] ++ testsArg
          --   fin = do
          --     pushLog LogLevelInfo $ "Cleanup (cabal.config)"
          --     when alreadyExists $ Turtle.mv cabalConfigBackupPath cabalConfigPath
          -- act `finally` fin
          return result
      "stack" -> do
        pushLog LogLevelError "TODO: stack upper bound check"
        mzero
      _ -> mzero
 
 where
  fetchCabalConfig
    :: forall m0
     . ( MonadIO m0
       , MonadPlus m0
       , MonadMultiState LogState m0
       , MonadMultiReader Infos m0
       )
    => String
    -> m0 ByteString.ByteString
  fetchCabalConfig urlStr = do
    pushLog LogLevelInfoVerbose $ "Fetching up-to-date cabal.config from " ++ urlStr
    -- TODO: exception handling
    r <- HTTP.simpleHttp urlStr
    return $ ByteString.concat $ Data.ByteString.Lazy.toChunks $ r
    -- url <- case URI.parseURI urlStr of
    --   Nothing -> do
    --     pushLog LogLevelError "bad URI"
    --     mzero
    --   Just u -> return u
    -- result <- liftIO $ HTTP.simpleHTTP (HTTP.mkRequest HTTP.GET url)
    -- case result of
    --   Left _ -> do
    --     pushLog LogLevelError "Error: Could not retrieve hackage version"
    --     mzero
    --   Right x -> do
    --     pushLog LogLevelInfoVerboser $ show x
    --     let body = HTTP.rspBody x
    --     pushLog LogLevelInfoVerbose $ "Retrieved " ++ show (ByteString.length body) ++ " bytes."
    --     return $ body
