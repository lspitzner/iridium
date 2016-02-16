{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}

module Development.Iridium.SecondChecks
  ( compile
  , compileVersions
  , documentation
  , upperBoundsStackage
  )
where



import qualified Data.Text     as Text
import qualified Data.Text.IO  as Text.IO
import qualified Turtle        as Turtle
import qualified Control.Foldl as Foldl

import           Data.Text ( Text )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Control.Monad
import           Distribution.PackageDescription
import           Control.Exception.Lifted
import           Control.Monad.Trans.Control

import           Control.Monad.Trans.MultiRWS

import           Data.Maybe ( maybeToList )
import           Data.ByteString ( ByteString )

import           Data.Text.Encoding

-- well, no Turtle, apparently.
-- no way to retrieve stdout, stderr and exitcode.
-- the most generic case, not supported? psshhh.
import           System.Process hiding ( cwd )
import           Data.List ( nub )
import           Filesystem.Path.CurrentOS hiding ( null )
import qualified Distribution.Package
import           Data.Version ( showVersion )
import           Distribution.Version

import qualified Network.URI            as URI
import qualified Network.HTTP           as HTTP

import           Development.Iridium.Types
import           Development.Iridium.Logging
import           Development.Iridium.Config
import           Development.Iridium.Prompt
import           Development.Iridium.Utils
import           Development.Iridium.CheckState




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
          runCommandSuccess "cabal" ["clean"]
          runCommandSuccess "cabal" $ ["install", "--dep"] ++ testsArg
          runCommandSuccess "cabal" $ ["configure"] ++ testsArg ++ werrorArg
          runCommandSuccess "cabal" ["build"]
          when testsEnabled $
            runCommandSuccess "cabal" ["test"]
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
        runCommandSuccess "cabal" ["clean"]
        runCommandSuccess "cabal" ["install", "--dep"]
        runCommandSuccess "cabal" ["configure"]
        runCommandSuccess "cabal" ["haddock"]
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
            runCommandSuccess "cabal" ["clean"]
            runCommandSuccess "cabal" $ ["install", "--dep", "-w" ++ compilerPath]
                                     ++ testsArg
            runCommandSuccess "cabal" $ ["configure", "-w" ++ compilerPath]
                                     ++ testsArg
                                     ++ werrorArg
            runCommandSuccess "cabal" ["build"]
            when testsEnabled $
              runCommandSuccess "cabal" ["test"]
        "stack" -> do
          pushLog LogLevelError "TODO: stack build"
          mzero
        _ -> mzero
  }

upperBoundsStackage
  :: forall m
   . ( MonadIO m
     , MonadBaseControl IO m
     , MonadPlus m
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
      "cabal" ->
        mzeroToFalse $ do
          cabalConfigPath       <- getLocalFilePath "cabal.config"
          cabalConfigBackupPath <- getLocalFilePath "cabal.config.backup"
          alreadyExists <- Turtle.testfile cabalConfigPath
          let
            act :: MaybeT m () = do
              pushLog LogLevelInfo $ "Preparing cabal.config"
              when alreadyExists $ do
                pushLog LogLevelInfoVerbose $ "Renaming existing cabal.config to cabal.config.backup"
                Turtle.mv cabalConfigPath cabalConfigBackupPath
              useNightly <- configIsTrueM ["checks", "upper-bounds-stackage", "use-nightly"]
              let urlStr = if useNightly
                    then "http://www.stackage.org/nightly/cabal.config"
                    else "http://www.stackage.org/lts/cabal.config"
              cabalConfigContents <- fetchCabalConfig urlStr
              pName <- liftM (Text.pack . (\(Distribution.Package.PackageName n) -> n)) askPackageName
              let filteredLines = filter (pName `Text.isInfixOf`)
                                $ Text.lines
                                $ decodeUtf8 cabalConfigContents
              liftIO $ Text.IO.writeFile (encodeString cabalConfigPath) (Text.unlines filteredLines)
              let testsArg = ["--enable-tests" | testsEnabled]
              runCommandSuccess "cabal" ["clean"]
              runCommandSuccess "cabal" $ ["install", "--dep"] ++ testsArg
            fin = do
              pushLog LogLevelInfo $ "Cleanup (cabal.config)"
              when alreadyExists $ Turtle.mv cabalConfigBackupPath cabalConfigPath
          act `finally` fin
          return True
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
    -> m0 ByteString
  fetchCabalConfig urlStr = do
    pushLog LogLevelInfoVerbose $ "Fetching up-to-date cabal.config from " ++ urlStr
    url <- case URI.parseURI urlStr of
      Nothing -> do
        pushLog LogLevelError "bad URI"
        mzero
      Just u -> return u
    result <- liftIO $ HTTP.simpleHTTP (HTTP.mkRequest HTTP.GET url)
    case result of
      Left _ -> do
        pushLog LogLevelError "Error: Could not retrieve hackage version"
        mzero
      Right x -> return $ HTTP.rspBody x
