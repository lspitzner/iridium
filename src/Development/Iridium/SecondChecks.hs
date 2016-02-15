{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}

module Development.Iridium.SecondChecks
  ( compile
  , compileVersions
  , documentation
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

-- well, no Turtle, apparently.
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




compile
  :: forall m
   . ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiReader Config m
     )
  => m ()
compile = ignoreBool $ do
  fallbackCheck
    (falseToConfirm $ runCheck "Checking basic compilation" (checks True))
    (do
      putLog LogLevelPrint "Falling back on compilation without warnings."
      falseToAbort $ runCheck "Checking basic compilation -w" (checks False)
    )

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
          runCommandSuccess "cabal" ["clean"]
          runCommandSuccess "cabal" $ ["install", "--dep"] ++ testsArg
          runCommandSuccess "cabal" $ ["configure"] ++ testsArg ++ werrorArg
          runCommandSuccess "cabal" ["build"]
          when testsEnabled $
            runCommandSuccess "cabal" ["test"]
      "stack" -> do
        putLog LogLevelError "TODO: stack build"
        mzero
      _ -> mzero

documentation
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiReader Config m
     )
  => m ()
documentation = ignoreBool
              $ falseToConfirm
              $ runCheck "Checking documentation"
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
      putLog LogLevelError "TODO: stack build"
      mzero
    _ -> mzero

compileVersions
  :: forall m
   . ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiReader Config m
     )
  => m ()
compileVersions = do
  buildtool    <- configReadStringM ["setup", "buildtool"]
  testsEnabled <- configIsEnabledM ["checks", "testsuites"]
  case () of {
    () -> do
      if testsEnabled
        then putLog LogLevelPrint "Checking compilation and tests with different compiler versions"
        else putLog LogLevelPrint "Checking compilation with different compiler versions"
      withIndentation $ do
        rawList <- configReadListM ["checks", "compiler-versions", "compilers"]
        rawList `forM_` \val -> ignoreBool $ do
          let compilerStr = configReadString ["compiler"] val
                         ++ "-"
                         ++ configReadString ["version"] val
          let checkBaseName = "Checking with compiler " ++ compilerStr
          fallbackCheck
            (falseToConfirm $ runCheck checkBaseName $ checks compilerStr True)
            (do
              putLog LogLevelPrint "Falling back on compilation without warnings."
              falseToAbort $ runCheck (checkBaseName ++ " -w") $ checks compilerStr False
            )
    where
      checks :: String -> Bool -> m Bool
      checks compilerStr werror = case buildtool of
        "cabal" -> do
          let confList = ["setup", "compiler-paths", compilerStr]
          compilerPathMaybe <- configReadStringMaybeM confList
          compilerPath <- case compilerPathMaybe of
            Nothing -> do
              putLog LogLevelError $ "Expected string in config for " ++ show confList
              mzero
            Just x -> return x
          (if werror then mzeroToFalse else liftM (const True)) $ do
            let testsArg = ["--enable-tests" | testsEnabled]
            let werrorArg = ["--ghc-options=\"-Werror\"" | werror]
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
          putLog LogLevelError "TODO: stack build"
          mzero
        _ -> mzero
  }
