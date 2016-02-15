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
  fallbackCheck
    (do
      b <- runCheck "Checking basic compilation" (checks True)
      unless b $ do
        incWarningCounter
        addNotWallClean "<default>"
      return b
    )
    (do
      pushLog LogLevelPrint "Falling back on compilation without warnings."
      b <- runCheck "Checking basic compilation -w" (checks False)
      unless b $ incErrorCounter
      return b
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
  buildtool    <- configReadStringM ["setup", "buildtool"]
  testsEnabled <- configIsEnabledM ["checks", "testsuites"]
  case () of {
    () -> do
      if testsEnabled
        then pushLog LogLevelPrint "Checking compilation and tests with different compiler versions"
        else pushLog LogLevelPrint "Checking compilation with different compiler versions"
      withIndentation $ do
        rawList <- configReadListM ["checks", "compiler-versions", "compilers"]
        rawList `forM_` \val -> do
          let compilerStr = configReadString ["compiler"] val
                         ++ "-"
                         ++ configReadString ["version"] val
          let checkBaseName = "Checking with compiler " ++ compilerStr
          withStack compilerStr $
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
                do
                  b <- runCheck (checkBaseName ++ " -w") $ checks compilerStr False
                  unless b $ do
                    incErrorCounter
                  return b
              )
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
