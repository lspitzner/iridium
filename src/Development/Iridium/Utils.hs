{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Development.Iridium.Utils
  ( askAllBuildInfo
  , askPackageName
  , askPackageVersion
  , runCommandSuccess
  , mzeroToFalse
  , runCheck
  , fallbackCheck
  , falseToConfirm
  , falseToAbort
  , ignoreBool
  )
where


import           Prelude hiding ( FilePath )

import qualified Data.Text           as Text
import qualified Turtle              as Turtle
import qualified Control.Foldl       as Foldl

import qualified Data.Yaml           as Yaml
import           Control.Monad.Trans.MultiRWS
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Distribution.PackageDescription
import           Distribution.Package
import           Data.Version ( Version(..) )
import           Data.Proxy
import           Data.Tagged
import           Control.Applicative
import           Control.Monad
import           Data.Functor

-- well, no Turtle, apparently.
-- no way to retrieve stdout, stderr and exitcode.
-- the most generic case, not supported? psshhh.
import           System.Process hiding ( cwd )

import           Data.Maybe ( maybeToList )

import qualified Filesystem.Path.CurrentOS as Path

import           Development.Iridium.Types
import           Development.Iridium.Logging
import           Development.Iridium.Prompt



runCheck
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     )
  => String
  -> m Bool
  -> m Bool
runCheck s m = do
  putLog LogLevelPrint $ s ++ ":"
  r <- withIndentation m
  if r
    then do
      withoutIndentation $ putLog LogLevelPrint $ ".." ++ replicate 70 ' ' ++ "clear."
      return True
    else do
      withoutIndentation $ putLog LogLevelPrint $ ".." ++ replicate 70 ' ' ++ "failed."
      putLog LogLevelPrint $ "(Latest: " ++ s ++ ")"
      return False

askAllBuildInfo :: (MonadMultiReader Infos m) => m [BuildInfo]
askAllBuildInfo = do
  Infos _ pDesc _ _ <- mAsk
  return $ (libBuildInfo       . condTreeData <$> maybeToList (condLibrary pDesc))
        ++ (buildInfo          . condTreeData . snd <$> condExecutables pDesc)
        ++ (testBuildInfo      . condTreeData . snd <$> condTestSuites pDesc)
        ++ (benchmarkBuildInfo . condTreeData . snd <$> condBenchmarks pDesc)  

askPackageName :: MonadMultiReader Infos m => m PackageName
askPackageName = do
  Infos _ pDesc _ _ <- mAsk
  return $ pkgName $ package $ packageDescription pDesc

askPackageVersion :: MonadMultiReader Infos m => m Version
askPackageVersion = do
  Infos _ pDesc _ _ <- mAsk
  return $ pkgVersion $ package $ packageDescription pDesc

runCommandSuccess
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     )
  => String
  -> [String]
  -> m ()
runCommandSuccess c ps = do
  putLog LogLevelInfo $ "Calling " ++ show c ++ " " ++ show ps
  (exitCode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode c ps ""
  case exitCode of
    Turtle.ExitSuccess -> return ()
    Turtle.ExitFailure _ -> do
      lines stdOut `forM_` \l ->
        putLog LogLevelPrint $ "  " ++ l
      lines stdErr `forM_` \l ->
        putLog LogLevelPrint $ "  " ++ l
      mzero

mzeroToFalse :: MonadPlus m => m a -> m Bool
mzeroToFalse m = liftM (const True) m `mplus` return False

falseToConfirm
  :: (MonadMultiState LogState m, MonadPlus m, MonadIO m) => m Bool -> m Bool
falseToConfirm m = m >>= \x -> if x
  then return True
  else askConfirmationOrMZero >> return False

falseToAbort :: MonadPlus m => m Bool -> m Bool
falseToAbort m = m >>= guard >> return True

fallbackCheck :: Monad m => m Bool -> m Bool -> m Bool
fallbackCheck m1 m2 = do
  x <- m1
  if x
    then return True
    else m2

ignoreBool :: Monad m => m Bool -> m ()
ignoreBool = liftM (const ())
