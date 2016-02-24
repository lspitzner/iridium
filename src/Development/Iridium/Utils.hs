{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Development.Iridium.Utils
  ( askAllBuildInfo
  , askPackageName
  , askPackageVersion
  , mzeroToFalse
  , falseToMZero
  , runCheck
  , fallbackCheck
  -- , falseToConfirm
  , falseToAbort
  , ignoreBool
  , boolToWarning
  , boolToError
  , getLocalFilePath
  , mzeroIfNonzero
  )
where



#include "qprelude/bundle-gamma.inc"

import qualified Turtle              as Turtle
import qualified Control.Exception   as C

import qualified Data.Yaml           as Yaml
import           Distribution.PackageDescription
import           Distribution.Package
import           Filesystem.Path.CurrentOS hiding ( null )
import           Data.Version ( Version(..) )
import           System.Exit
import           System.IO.Error
import           GHC.IO.Exception ( ioException, IOErrorType(..), IOException(..) )

-- well, no Turtle, apparently.
-- no way to retrieve stdout, stderr and exitcode.
-- the most generic case, not supported? psshhh.
import           System.Process hiding ( cwd )

import qualified Filesystem.Path.CurrentOS as Path

import           Development.Iridium.Types
import           Development.Iridium.UI.Console
import           Development.Iridium.UI.Prompt
import           Development.Iridium.CheckState
import           Development.Iridium.Config



runCheck
  :: ( MonadIO m
     , MonadMultiState LogState m
     )
  => String
  -> m Bool
  -> m Bool
runCheck s m = do
  pushLogPrepare $ s ++ ":"
  writeCurLine $ s ++ ":"
  r <- withIndentation m
  if r
    then do
      pushLogFinalize 70 "clear."
      return True
    else do
      pushLogFinalize 70 "failed."
      pushLog LogLevelPrint $ "(Latest: " ++ s ++ ")"
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

mzeroToFalse :: Monad m => MaybeT m a -> m Bool
mzeroToFalse m = do
  x <- runMaybeT m
  case x of
    Nothing -> return False
    Just _  -> return True

falseToMZero :: MonadPlus m => m Bool -> m ()
falseToMZero m = m >>= guard

-- mzeroToFalse :: MonadPlus m => m a -> m Bool
-- mzeroToFalse m = liftM (const True) m `mplus` return False

-- falseToConfirm
--   :: (MonadMultiState LogState m, MonadPlus m, MonadIO m) => m Bool -> m Bool
-- falseToConfirm m = m >>= \x -> if x
--   then return True
--   else askConfirmationOrMZero >> return False

falseToAbort :: MonadPlus m => m Bool -> m Bool
falseToAbort m = m >>= guard >> return True

fallbackCheck :: Monad m => m Bool -> m Bool -> m Bool
fallbackCheck m1 m2 = do
  x <- m1
  if x
    then return True
    else m2

mzeroIfNonzero
  :: ( MonadPlus m )
  => m ExitCode
  -> m ()
mzeroIfNonzero k = do
  r <- k
  case r of
    ExitSuccess   -> return ()
    ExitFailure _ -> mzero

ignoreBool :: Monad m => m Bool -> m ()
ignoreBool = liftM (const ())

boolToWarning
  :: ( MonadMultiState CheckState m )
  => m Bool
  -> m ()
boolToWarning m = do
  b <- m
  unless b incWarningCounter


boolToError
  :: ( MonadMultiState CheckState m )
  => m Bool
  -> m ()
boolToError m = do
  b <- m
  unless b incErrorCounter


getLocalFilePath
  :: ( MonadMultiReader Infos m )
  => String
  -> m Turtle.FilePath
getLocalFilePath s = do
  infos <- mAsk
  return $ _i_cwd infos </> decodeString s
