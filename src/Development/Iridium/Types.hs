{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.Iridium.Types
  ( Infos (..)
  , Repo (..)
  , NoRepo (..)
  , LogLevel (..)
  , LogState (..)
  , Config
  , repoRunChecks
  , repoDisplaySummary
  , repoActionSummary
  , repoPerformAction
  , CheckState (..)
  )
where


import           Prelude hiding ( FilePath )

import qualified Data.Yaml           as Yaml
import           Control.Monad.Trans.MultiRWS
import           Control.Monad.Trans.MultiState
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           Data.HList.HList
import           Control.Monad.IO.Class
import           Distribution.PackageDescription
import           Data.Version ( Version(..) )
import           Data.Proxy
import           Data.Tagged
import           Control.Applicative
import           Control.Monad
import           Data.HList.ContainsType
import           Control.Monad.Trans.Control
import           Control.Monad.Base

import qualified Filesystem.Path.CurrentOS as Path



-- these are ugly orphans, i am aware. FIXME!
instance MonadIO m => MonadIO (MultiRWST r w s m) where
  liftIO = lift . liftIO
instance MonadIO m => MonadIO (MultiStateT s m) where
  liftIO = lift . liftIO
instance (Functor m, MonadPlus m, Alternative m) => Alternative (MultiRWST r w s m) where
  empty   = lift empty
  x <|> y = MultiRWST $ runMultiRWSTRaw x <|> runMultiRWSTRaw y
instance (MonadPlus m, Monad m) => MonadPlus (MultiRWST r w s m) where
  mzero = lift mzero
  mplus x y = MultiRWST $ mplus (runMultiRWSTRaw x) (runMultiRWSTRaw y)
instance MonadBase b m => MonadBase b (MultiRWST r w s m) where
  liftBase = lift . liftBase
instance MonadTransControl (MultiRWST r w s) where
  type StT (MultiRWST r w s) a = (a, (HList r, HList w, HList s))
  liftWith f = MultiRWST $ liftWith $ \s -> f $ \r -> s $ runMultiRWSTRaw r
  restoreT = MultiRWST . restoreT

instance MonadBaseControl b m => MonadBaseControl b (MultiRWST r w s m) where
  type StM (MultiRWST r w s m) a = ComposeSt (MultiRWST r w s) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM


data LogLevel = LogLevelSilent
              | LogLevelPrint -- like manual output; should never be filtered
              | LogLevelDebug
              | LogLevelTrace
              | LogLevelWarn
              | LogLevelError
              | LogLevelInfo
              | LogLevelInfoVerbose
              | LogLevelInfoVerboser
              | LogLevelInfoSpam
              | LogLevelThread
  deriving (Show, Eq)

data LogState = LogState
  { _log_mask     :: [LogLevel]
  , _log_indent   :: Int
  , _log_prepared :: Maybe String
  , _log_cur      :: String
  }

type Config = Yaml.Value

data Infos = forall repo . Repo repo => Infos
  { _i_cwd            :: Path.FilePath
  , _i_package        :: GenericPackageDescription
  , _i_remote_version :: Maybe Version
  , _i_repo           :: repo
  }

data CheckState = CheckState
  { _check_stack :: [String]
  , _check_errorCount :: Int
  , _check_warningCount :: Int
  , _check_notWallClean :: [String]
  }

class Repo a where
  -- | the action to retrieve/collect all the
  --   data relevant for the later steps.
  repo_retrieveInfo   :: ( MonadIO m
                         , MonadPlus m
                         , MonadMultiReader Config m
                         , MonadMultiState LogState m
                         )
                      => m a
  -- | The checks to be run for this repo type
  repo_runChecks      :: ( MonadIO m
                         , MonadPlus m
                         , MonadMultiReader Config m
                         , MonadMultiState LogState m
                         , MonadMultiState CheckState m
                         )
                      => a -> m ()
  -- | Summary of repository-type-specific information
  --   to display to the user, e.g. "current branch: .."
  repo_displaySummary :: ( MonadIO m
                         , MonadMultiReader Config m
                         , MonadMultiState LogState m
                         )
                      => a -> m ()
  -- | (Configured) (repository-type-specific) actions
  --   that will be taken, e.g. "Tag the current commit"
  repo_ActionSummary  :: ( MonadMultiReader Config m
                         , MonadMultiReader Infos m
                         , MonadMultiState LogState m
                         )
                      => a -> m [String]
  -- | Perform repository-type-specific real side-effects.
  --   This is post-confirmation by the user, but before
  --   doing hackage upload.
  repo_performAction  :: ( MonadIO m
                         , MonadPlus m
                         , MonadMultiReader Config m
                         , MonadMultiReader Infos m
                         , MonadMultiState LogState m
                         )
                      => a -> m ()

repoRunChecks
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiReader Infos m
     , MonadMultiReader Config m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     )
  => m ()
repoRunChecks = do
  Infos _ _ _ repo <- mAsk
  repo_runChecks repo

repoDisplaySummary
  :: ( MonadIO m
     , MonadMultiReader Infos m
     , MonadMultiReader Config m
     , MonadMultiState LogState m
     )
  => m ()
repoDisplaySummary = do
  Infos _ _ _ repo <- mAsk
  repo_displaySummary repo

repoActionSummary
  :: ( MonadIO m
     , MonadMultiReader Infos m
     , MonadMultiReader Config m
     , MonadMultiState LogState m
     )
  => m [String]
repoActionSummary = do
  Infos _ _ _ repo <- mAsk
  repo_ActionSummary repo

repoPerformAction
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiReader Infos m
     , MonadMultiReader Config m
     , MonadMultiState LogState m
     )
  => m ()
repoPerformAction = do
  Infos _ _ _ repo <- mAsk
  repo_performAction repo

-- witnessProxy :: Tagged a b -> (Proxy a -> r) -> r
-- witnessProxy _ f = f Proxy

data NoRepo = NoRepo

instance Repo NoRepo where
  repo_retrieveInfo     = return $ NoRepo
  repo_runChecks      _ = return ()
  repo_displaySummary _ = return ()
  repo_ActionSummary  _ = return []
  repo_performAction  _ = return ()
