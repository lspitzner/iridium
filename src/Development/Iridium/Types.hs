{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Development.Iridium.Types
  ( Infos (..)
  , Repo (..)
  , NoRepo (..)
  , Config
  , repoDisplaySummary
  )
where


import           Prelude hiding ( FilePath )

import qualified Data.Yaml           as Yaml
import           Control.Monad.Trans.MultiRWS
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Distribution.PackageDescription
import           Data.Version ( Version(..) )
import           Data.Proxy
import           Data.Tagged
import           Control.Applicative
import           Control.Monad
import           Data.HList.ContainsType

import qualified Filesystem.Path.CurrentOS as Path



-- this is an ugly orphan, i am aware. FIXME!
instance MonadIO m => MonadIO (MultiRWST r w s m) where
  liftIO = lift . liftIO
instance (Functor m, MonadPlus m, Alternative m) => Alternative (MultiRWST r w s m) where
  empty   = lift empty
  x <|> y = MultiRWST $ runMultiRWSTRaw x <|> runMultiRWSTRaw y
instance (MonadPlus m, Monad m) => MonadPlus (MultiRWST r w s m) where
  mzero = lift mzero
  mplus x y = MultiRWST $ mplus (runMultiRWSTRaw x) (runMultiRWSTRaw y)


type Config = Yaml.Value

data Infos = forall repo . Repo repo => Infos
  { i_cwd            :: Path.FilePath
  , i_package        :: GenericPackageDescription
  , i_remote_version :: Maybe Version
  , i_repo           :: Tagged repo (RepoInfo repo)
  }

class Repo a where
  type RepoInfo a :: *
  repo_retrieveInfo   :: ( MonadIO m
                         , MonadPlus m
                         , MonadMultiReader Config m
                         )
                      => m (Tagged a (RepoInfo a))
  repo_displaySummary :: ( MonadIO m
                         , MonadMultiReader Config m
                         )
                      => Tagged a (RepoInfo a)
                      -> m ()
  repo_runChecks      :: ( MonadMultiReader Config m
                         , MonadMultiReader (Tagged a (RepoInfo a)) m
                         , m ~ t (MaybeT IO)
                         )
                      => Proxy a -> m ()

repoDisplaySummary
  :: ( MonadMultiReader Infos m
     , m ~ MultiRWST r w s (MaybeT IO)
     , ContainsType Config r
     )
  => m ()
repoDisplaySummary = do
  Infos _ _ _ taggedInfo <- mAsk
  withMultiReader taggedInfo $ witnessProxy taggedInfo $ repo_runChecks

witnessProxy :: Tagged a b -> (Proxy a -> r) -> r
witnessProxy _ f = f Proxy

newtype NoRepo = NoRepo ()

instance Repo NoRepo where
  type RepoInfo NoRepo = ()
  repo_retrieveInfo     = return $ Tagged ()
  repo_displaySummary _ = return ()
  repo_runChecks      _ = return ()
