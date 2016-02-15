{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Development.Iridium.CheckState
  ( initCheckState
  , withStack
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
import           Data.List

-- well, no Turtle, apparently.
-- no way to retrieve stdout, stderr and exitcode.
-- the most generic case, not supported? psshhh.
import           System.Process hiding ( cwd )

import           Data.Maybe ( maybeToList )

import qualified Filesystem.Path.CurrentOS as Path

import           Development.Iridium.Types
import           Development.Iridium.Logging
import           Development.Iridium.Prompt



initCheckState :: CheckState
initCheckState = CheckState []

withStack
  :: ( MonadIO m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     )
  => String
  -> m a
  -> m a
withStack s m = do
  s1 <- mGet
  let newStack = s : check_stack s1
  mSet $ s1 { check_stack = newStack }
  id $ withoutIndentation
     $ writeCurLine
     $ take 60
     $ intercalate ": "
     $ reverse
     $ newStack
  r <- m
  s2 <- mGet
  mSet $ s2 { check_stack = check_stack s1 }
  return r
