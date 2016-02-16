{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Development.Iridium.CheckState
  ( initCheckState
  , withStack
  , logStack
  , incWarningCounter
  , incErrorCounter
  , addNotWallClean
  , replaceStackTop
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
initCheckState = CheckState [] 0 0 []

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
  let newStack = s : _check_stack s1
  mSet $ s1 { _check_stack = newStack }
  id $ withoutIndentation
     $ writeCurLine
     $ take 76
     $ intercalate ": "
     $ reverse
     $ newStack
  r <- m
  s2 <- mGet
  mSet $ s2 { _check_stack = _check_stack s1 }
  return r

replaceStackTop
  :: ( MonadIO m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     )
  => String
  -> m ()
replaceStackTop s = do
  s1 <- mGet
  let newStack = s : drop 1 (_check_stack s1)
  mSet s1 { _check_stack = newStack }
  id $ withoutIndentation
     $ writeCurLine
     $ take 76
     $ intercalate ": "
     $ reverse
     $ newStack

logStack
  :: ( MonadIO m
     , MonadMultiState CheckState m
     , MonadMultiState LogState m
     )
  => m ()
logStack = do
  s1 <- mGet
  let line = "(stack: "
          ++ intercalate ": " (reverse $ _check_stack s1)
          ++ ")"
  pushLog LogLevelPrint line

incWarningCounter
  :: ( MonadMultiState CheckState m )
  => m ()
incWarningCounter = do
  s <- mGet
  mSet $ s { _check_warningCount = _check_warningCount s + 1 }

incErrorCounter
  :: ( MonadMultiState CheckState m )
  => m ()
incErrorCounter = do
  s <- mGet
  mSet $ s { _check_errorCount = _check_errorCount s + 1 }

addNotWallClean
  :: ( MonadMultiState CheckState m )
  => String
  -> m ()
addNotWallClean compStr = do
  s <- mGet
  mSet $ s { _check_notWallClean = compStr : _check_notWallClean s }
