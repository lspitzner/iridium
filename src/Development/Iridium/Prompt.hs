{-# LANGUAGE TypeFamilies #-}

module Development.Iridium.Prompt
  ( askConfirmationOrMZero
  , promptYesOrNo
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

import           Development.Iridium.Types
import           Development.Iridium.Logging
import           Development.Iridium.Config

import           Control.Monad.Trans.MultiRWS

-- well, fuck Turtle, apparently.
-- no way to retrieve stdout, stderr and exitcode.
-- the most generic case, not supported? psshhh.
import           System.Process
import           System.IO ( hFlush, stdout )
import           Control.Concurrent ( threadDelay )



askConfirmationOrMZero
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     )
  => m ()
askConfirmationOrMZero = do
  liftIO $ putStr "> Abort imminent; enter 'i' to overwrite and continue> "
  liftIO $ hFlush stdout
  s <- liftIO $ getLine
  case s of
    "i" -> do
      putLog LogLevelPrint "  (Remember that you can disable individual tests in iridium.yaml)"
      liftIO $ threadDelay 1000000
      return ()
    _   -> mzero

promptYesOrNo
  :: (MonadIO m, MonadPlus m)
  => String
  -> m ()
promptYesOrNo p = do
  liftIO $ putStr $ "> " ++ p ++ "> "
  liftIO $ hFlush stdout
  s <- liftIO $ getLine
  case s of
    "y" -> do
      return ()
    "n" -> mzero
    _   -> promptYesOrNo p
