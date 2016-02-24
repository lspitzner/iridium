{-# LANGUAGE TypeFamilies #-}

module Development.Iridium.UI.Prompt
  ( askConfirmationOrMZero
  , promptYesOrNo
  , promptSpecific
  )
where



#include "qprelude/bundle-gamma.inc"

import qualified Turtle        as Turtle

import           Development.Iridium.Types
import           Development.Iridium.UI.Console
import           Development.Iridium.Config



askConfirmationOrMZero
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     )
  => m ()
askConfirmationOrMZero = do
  liftIO $ putStr "> Abort imminent; enter 'i' to overwrite and continue> "
  liftIO $ hFlush stdout
  s <- liftIO $ System.IO.getLine
  case s of
    "i" -> do
      pushLog LogLevelPrint "  (Remember that you can disable individual tests in iridium.yaml)"
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
  s <- liftIO $ System.IO.getLine
  case s of
    "y" -> do
      return ()
    "n" -> mzero
    _   -> promptYesOrNo p

promptSpecific
  :: (MonadIO m, MonadPlus m)
  => String
  -> String
  -> m ()
promptSpecific p cont = do
  liftIO $ putStr $ "> " ++ p ++ "> "
  liftIO $ hFlush stdout
  s <- liftIO $ System.IO.getLine
  if s == cont then return () else mzero
