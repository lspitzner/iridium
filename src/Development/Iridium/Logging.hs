module Development.Iridium.Logging
  ( LogLevel (..)
  , setLogMask
  , putLog
  , LogState
  , initialLogState
  , withIndentation
  , withoutIndentation
  )
where



import qualified System.Unsafe as Unsafe
import           Data.IORef
import           Control.Monad ( when )
import           Control.Monad.IO.Class

import           Control.Monad.Trans.MultiRWS

import           System.Console.ANSI

import           Development.Iridium.Types



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

initialLogState :: LogState
initialLogState = LogState
  [ LogLevelPrint
  , LogLevelWarn
  , LogLevelError
  , LogLevelInfo
  ]
  0
  Nothing
  ""

-- only logmessages that are _in_ the list in this IORef are printed.
-- {-# NOINLINE currentLogMask #-}
-- currentLogMask :: IORef [LogLevel]
-- currentLogMask
--   = Unsafe.performIO
--   $ newIORef
--   $ [ LogLevelPrint
--     , LogLevelWarn
--     , LogLevelError
--     , LogLevelInfo
--     ]

-- setLogMask :: MonadIO io => [LogLevel] -> io ()
-- setLogMask = liftIO . writeIORef currentLogMask

-- putLog :: MonadIO io => LogLevel -> String -> io ()
-- putLog level message = liftIO $ do
--   mask <- readIORef currentLogMask
--   when (level `elem` mask) $
--     putStrLn message

setLogMask
  :: ( MonadMultiState LogState m )
  => [LogLevel]
  -> m ()
setLogMask levels = do
  s <- mGet
  mSet $ s { _log_mask = levels }

withIndentation
  :: MonadMultiState LogState m
  => m a
  -> m a
withIndentation k = do
  s <- mGet
  mSet $ s { _log_indent = _log_indent s + 1 }
  r <- k
  mSet s -- we do a full reset here. this might be evil.
         -- but probably just the right thing to do.
  return r

withoutIndentation
  :: MonadMultiState LogState m
  => m a
  -> m a
withoutIndentation k = do
  s <- mGet
  mSet $ s { _log_indent = 0 }
  r <- k
  mSet s -- we do a full reset here. this might be evil.
         -- but probably just the right thing to do.
  return r

checkWhenLevel
  :: ( MonadMultiState LogState m )
  => LogLevel
  -> m ()
  -> m ()
checkWhenLevel level m = do
  s <- mGet
  when (level `elem` _log_mask s) m

pushLog
  :: ( MonadMultiState LogState m
     , MonadIO m
     )
  => LogLevel
  -> String
  -> m ()
pushLog level message = checkWhenLevel level $ do
  s <- mAsk
  case _log_prepared s of
    Nothing ->
      -- clear current line
      -- write message
      -- newline
      _
    Just x -> _

pushLogPrepare
  :: ( MonadMultiState LogState m
     , MonadIO m
     )
  => String
  -> m ()
pushLogPrepare = do
  s <- mAsk
  case _log_prepared s of
    Nothing -> _
    Just x  -> _

pushLogFinalize
  :: ( MonadMultiState LogState m
     , MonadIO m
     )
  => Int
  -> String
  -> m ()
pushLogFinalize indent message = do
  s <- mAsk
  _

writeCurLine
  :: ( MonadMultiState LogState m
     , MonadIO m
     )
  => String
  -> m ()
writeCurLine message = do
  _

pushCurLine
  :: ( MonadMultiState LogState m
     , MonadIO m
     )
  => m ()
pushCurLine = do
  _

-- putLog
--   :: ( MonadMultiState LogState m
--      , MonadIO m
--      )
--   => LogLevel
--   -> String
--   -> m ()
-- putLog level message = do
--   LogState mask i <- mGet
--   liftIO $ when (level `elem` mask) $
--     putStrLn $ replicate (2*i) ' ' ++ message

