module Development.Iridium.UI.Console
  ( LogLevel (..)
  , setLogMask
  , pushLog
  , pushLogPrepare
  , pushLogFinalize
  , writeCurLine
  , pushCurLine
  , LogState
  , initialLogState
  , withIndentation
  , withoutIndentation
  , isEnabledLogLevel
  )
where



import qualified System.Unsafe as Unsafe
import           Data.IORef
import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Monad.Trans.MultiRWS

import           System.Console.ANSI
import           System.IO

import           Development.Iridium.Types



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
  s2 <- mGet
  mSet $ s2 { _log_indent = _log_indent s }
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

getIndentLine
  :: ( MonadMultiState LogState m )
  => String
  -> m String
getIndentLine str = do
  s <- mGet
  return $ replicate (2*_log_indent s) ' ' ++ str

flushPrepared
  :: ( MonadIO m
     , MonadMultiState LogState m
     )
  => m ()
flushPrepared = do
  s <- mGet
  liftIO $ clearLine >> setCursorColumn 0 >> hFlush stdout
  case _log_prepared s of
    Nothing -> return ()
    Just x -> do
      liftIO $ putStrLn x
      mSet $ s { _log_prepared = Nothing }

pushLog
  :: ( MonadMultiState LogState m
     , MonadIO m
     )
  => LogLevel
  -> String
  -> m ()
pushLog level message = checkWhenLevel level $ do
  flushPrepared
  forM_ (lines message) $
    (liftIO . putStrLn =<<) . getIndentLine

pushLogPrepare
  :: ( MonadMultiState LogState m
     , MonadIO m
     )
  => String
  -> m ()
pushLogPrepare message = do
  s <- mGet
  flushPrepared
  mess <- getIndentLine message
  mSet $ s { _log_prepared = Just mess }

pushLogFinalize
  :: ( MonadMultiState LogState m
     , MonadIO m
     )
  => Int
  -> String
  -> m ()
pushLogFinalize indent message = do
  s <- mGet
  liftIO $ clearLine >> setCursorColumn 0 >> hFlush stdout
  case _log_prepared s of
    Nothing -> do
      liftIO $ putStrLn $ replicate indent ' ' ++ message
    Just x -> do
      liftIO $ if length x > indent
        then do
          putStrLn x
          putStrLn $ replicate indent ' ' ++ message
        else do
          putStrLn $ x ++ replicate (indent - length x) ' ' ++ message
      mSet $ s { _log_prepared = Nothing }

writeCurLine
  :: ( MonadMultiState LogState m
     , MonadIO m
     )
  => String
  -> m ()
writeCurLine message = do
  liftIO $ clearLine >> setCursorColumn 0
  s <- mGet
  imess <- getIndentLine message
  liftIO $ putStr $ "> " ++ imess
  liftIO $ hFlush stdout
  mSet $ s { _log_cur = imess }

pushCurLine
  :: ( MonadMultiState LogState m
     , MonadIO m
     )
  => LogLevel
  -> m ()
pushCurLine level = do
  s <- mGet
  if level `elem` _log_mask s
    then liftIO $ putStrLn ""
    else liftIO $ clearLine >> setCursorColumn 0 >> hFlush stdout
  mSet $ s { _log_cur = "" }

isEnabledLogLevel
  :: ( MonadMultiState LogState m )
  => LogLevel
  -> m Bool
isEnabledLogLevel level = do
  s <- mGet
  return $ level `elem` _log_mask s

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

