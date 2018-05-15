{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Development.Iridium.ExternalProgWrappers
  ( runCommandSuccess
  , runCommandStdOut
  , observeCreateProcessWithExitCode
  , getExternalProgramVersion
  , runCommandSuccessCabal
  , runCommandSuccessHLint
  )
where


import           Prelude hiding ( FilePath )

import qualified Data.Text           as Text
import qualified Turtle              as Turtle
import qualified Control.Foldl       as Foldl
import qualified Control.Exception   as C

import qualified Data.Yaml           as Yaml
import           Control.Monad.Trans.MultiRWS
import           Control.Monad.Trans.MultiState as MultiState
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Distribution.PackageDescription
import           Distribution.Package
import           Filesystem.Path.CurrentOS hiding ( null )
import           Data.Version ( Version(..) )
import           Data.Proxy
import           Data.Tagged
import           Control.Applicative
import           Control.Monad
import           Data.Functor
import           Data.List
import           System.Exit
import           System.IO
import           Control.Concurrent.MVar
import           Control.Concurrent
import           System.IO.Error
import           GHC.IO.Exception ( ioException, IOErrorType(..), IOException(..) )
import           Foreign.C
import           System.Process.Internals
import           Data.IORef
import qualified Data.List.Split as Split
import qualified System.Process as Process
import qualified Data.Char as Char
import           Text.Read ( readMaybe )
import           Data.Text ( Text )

-- import           System.Process hiding ( cwd )

import           Data.Maybe ( maybeToList )

import qualified Filesystem.Path.CurrentOS as Path

import           Development.Iridium.Types
import           Development.Iridium.Utils
import           Development.Iridium.UI.Console
import           Development.Iridium.UI.Prompt
import           Development.Iridium.CheckState
import           Development.Iridium.Config



-- readShellProcessWithExitCode
--   :: String
--   -> [String]
--   -> IO (ExitCode, String, String)
-- readShellProcessWithExitCode c ps =
--   readCreateProcessWithExitCode
--     (shell $ c ++ " " ++ intercalate " " (fmap show ps))
--     ""

runCommandSuccess
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState CheckState m
     , MonadMultiState LogState m
     )
  => String
  -> [String]
  -> m ()
runCommandSuccess c ps = falseToMZero $ do
  let infoStr = c ++ " " ++ intercalate " " ps
  pushLog LogLevelInfo $ infoStr
  withStack infoStr $ do
    outListRef <- liftIO $ newIORef []
    exitCode <- withStack "" $ do -- the additional stack elem is for
                                  -- output display stuff.
      -- this is evil, because we discard states down there.
      -- but .. the alternative is somewhat complex ( to do right ).
      s1 :: LogState   <- mGet
      s2 :: CheckState <- mGet

      let handleLine l = runMultiStateTNil
                       $ MultiState.withMultiStateA s1
                       $ MultiState.withMultiStateA s2
                       $ do
            liftIO $ atomicModifyIORef outListRef (\x -> (l:x, ()))
            replaceStackTop l

      liftIO $ observeCreateProcessWithExitCode
        (Process.shell $ c ++ " " ++ intercalate " " (fmap show ps))
        ""
        handleLine
        handleLine

    
    case exitCode of
      ExitSuccess -> do
        return True
      ExitFailure _ -> do
        pushLog LogLevelPrint infoStr
        outLines <- liftIO $ readIORef outListRef
        reverse outLines `forM_` pushLog LogLevelPrint
        logStack
        return False

runCommandSuccessCabal
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState CheckState m
     , MonadMultiState LogState m
     , MonadMultiReader Config m
     )
  => [String]
  -> m ()
runCommandSuccessCabal ps = do
  cabalInvoc <- configReadStringWithDefaultM "cabal" ["setup", "cabal-command"]
  runCommandSuccess cabalInvoc ps

runCommandSuccessHLint
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState CheckState m
     , MonadMultiState LogState m
     , MonadMultiReader Config m
     )
  => [String]
  -> m ()
runCommandSuccessHLint ps = do
  hlintInvoc <- configReadStringWithDefaultM "hlint" ["setup", "hlint-command"]
  runCommandSuccess hlintInvoc ps

runCommandStdOut
  :: (MonadIO m, MonadPlus m, MonadMultiState LogState m)
  => String
  -> [String]
  -> m String
runCommandStdOut c ps = do
  let infoStr = c ++ " " ++ intercalate " " ps
  (exitCode, stdOut, _stdErr) <- liftIO $ Turtle.procStrictWithErr
    (Text.pack c)
    (Text.pack `fmap` ps)
    Control.Applicative.empty
  case exitCode of
    ExitFailure _ -> do
      pushLog LogLevelError $ "Error running command `" ++ infoStr ++ "`."
      mzero
    ExitSuccess   -> do
      return (Text.unpack stdOut)

getExternalProgramVersion
  :: (MonadIO m, MonadPlus m, MonadMultiState LogState m) => String -> m [Int]
getExternalProgramVersion prog = do
  let err = do
        pushLog LogLevelError
          $  "Could not determine version of external program "
          ++ prog
        mzero
  (exitCode, stdOut, _stdErr) <- liftIO $ Turtle.procStrictWithErr
    (Text.pack prog)
    [Text.pack "--version"]
    Control.Applicative.empty
  case exitCode of
    ExitSuccess   -> do
      case lines (Text.unpack stdOut) of
        (line:_) ->
          case
              takeWhile (`elem`".0123456789")
                $ dropWhile (not . Char.isNumber) line
            of
              "" -> err
              s  -> do
                pushLog LogLevelInfoVerbose
                  $  "detected "
                  ++ prog
                  ++ " version "
                  ++ s
                case mapM readMaybe $ Split.splitOn "." s of
                  Just vs -> return vs
                  Nothing -> err
        _ -> err
    ExitFailure _ -> err

observeCreateProcessWithExitCode
    :: CreateProcess
    -> String            -- ^ standard input
    -> (String -> IO ()) -- ^ stdout line handler
    -> (String -> IO ()) -- ^ stderr line handler
    -> IO (ExitCode)     -- ^ exitcode
observeCreateProcessWithExitCode cp input stdoutHandler stderrHandler = do
    let cp_opts = cp {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = CreatePipe
                  }
    withCreateProcess_ "observeCreateProcessWithExitCode" cp_opts $
      \(Just inh) (Just outh) (Just errh) ph -> do

        let processStream :: Handle -> (String -> IO ()) -> IO ()
            processStream h f = do
              catchIOError (forever $ hGetLine h >>= f) (\e -> unless (isEOFError e) (ioError e))

        -- fork off threads to start consuming stdout & stderr
        withForkWait  (processStream outh stdoutHandler) $ \waitOut ->
         withForkWait (processStream errh stderrHandler) $ \waitErr -> do

          -- now write any input
          unless (null input) $
            ignoreSigPipe $ hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut
          waitErr

          -- hClose outh
          -- hClose errh

        -- wait on the process
        ex <- Process.waitForProcess ph

        return ex

-- ***********
-- copied from System.Process, because not exposed..
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either C.SomeException ()))
  C.mask $ \restore -> do
    tid <- forkIO $ C.try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either C.throwIO return
    restore (body wait) `C.onException` killThread tid
withCreateProcess_
  :: String
  -> CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess_ fun c action =
    C.bracketOnError (createProcess_ fun c) cleanupProcess
                     (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)
ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> C.throwIO e

cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
               -> IO ()
cleanupProcess (mb_stdin, mb_stdout, mb_stderr,
                ph@(ProcessHandle _ delegating_ctlc)) = do
    Process.terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
    maybe (return ()) hClose mb_stdout
    maybe (return ()) hClose mb_stderr
    -- terminateProcess does not guarantee that it terminates the process.
    -- Indeed on Unix it's SIGTERM, which asks nicely but does not guarantee
    -- that it stops. If it doesn't stop, we don't want to hang, so we wait
    -- asynchronously using forkIO.

    -- However we want to end the Ctl-C handling synchronously, so we'll do
    -- that synchronously, and set delegating_ctlc as False for the
    -- waitForProcess (which would otherwise end the Ctl-C delegation itself).
    when delegating_ctlc
      stopDelegateControlC
    _ <- forkIO (Process.waitForProcess (resetCtlcDelegation ph) >> return ())
    return ()
  where
    resetCtlcDelegation (ProcessHandle m _) = ProcessHandle m False
-- ***********
