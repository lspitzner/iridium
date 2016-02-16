{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Development.Iridium.Utils
  ( askAllBuildInfo
  , askPackageName
  , askPackageVersion
  , runCommandSuccess
  , mzeroToFalse
  , runCheck
  , fallbackCheck
  -- , falseToConfirm
  , falseToAbort
  , ignoreBool
  , boolToWarning
  , boolToError
  , getLocalFilePath
  , observeCreateProcessWithExitCode
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

-- well, no Turtle, apparently.
-- no way to retrieve stdout, stderr and exitcode.
-- the most generic case, not supported? psshhh.
import           System.Process hiding ( cwd )

import           Data.Maybe ( maybeToList )

import qualified Filesystem.Path.CurrentOS as Path

import           Development.Iridium.Types
import           Development.Iridium.Logging
import           Development.Iridium.Prompt
import           Development.Iridium.CheckState



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

runCommandSuccess
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState CheckState m
     , MonadMultiState LogState m
     )
  => String
  -> [String]
  -> m ()
runCommandSuccess c ps = do
  let infoStr = c ++ " " ++ intercalate " " ps
  withStack infoStr $ withStack "" $ do
    -- this is evil, because we discard states down there.
    -- but .. the alternative is somewhat complex ( to do right ).
    s1 :: LogState   <- mGet
    s2 :: CheckState <- mGet
    outListRef <- liftIO $ newIORef []

    let handleLine l = runMultiStateTNil
                     $ MultiState.withMultiStateA s1
                     $ MultiState.withMultiStateA s2
                     $ do
          liftIO $ atomicModifyIORef outListRef (\x -> (l:x, ()))
          replaceStackTop l

    exitCode <- liftIO $ observeCreateProcessWithExitCode (proc c ps) "" handleLine handleLine
    case exitCode of
      ExitSuccess -> do
        pushLog LogLevelInfo $ infoStr
      ExitFailure _ -> do
        pushLog LogLevelPrint infoStr
        outLines <- liftIO $ readIORef outListRef
        reverse outLines `forM_` pushLog LogLevelPrint
        logStack
        mzero    
    -- (exitCode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode c ps ""
    -- case exitCode of
    --   Turtle.ExitSuccess   -> do
    --     pushLog LogLevelInfo $ infoStr
    --   Turtle.ExitFailure _ -> do
    --     pushLog LogLevelPrint infoStr
    --     withIndentation $ do
    --       lines stdOut `forM_` \l ->
    --         pushLog LogLevelPrint $ l
    --       lines stdErr `forM_` \l ->
    --         pushLog LogLevelPrint $ l
    --     logStack
    --     mzero

mzeroToFalse :: Monad m => MaybeT m a -> m Bool
mzeroToFalse m = do
  x <- runMaybeT m
  case x of
    Nothing -> return False
    Just _  -> return True

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
        ex <- waitForProcess ph

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
    terminateProcess ph
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
    _ <- forkIO (waitForProcess (resetCtlcDelegation ph) >> return ())
    return ()
  where
    resetCtlcDelegation (ProcessHandle m _) = ProcessHandle m False
-- ***********
