module Main
  ( main
  )
where



import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.MultiRWS
import           Data.HList.HList
import           Control.Monad.IO.Class

import           Control.Monad
import           Text.Read ( readMaybe )
import           Control.Monad.Extra ( whenM )

import           Development.Iridium
import           Development.Iridium.Config
import           Development.Iridium.Logging
import           Development.Iridium.Types

import           Development.Iridium.Utils



main :: IO ()
main = do
  _ <- runMaybeT $ runMultiRWSTNil_ $ withMultiState initialLogState $ do
    liftIO $ putStrLn initNote
    -- for debugging purposes, for now.
    setLogMask [ LogLevelSilent
               , LogLevelPrint
               , LogLevelDebug
               , LogLevelTrace
               , LogLevelWarn
               , LogLevelError
               , LogLevelInfo
               , LogLevelInfoVerbose
               , LogLevelInfoSpam
               , LogLevelThread
               ]
    config <- parseConfigs
    withMultiReader config $ do
      infos <- retrieveInfos
      withMultiReader infos $ do
        summaryConfirmConf <- configReadStringM ["process", "summary-and-confirmation"]
        case summaryConfirmConf of
          "none"          -> return ()
          "summary-only"  -> displaySummary
          "before-checks" -> return ()
          "after-checks"  -> return ()
          _ -> error $ "bad config value " ++ summaryConfirmConf
        runFirstChecks
        when (summaryConfirmConf=="before-checks") $ do
          displaySummary >> askGlobalConfirmation
        runSecondChecks
        when (summaryConfirmConf=="after-checks") $ do
          displaySummary >> askGlobalConfirmation
        whenM (not `liftM` configIsTrueM ["process", "dry-run"]) $
          putLog LogLevelError $ "ACTUAL UPLOAD REALLY HAPPENING JUST NOW"
      return ()
  -- runMultiRWSTNil_ $ withMultiState initialLogState $ do
  --   runCommandSuccess "cabal" ["clean"]
  --   runCommandSuccess "cabal" ["configure"]
  return ()    
