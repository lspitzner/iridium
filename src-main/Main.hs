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

import qualified System.Environment
import qualified System.Console.GetOpt as GetOpt
import           Data.Version ( showVersion )

import           Development.Iridium
import           Development.Iridium.Config
import           Development.Iridium.Logging
import           Development.Iridium.Types
import           Development.Iridium.Utils
import           Development.Iridium.CheckState
import           Development.Iridium.Hackage

import           Paths_iridium



data Option
  = OptionHelp
  | OptionVersion
  | OptionVerbose
  deriving (Eq)

optDescrs :: [GetOpt.OptDescr Option]
optDescrs =
  [ GetOpt.Option "h" ["help"]    (GetOpt.NoArg OptionHelp   ) "help"
  , GetOpt.Option ""  ["version"] (GetOpt.NoArg OptionVersion) "version"
  , GetOpt.Option "v" ["verbose"] (GetOpt.NoArg OptionVerbose) "verbosity"
  ]


main :: IO ()
main = do
  args <- System.Environment.getArgs
  let (opts, others, errs) = GetOpt.getOpt GetOpt.Permute optDescrs args
  _ <- runMaybeT $ runMultiRWSTNil_ $ withMultiState initialLogState $ do
    let
      printHelp = do
        liftIO $ putStrLn $ GetOpt.usageInfo initNote optDescrs
        mzero
      printVersion = do
        liftIO $ putStrLn $ "iridium version " ++ showVersion version
        mzero
    when (not $ null errs)           printHelp
    when (not $ null others)         printHelp
    when (OptionHelp `elem` opts)    printHelp
    when (OptionVersion `elem` opts) printVersion
    let verbosity = length $ filter (==OptionVerbose) $ opts
    let levels = [ LogLevelSilent
                 , LogLevelPrint
                 , LogLevelDebug
                 , LogLevelTrace
                 , LogLevelWarn
                 , LogLevelError
                 , LogLevelThread
                 ]
              ++ [ LogLevelInfo        | verbosity > 0 ]
              ++ [ LogLevelInfoVerbose | verbosity > 1 ]
              ++ [ LogLevelInfoSpam    | verbosity > 2 ]
    setLogMask levels
    config <- parseConfigs
    withMultiReader config $ do
      infos <- retrieveInfos
      withMultiReader infos $ withMultiStateA initCheckState $ do
        runChecks
        displaySetting      <- configIsTrueM     ["process", "print-summary"]
        confirmationSetting <- configReadStringM ["process", "confirmation"]
        existWarnings <- liftM ((/=0) . _check_warningCount) mGet
        existErrors   <- liftM ((/=0) . _check_errorCount  ) mGet
        when displaySetting displaySummary
        case confirmationSetting of
          "confirm-always"     -> when (True                        ) askGlobalConfirmation
          "confirm-on-warning" -> when (existWarnings || existErrors) askGlobalConfirmation
          "confirm-on-error"   -> when (                 existErrors) askGlobalConfirmation
          _ -> error $ "bad config value " ++ confirmationSetting
        whenM (not `liftM` configIsTrueM ["process", "dry-run"]) $ do
          uploadPackage
          whenM (configIsTrueM ["process", "upload-docs"]) uploadDocs            
      return ()
  -- runMultiRWSTNil_ $ withMultiState initialLogState $ do
  --   runCommandSuccess "cabal" ["clean"]
  --   runCommandSuccess "cabal" ["configure"]
  return ()    
