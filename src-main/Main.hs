{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}

module Main
  ( main
  )
where



import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.MultiRWS
import           Data.HList.HList
import           Control.Monad.IO.Class

import           Control.Monad

import qualified System.Environment
import qualified System.Console.GetOpt as GetOpt
import           Data.Version ( showVersion )
import qualified Data.Yaml             as Yaml
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text             as Text

import           Development.Iridium
import           Development.Iridium.Config
import           Development.Iridium.UI.Console
import           Development.Iridium.Types

import           Paths_iridium

import           Control.Exception
import           Data.Functor



data Option
  = OptionHelp
  | OptionVersion
  | OptionVerbose
  | OptionDryRun
  deriving (Eq)

optDescrs :: [GetOpt.OptDescr Option]
optDescrs =
  [ GetOpt.Option "h" ["help"]    (GetOpt.NoArg OptionHelp   ) "print help and exit"
  , GetOpt.Option ""  ["version"] (GetOpt.NoArg OptionVersion) "print version and exit"
  , GetOpt.Option "v" ["verbose"] (GetOpt.NoArg OptionVerbose) "control verbosity. Can be used multiple times, -vvvv is max."
  , GetOpt.Option ""  ["dry-run"] (GetOpt.NoArg OptionDryRun)  "stop before firing any rockets."
  ]


main :: IO ()
main = do
  args <- System.Environment.getArgs
  let (opts, others, errs) = GetOpt.getOpt GetOpt.Permute optDescrs args
  let catcher f = (f $> ()) `catch` \e -> putStrLn "" >> print (e :: ErrorCall)
  _ <-
    catcher $ runMaybeT $ runMultiRWSTNil_ $ withMultiState initialLogState $ do
      let printHelp = do
            liftIO $ putStrLn $ GetOpt.usageInfo initNote optDescrs
            mzero
          printVersion = do
            liftIO
              $  putStrLn
              $  "iridium version "
              ++ showVersion version
              ++ ", (c) 2016 Lennart Spitzner"
            mzero
      when (not $ null errs)           printHelp
      when (not $ null others)         printHelp
      when (OptionHelp `elem` opts)    printHelp
      when (OptionVersion `elem` opts) printVersion
      let verbosity = length $ filter (==OptionVerbose) $ opts
      let levels =
            [ LogLevelSilent
              , LogLevelPrint
              , LogLevelDebug
              , LogLevelTrace
              , LogLevelWarn
              , LogLevelError
              , LogLevelThread
              ]
              ++ [ LogLevelInfo | verbosity > 0 ]
              ++ [ LogLevelInfoVerbose | verbosity > 1 ]
              ++ [ LogLevelInfoVerboser | verbosity > 2 ]
              ++ [ LogLevelInfoSpam | verbosity > 3 ]
      setLogMask levels
      let argConfig = if OptionDryRun `elem` opts
            then
              Yaml.Object
              $ HM.singleton (Text.pack "process")
              $ Yaml.Object
              $ HM.singleton (Text.pack "dry-run")
              $ Yaml.Bool
              $ True
            else Yaml.Object $ HM.empty
      iridiumMain argConfig
  return ()
