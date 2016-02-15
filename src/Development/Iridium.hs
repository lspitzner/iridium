{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Iridium
  ( initNote
  , helpString
  , retrieveInfos
  , runFirstChecks
  , displaySummary
  , askGlobalConfirmation
  , runSecondChecks
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
import           Text.Read ( readMaybe )
import           Control.Monad.Extra ( whenM )
import           Control.Monad.Trans.MultiRWS
import           Data.Proxy
import           Data.Tagged

import           Data.HList.ContainsType

import           Data.Version ( showVersion )
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import qualified Distribution.Package as Package

import           Development.Iridium.Types
import           Development.Iridium.Utils
import           Development.Iridium.Logging
import           Development.Iridium.Hackage
import           Development.Iridium.Config
import           Development.Iridium.Prompt
import           Development.Iridium.CheckState
import qualified Development.Iridium.FirstChecks  as FirstChecks
import qualified Development.Iridium.SecondChecks as SecondChecks

import           Filesystem.Path.CurrentOS



initNote :: String
initNote
  = "iridium - automated cabal package uploading utility"

helpString :: String
helpString
  = unlines
  $ [ "iridium includes the following steps:"
    , "1)  Run sanity checks, ask for confirmation if somethings"
    , "    seems suspicious."
    , "2a) Print summary and ask for confirmation (as configured)"
    , "3)  Run all configured checks (this may take a moment)."
    , "2b) Print summary and ask for confirmation (as configured)"
    , "4)  If enabled, tag the commit and upload to remote repository"
    , "5)  Upload the package to hackage"
    , "    (and, if enabled, the docs)."
    , ""
    , "By default, you will be asked a final time before any side-effects"
    , "(uploading) are performed."
    ]

retrieveInfos
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiReader Config m
     )
  => m Infos
retrieveInfos = do
  cwd <- Turtle.pwd
  packageDesc <- do
    packageFile <- do
      allFiles <- Turtle.fold (Turtle.ls cwd) Foldl.list
      let cabalFiles =
            filter (\p -> Turtle.extension p == Just (Text.pack "cabal"))
                   allFiles
      case cabalFiles of
        [f] -> return f
        [] -> do
          pushLog LogLevelError "Error: Found no cabal package!"
          mzero
        _ -> do
          pushLog LogLevelError "Error: Found more than one cabal package file!"
          mzero
    pushLog LogLevelInfo $ "Reading cabal package description " ++ encodeString packageFile
    content <- Text.unlines `liftM` Turtle.fold (Turtle.input packageFile) Foldl.list
    -- pushLog LogLevelDebug $ Text.unpack content
    let parseResult = parsePackageDescription $ Text.unpack content
    case parseResult of
      ParseFailed e -> do
        pushLog LogLevelError $ "Error parsing cabal package file: " ++ show e
        mzero
      ParseOk _ x -> do
        -- pushLog LogLevelDebug $ show $ packageDescription x
        return x
  let pkgName = (\(Package.PackageName n) -> n)
              $ Package.pkgName
              $ package
              $ packageDescription
              $ packageDesc
  urlStr <- configReadStringM ["setup", "remote-server"]
  latestVersionStr <- retrieveLatestVersion urlStr pkgName
  let latestVersionM = readMaybe latestVersionStr
  -- pushLog LogLevelDebug $ show latestVersionM
  repoInfo :: Tagged NoRepo () <- repo_retrieveInfo 
  return $ Infos cwd packageDesc latestVersionM repoInfo

runFirstChecks
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiReader Config m
     , MonadMultiReader Infos m
     )
  => m ()
runFirstChecks = do
  whenM (return True) {- consistency ! -}                     FirstChecks.packageCheck
  whenM (return True) {- consistency ! -}                     FirstChecks.remoteVersion
  whenM (configIsEnabledM ["checks", "hlint"])                FirstChecks.hlint
  whenM (configIsEnabledM ["checks", "upper-bounds-exist"]) $ FirstChecks.upperBounds
  whenM (configIsEnabledM ["checks", "changelog"]) $ FirstChecks.changelog
  return ()

displaySummary
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiReader Config m
     , MonadMultiReader Infos m
     )
  => m ()
displaySummary = do
  pushLog LogLevelPrint "Summary:"
  Package.PackageName pNameStr <- askPackageName
  pushLog LogLevelPrint $ "  Package:         " ++ pNameStr
  pVersion <- askPackageVersion
  pushLog LogLevelPrint $ "  Version:         " ++ showVersion pVersion
  remoteServer <- configReadStringM ["setup", "remote-server"]
  pushLog LogLevelPrint $ "  Remote location: " ++ remoteServer
  Infos _ _ _ repoInfo <- mAsk
  repo_displaySummary repoInfo
  pushLog LogLevelPrint $ "  Actions:         " ++ "Upload package" -- TODO: documentation, repo stuff
  return ()

askGlobalConfirmation
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiReader Config m
     )
  => m ()
askGlobalConfirmation = do
  whenM (not `liftM` configIsTrueM ["process", "dry-run"]) $
    promptYesOrNo "Continue (<y>es; <n> aborts)"

runSecondChecks
  :: ( m ~ MultiRWST r w s m0
     , MonadIO m0
     , MonadPlus m0
     , ContainsType Config r
     , ContainsType LogState s
     )
  => m ()
runSecondChecks = withMultiStateA initCheckState $ do
  SecondChecks.compile
  whenM (configIsEnabledM ["checks", "compiler-versions"]) $ SecondChecks.compileVersions
  whenM (configIsEnabledM ["checks", "documentation"])     $ SecondChecks.documentation
  pushLog LogLevelError "TODO second checks"
