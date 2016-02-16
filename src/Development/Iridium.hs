{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Iridium
  ( iridiumMain
  , initNote
  , helpString
  , retrieveInfos
  , runChecks
  , displaySummary
  , askGlobalConfirmation
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
import           Control.Monad.Trans.Control
import           Data.Proxy
import           Data.Tagged
import           Data.List
import           Data.HList.HList

import           Data.HList.ContainsType

import           Data.Version ( showVersion, parseVersion )
import           Text.ParserCombinators.ReadP
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
  let parseResults = readP_to_S parseVersion latestVersionStr
  let latestVersionM = fmap fst
                     $ flip find parseResults
                     $ \r -> case r of
        (_, "") -> True
        _       -> False
  pushLog LogLevelInfoVerbose $ "remote version: " ++ show (liftM showVersion latestVersionM)
  repoInfo :: Tagged NoRepo () <- repo_retrieveInfo 
  return $ Infos cwd packageDesc latestVersionM repoInfo

runChecks
  :: ( m ~ MultiRWST r w s m0
     , MonadIO m0
     , MonadPlus m0
     , MonadBaseControl IO m0
     , ContainsType LogState s
     , ContainsType CheckState s
     , ContainsType Config r
     , ContainsType Infos r
     )
  => m ()
runChecks = do
  whenM (configIsEnabledM ["checks", "compiler-versions"])  $ SecondChecks.compileVersions
  whenM (configIsEnabledM ["checks", "upper-bounds-stackage"]) $ SecondChecks.upperBoundsStackage
  whenM (configIsEnabledM ["checks", "documentation"])      $ SecondChecks.documentation
  -- we do this last so that we return in an "everything is compiled" state,
  -- if possible.
  SecondChecks.compile
  whenM (configIsEnabledM ["checks", "hlint"])                FirstChecks.hlint
  whenM (configIsEnabledM ["checks", "upper-bounds-exist"]) $ FirstChecks.upperBounds
  whenM (return True)                                         FirstChecks.packageCheck
  whenM (configIsEnabledM ["checks", "changelog"])          $ FirstChecks.changelog
  whenM (return True)                                         FirstChecks.remoteVersion

displaySummary
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiState CheckState m
     , MonadMultiReader Config m
     , MonadMultiReader Infos m
     )
  => m ()
displaySummary = do
  pushLog LogLevelPrint "Summary:"
  withIndentation $ do
    Package.PackageName pNameStr <- askPackageName
    pushLog LogLevelPrint $ "Package:         " ++ pNameStr
    pVersion <- askPackageVersion
    pushLog LogLevelPrint $ "Version:         " ++ showVersion pVersion
    -- TODO: This should not be printed unless we verify that
    --       the information is correct by looking at the .cabal config.
    -- remoteServer <- configReadStringM ["setup", "remote-server"]
    -- pushLog LogLevelPrint $ "Remote location: " ++ remoteServer
    do
      CheckState _ errC warnC walls <- mGet
      pushLog LogLevelPrint $ "Warning count:   " ++ show warnC
      pushLog LogLevelPrint $ "Error   count:   " ++ show errC
      pushLog LogLevelPrint $ "Not -Wall clean: " ++ intercalate ", " (reverse walls)
    do
      Infos _ _ _ repoInfo <- mAsk
      repo_displaySummary repoInfo
    uploadEnabled <- configIsTrueM ["process", "upload-docs"]
    let actions = ["Upload package"]
               ++ ["Upload documentation" | uploadEnabled]
    pushLog LogLevelPrint $ "Actions:         " ++ intercalate ", " actions
  return ()

askGlobalConfirmation
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiReader Config m
     )
  => m ()
askGlobalConfirmation = do
  whenM (not `liftM` configIsTrueM ["process", "dry-run"]) $
    promptYesOrNo "Continue <y>es <n>o? "

iridiumMain :: MultiRWST '[Config] '[] '[LogState] (MaybeT IO) ()
iridiumMain = do
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
