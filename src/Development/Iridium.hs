{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Iridium
  ( iridiumMain
  , initNote
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
import           Filesystem.Path.CurrentOS
import           System.Exit
import           Text.ParserCombinators.ReadP
import qualified Distribution.PackageDescription as PackageDescription
import           Distribution.PackageDescription.Parse
import qualified Distribution.Package as Package

import           Development.Iridium.Types
import           Development.Iridium.Utils
import           Development.Iridium.UI.Console
import           Development.Iridium.Hackage
import           Development.Iridium.Config
import           Development.Iridium.UI.Prompt
import           Development.Iridium.CheckState
import qualified Development.Iridium.Checks  as Checks
import qualified Development.Iridium.Repo.Git as Git
import           Development.Iridium.ExternalProgWrappers



initNote :: String
initNote
  = "iridium - automated cabal package uploading utility"

retrieveInfos
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState LogState m
     , MonadMultiReader Config m
     )
  => m Infos
retrieveInfos = do
  cabalInvoc <- configReadStringWithDefaultM "cabal" ["setup", "cabal-command"]
  cabalVersion <- getExternalProgramVersion cabalInvoc
  when (cabalVersion < [1,22,8]) $ do
    pushLog LogLevelError "This program requires cabal version 1.22.8 or later. aborting."
    mzero
  hlint <- configReadStringWithDefaultM "hlint" ["setup", "hlint-command"]
  _ <- getExternalProgramVersion hlint
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
              $ PackageDescription.package
              $ PackageDescription.packageDescription
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
  configDecideStringM ["repository", "type"]
    [ (,) "none" $ do
        repoInfo :: NoRepo <- repo_retrieveInfo
        return $ Infos cwd packageDesc latestVersionM repoInfo
    , (,) "git" $ do
        repoInfo :: Git.GitImpl <- repo_retrieveInfo
        return $ Infos cwd packageDesc latestVersionM repoInfo
    ]


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
  whenM (configIsEnabledM ["checks", "compiler-versions"])  $ Checks.compileVersions
  whenM (configIsEnabledM ["checks", "upper-bounds-stackage"]) $ Checks.upperBoundsStackage
  whenM (configIsEnabledM ["checks", "documentation"])      $ Checks.documentation
  -- we do this last so that we return in an "everything is compiled" state,
  -- if possible.
  Checks.compile
  whenM (configIsEnabledM ["checks", "hlint"])                Checks.hlint
  whenM (configIsEnabledM ["checks", "upper-bounds-exist"]) $ Checks.upperBounds
  whenM (return True)                                         Checks.packageCheck
  whenM (configIsEnabledM ["checks", "changelog"])          $ Checks.changelog
  whenM (return True)                                         Checks.remoteVersion
  whenM (return True)                                         repoRunChecks

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
      repoDisplaySummary
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
  => Bool
  -> m ()
askGlobalConfirmation existErrors = do
  if existErrors
    then promptSpecific "There are errors; write \"override\" to (try) continue anyways " "override"
    else promptYesOrNo "Continue [y]es [n]o? "

iridiumMain :: MultiRWST '[Config] '[] '[LogState] (MaybeT IO) ()
iridiumMain = do
  infos <- retrieveInfos
  withMultiReader infos $ withMultiStateA initCheckState $ do
    runChecks
    displaySetting      <- configIsTrueM     ["process", "print-summary"]
    existWarnings <- liftM ((/=0) . _check_warningCount) mGet
    existErrors   <- liftM ((/=0) . _check_errorCount  ) mGet
    when displaySetting displaySummary
    whenM (not `liftM` configIsTrueM ["process", "dry-run"]) $ do
      configDecideStringM ["process", "confirmation"]
        [ ("confirm-always"    , when (True                        ) $ askGlobalConfirmation existErrors)
        , ("confirm-on-warning", when (existWarnings || existErrors) $ askGlobalConfirmation existErrors)
        , ("confirm-on-error"  , when (                 existErrors) $ askGlobalConfirmation existErrors)
        ]
      repoPerformAction
      uploadPackage
      whenM (configIsTrueM ["process", "upload-docs"]) uploadDocs
