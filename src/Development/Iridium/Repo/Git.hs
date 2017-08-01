module Development.Iridium.Repo.Git
  ( GitImpl
  )
where



import           Control.Monad (when, mzero, liftM, MonadPlus)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.MultiRWS
import           Control.Monad.Extra ( whenM )
import           Data.List.Extra
import           Data.Version
import           System.Process hiding ( cwd )
import           Data.Char
import           Data.Foldable (forM_)

import           Development.Iridium.Types
import           Development.Iridium.Utils
import           Development.Iridium.ExternalProgWrappers
import           Development.Iridium.UI.Console
import           Development.Iridium.Config
import           Development.Iridium.CheckState



data GitImpl = GitImpl
  { _git_branchName :: String
  }

instance Repo GitImpl where
  repo_retrieveInfo = do
    branchStringRaw <- runCommandStdOut "git" ["branch"]
    let branchNamePred ('*':' ':branchName) = Just branchName
        branchNamePred _ = Nothing
    case firstJust branchNamePred $ lines branchStringRaw of
      Nothing -> do
        pushLog LogLevelError "Could not parse current git branch name."
        mzero
      Just branchName ->
        return $ GitImpl $ branchName
  repo_runChecks _git = withStack "[git]" $ do
    pushLog LogLevelPrint "[git]"
    withIndentation $ do
      uncommittedChangesCheck
  repo_displaySummary git = do
    pushLog LogLevelPrint $ "[git]"
    withIndentation $ do
      whenM (configIsTrueM ["repository", "git", "display-current-branch"]) $
        pushLog LogLevelPrint $ "Branch:               " ++ _git_branchName git
  repo_ActionSummary _git = do
    tagEnabled <- configIsEnabledM ["repository", "git", "release-tag"]
    tagAction <- if tagEnabled
      then do
        tagStr <- askTagString
        return $ ["Tag the current commit with \"" ++ tagStr ++ "\""]
      else
        return []
    pushEnabled <- configIsEnabledM ["repository", "git", "push-remote"]
    return $ tagAction
          ++ ["Push current branch and tag to upstream repo" | pushEnabled]
  repo_performAction git = do
    tagEnabled <- configIsEnabledM ["repository", "git", "release-tag"]
    tagStringMaybe <- if tagEnabled then liftM Just askTagString else return Nothing
    tagStringMaybe `forM_` \tagStr -> do
      pushLog LogLevelPrint "[git] Tagging this release."
      withIndentation $ do
        curOut <- runCommandStdOut "git" ["tag", "-l", tagStr]
        pushLog LogLevelDebug curOut
        if all isSpace curOut
          then do
            mzeroIfNonzero $ liftIO $
              runProcess "git"
                         ( [ "tag"
                           , tagStr
                           ]
                         )
                         Nothing Nothing Nothing Nothing Nothing
              >>= waitForProcess
            pushLog LogLevelPrint $ "Tagged as " ++ tagStr
          else pushLog LogLevelPrint "Tag already exists, leaving it as-is."
    pushEnabled <- configIsEnabledM ["repository", "git", "push-remote"]
    when pushEnabled $ do
      pushLog LogLevelPrint "[git] Pushing to remote."
      withIndentation $ do
        remote <- configReadStringWithDefaultM "origin" ["repository", "git", "push-remote", "remote-name"]
        mzeroIfNonzero $ liftIO $
          runProcess "git"
                     ( [ "push"
                       , remote
                       , _git_branchName git
                       ] ++ maybe [] return tagStringMaybe
                     )
                     Nothing Nothing Nothing Nothing Nothing
          >>= waitForProcess
    return ()

askTagString
  :: ( MonadMultiReader Config m
     , MonadMultiReader Infos m
     )
  => m String
askTagString = do
  tagRawStr <- configReadStringWithDefaultM "$VERSION" ["repository", "git", "release-tag", "content"]
  vers <- liftM showVersion askPackageVersion
  return $ replace "$VERSION" vers tagRawStr


uncommittedChangesCheck
  :: ( MonadIO m
     , MonadPlus m
     , MonadMultiState CheckState m
     , MonadMultiState LogState m
     )
  => m ()
uncommittedChangesCheck = boolToWarning
                        $ runCheck "Testing for uncommitted changes"
                        $ withStack "git status -uno"
                        $ do
  changesRaw <- runCommandStdOut "git" ["status", "-uno", "--porcelain"]
  let changes = lines changesRaw
  if null changes
    then
      return True
    else do
      pushLog LogLevelPrint $ "git status reports uncommitted changes:"
      withIndentation $ changes `forM_` pushLog LogLevelPrint
      return False
