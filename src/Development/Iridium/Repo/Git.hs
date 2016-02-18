module Development.Iridium.Repo.Git
  ( GitImpl
  )
where



import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.MultiRWS
import           Control.Monad.Extra ( whenM )
import           Data.List.Extra
import           Data.Version
import           System.Process hiding ( cwd )
import           Data.Char

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
    case branchStringRaw of
      ('*':' ':branchName) ->
        return $ GitImpl $ takeWhile (`notElem` "\n\r") branchName
      _ -> do
        pushLog LogLevelError "Could not parse current git branch name."
        mzero
  repo_runChecks _git = withStack "[git]" $ do
    pushLog LogLevelPrint "[git]"
    withIndentation $ do
      uncommittedChangesCheck
  repo_displaySummary git = do
    pushLog LogLevelPrint $ "[git]"
    withIndentation $ do
      whenM (configIsTrueM ["repository", "git", "display-current-branch"]) $
        pushLog LogLevelPrint $ "Branch:        " ++ _git_branchName git
  repo_ActionSummary _git = do
    tagEnabled <- configIsEnabledM ["repository", "git", "release-tag"]
    pushEnabled <- configIsEnabledM ["repository", "git", "push-remote"]
    return $ ["Tag the current commit" | tagEnabled]
          ++ ["Push the current branch to upstream repo" | pushEnabled]
  repo_performAction _git = do
    tagEnabled <- configIsEnabledM ["repository", "git", "release-tag"]
    when tagEnabled $ do
      pushLog LogLevelPrint "[git] Tagging this release."
      withIndentation $ do
        tagRawStr <- configReadStringWithDefaultM "v$VERSION" ["repository", "git", "release-tag", "content"]
        vers <- liftM showVersion askPackageVersion
        let tagStr = replace "$VERSION" vers tagRawStr
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
                       ]
                     )
                     Nothing Nothing Nothing Nothing Nothing
          >>= waitForProcess
    return ()

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
