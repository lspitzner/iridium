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

import           Development.Iridium.Types
import           Development.Iridium.Utils
import           Development.Iridium.UI.Console
import           Development.Iridium.Config



data GitImpl = GitImpl
  { _git_branchName :: String
  }

instance Repo GitImpl where
  repo_retrieveInfo = do
    branchStringRaw <- runCommandStdOut "git" ["branch"]
    case branchStringRaw of
      ('*':' ':branchName) ->
        return $ GitImpl branchName
      _ -> do
        pushLog LogLevelError "Could not parse current git branch name."
        mzero
  repo_runChecks _git = do
    uncommittedChangesCheck
  repo_displaySummary git = do
    pushLog LogLevelPrint $ "[git]"
    whenM (configIsTrueM ["repository", "git", "display-current-branch"]) $
      pushLog LogLevelPrint $ "  Branch:        " ++ _git_branchName git
  repo_ActionSummary _git = do
    -- TODO
    return []
  repo_performAction _git = do
    -- TODO
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
