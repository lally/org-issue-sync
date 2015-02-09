module Sync.Retrieve.GitHub.GitHub where
import Data.Issue
import Data.Maybe
import Data.Char
import qualified Github.Auth as GA
import qualified Github.Issues as GI
import qualified Github.Data.Definitions as GD

rstrip xs = reverse $ lstrip $ reverse xs
lstrip = dropWhile (== ' ')
strip xs = lstrip $ rstrip xs

convertIssue :: String -> GD.Issue -> Issue
convertIssue origin iss =
  let user = case GD.issueAssignee iss of
        Nothing -> GD.issueUser iss
        Just us -> us
      userName = GD.githubOwnerLogin user
      tags = map GD.labelName $ GD.issueLabels iss
      isClosed = isJust $ GD.issueClosedAt iss
      isActive = any (== "T:Active") tags
      status = if isClosed
               then Closed
               else if isActive
                    then Active
                    else Open
      cleanChar c
        | isAlphaNum c = c
        | otherwise = '_'
      cleanTag tag = map cleanChar tag
      cleanTags = map cleanTag tags
  in Issue origin (GD.issueNumber iss) userName status cleanTags (strip $ GD.issueTitle iss) "github"

fetch :: Maybe String -> String -> String -> Maybe IssueStatus -> [String] -> IO [Issue]
fetch tok user repo stat tags = do
  let auth = case tok of
        Nothing -> Nothing
        Just s -> Just $ GA.GithubOAuth s
      statusLim = case stat of
        Just Open -> [GI.Open]
        Just Closed -> [GI.OnlyClosed]
        _ -> []
      tagLim = if length tags > 0
               then [GI.Labels tags]
               else []
  res <- GI.issuesForRepo' auth user repo (statusLim++tagLim)
  case res of
    Left err -> do putStrLn $ show err
                   return []
    Right issues -> return $ map (convertIssue (user++"/"++repo)) issues
