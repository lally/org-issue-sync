{-# LANGUAGE ScopedTypeVariables #-}
module Sync.Retrieve.GitHub.GitHub where
import Control.Exception
import Data.Issue
import Data.Maybe
import Data.Char
import Data.List (sort)
import Debug.Trace
import Network.HTTP.Conduit (HttpException(..))
import Network.HTTP.Types (statusCode, statusMessage)
import qualified Github.Auth as GA
import qualified Github.Issues as GI
import qualified Github.Issues.Events as GIE
import qualified Github.Issues.Comments as GIC
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
  in Issue origin (GD.issueNumber iss) userName status cleanTags (strip $ GD.issueTitle iss) "github" []

wrapEvent :: GD.Event -> IssueEventDetails -> IssueEvent
wrapEvent event details =
  IssueEvent (GD.fromGithubDate $ GD.eventCreatedAt event) (
                    GD.githubOwnerLogin $ GD.eventActor event) details

convertEvent :: GD.Event -> IssueEventDetails
convertEvent evt = IssueComment (show evt)

convertIssueEvent :: GD.Event -> [IssueEvent]
convertIssueEvent event
-- status change
  | (GD.eventType event) == GD.Assigned = [
    wrapEvent event $ IssueOwnerChange (GD.githubOwnerLogin $ GD.eventActor event)]
  | (GD.eventType event) == GD.Closed = [
    wrapEvent event $ IssueStatusChange Closed]
  | (GD.eventType event) == GD.ActorUnassigned = [
    wrapEvent event $ IssueComment "Unassigned owner"]
  | (GD.eventType event) == GD.Reopened = [
    wrapEvent event $ IssueStatusChange Open]
  | (GD.eventType event) == GD.Renamed = [
    wrapEvent event $ IssueComment ("Changed title")]
-- label change 
  | (GD.eventType event) == GD.Labeled = [
    wrapEvent event $ IssueComment ("Added a label")]
  | (GD.eventType event) == GD.Unlabeled = [
    wrapEvent event $ IssueComment ("Removed a label")]
-- milestone change 
  | (GD.eventType event) == GD.Milestoned =
    let mstone =
          case GD.eventIssue event of
            Just evt ->
              case GD.issueMilestone evt of
                Just ms -> " " ++ GD.milestoneTitle ms
                Nothing -> ""
            Nothing -> ""
    in [wrapEvent event $ (IssueComment ("Added milestone" ++ mstone))]

  | (GD.eventType event) == GD.Demilestoned = [
    wrapEvent event $ IssueComment "Removed a milestone"]
-- ignored, make into comment
  | otherwise = [wrapEvent event $ (IssueComment (show event))]

convertIssueComment :: GD.IssueComment -> [IssueEvent]
convertIssueComment comment =
  [IssueEvent (GD.fromGithubDate $ GD.issueCommentCreatedAt comment) (
      GD.githubOwnerLogin $ GD.issueCommentUser comment) (
      IssueComment (GD.issueCommentBody comment))]

loadIssueComments :: Maybe GA.GithubAuth -> String -> String -> Int -> IO [IssueEvent]
loadIssueComments oauth user repo num = do
  res <- GIC.comments' oauth user repo num
  case res of
    Left err -> do
      putStrLn (user ++ "/" ++ repo ++ ": issue " ++ (
                   show num) ++ ": " ++ show err)
      return []
    Right comments ->
      return $ concatMap convertIssueComment comments

loadIssueEvents :: Maybe GA.GithubAuth -> String -> String -> GD.Issue -> IO [IssueEvent]
loadIssueEvents oauth user repo iss = do
  let classifyError (GD.HTTPConnectionError ex) =
        case (fromException ex) of
          Just (StatusCodeException st _ _) -> "HTTP Connection Error " ++
                                               show (statusCode st) ++ ": " ++
                                               show (statusMessage st)
          _ -> "HTTP Connection Error (unknown status code): " ++ show ex
      classifyError err = show err
  res <- GIE.eventsForIssue user repo (GD.issueNumber iss)
  case res of
    Left err -> do
      putStrLn (user ++ "/" ++ repo ++ ": issue " ++ (
                   show $ GD.issueNumber iss) ++ ": " ++ classifyError err)
      return []
    Right events ->
      return $ concatMap convertIssueEvent events

makeIssueComment :: GD.Issue -> IssueEvent
makeIssueComment issue =
  let user = case GD.issueAssignee issue of
        Nothing -> GD.issueUser issue
        Just us -> us
      userName = GD.githubOwnerLogin user
      createDate = GD.fromGithubDate $ GD.issueCreatedAt issue
  in IssueEvent createDate userName (IssueComment (maybe "" id (GD.issueBody issue)))

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
    Left err -> do
      putStrLn $ show err
      return []
    Right issues -> do
      eventList <- mapM (loadIssueEvents auth user repo) issues
      commentList <-
        mapM (\i -> loadIssueComments auth user repo (GD.issueNumber i)) issues
      let convertedIssues = map (convertIssue (user++"/"++repo)) issues
          comments = map makeIssueComment issues
          conversions =
            zip convertedIssues $ zip3 comments eventList commentList
      return $
        map (\(i,(comm,es,cs)) -> i { events = comm:(sort es++cs) }) conversions
