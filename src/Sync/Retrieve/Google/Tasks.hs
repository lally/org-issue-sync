{-# LANGUAGE OverloadedStrings #-}
module Sync.Retrieve.Google.Tasks where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Time.Clock.POSIX
import Data.Hashable
import Data.Issue
import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Time.LocalTime
import Debug.Trace
import Network.Google.OAuth2
import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)
import System.Locale (defaultTimeLocale)
import Text.Regex.Posix

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Text as T

data RFCTime = RFCTime UTCTime deriving (Eq, Show)
getTime (RFCTime t) = t

data Task = Task
            { tId :: String
            , tTitle :: String
            , tPosition :: String
            , tUpdated :: RFCTime
            , tNotes :: Maybe String
            , tStatus :: IssueStatus
            , tCompleted :: Maybe RFCTime
            , tDue :: Maybe RFCTime
            , tUrl :: String
            } deriving (Eq, Show)

data Tasks = Tasks { tsTasks :: [Task] } deriving (Eq, Show)


data TaskList = TaskList
              { tlId :: String
              , tlTitle :: String
              , tlUpdated :: RFCTime
              } deriving (Eq, Show)

data TaskListList = TaskListList [TaskList] deriving (Eq, Show)

fromRfc3399 :: T.Text -> RFCTime
fromRfc3399 str =
  -- Formats pulled from Data.Time.RFC3399, which didn't compile for me.
  let formats = ["%FT%TZ", "%FT%T%z", "%FT%T%Q%z", "%FT%T%QZ"]
      tryParse fmt = parseTime defaultTimeLocale fmt $ T.unpack str
      parse_successes = filter isJust $ map tryParse formats
  in if length parse_successes > 0
     then RFCTime $ zonedTimeToUTC $ fromJust $ head parse_successes
     else trace ("Failed parse of " ++ (T.unpack str)) $ RFCTime $ posixSecondsToUTCTime (fromIntegral 0)

instance FromJSON IssueStatus where
  parseJSON (String s) =
    case s of
      "needsAction" -> return Open
      "completed" -> return Closed
      otherwise -> trace ("Failed parsing issue status.") $ mzero
  parseJSON _ = mzero

instance FromJSON RFCTime where
  parseJSON (String s) = return $ fromRfc3399 s
  parseJSON _ = mzero

instance FromJSON Task where
  parseJSON (Object v) = Task <$>
       v .: "id" <*>
       v .: "title" <*>
       v .: "position" <*>
       v .: "updated" <*>
       v .:? "notes" <*>
       v .: "status" <*>
       v .:? "completed" <*>
       v .:? "due" <*>
       v .: "selfLink"
  parseJSON _ = mzero

instance FromJSON TaskList where
  parseJSON (Object v) = TaskList <$>
                         v .: "id" <*>
                         v .: "title" <*>
                         v .: "updated"
  parseJSON _ = mzero

instance FromJSON TaskListList where
  parseJSON (Object v) = do
    let (Array vec) = HM.lookupDefault (Array V.empty) "items" v
        lists = V.toList vec
    args <- mapM parseJSON lists
    return $ TaskListList args
  parseJSON _ = mzero

instance FromJSON Tasks where
  parseJSON (Object v) = do
    let (Array vec) = HM.lookupDefault (Array V.empty) "items" v
        list = V.toList vec
    args <- mapM parseJSON list
    return $ Tasks args
  parseJSON _ = trace ("FromJSON Tasks: not an object.") $ mzero

setupToken :: Maybe FilePath -> OAuth2Client -> IO OAuth2Token
setupToken tokenpath client = do
    let scopes = ["https://www.googleapis.com/auth/tasks.readonly"]
    token <- getAccessToken client scopes tokenpath
    return token

authorize token request = request
       { requestHeaders = [(hAuthorization, B8.pack $ "Bearer " <> token)] }

compilePattern :: String -> (TaskList -> Bool)
compilePattern ('+':rest) = \task -> tlId task == rest
compilePattern pat =
  \task -> (tlTitle task) =~ pat

-- | Returns lists for the given user and list-regexs.
getLists :: Maybe FilePath -> OAuth2Client -> [String] -> IO ([String])
getLists tokenpath client listregs = do
  let matchers :: [(TaskList -> Bool)]
      matchers = map compilePattern listregs
  token <- setupToken tokenpath client
  request <- parseUrl "https://www.googleapis.com/tasks/v1/users/@me/lists"
  response <- withManager $ httpLbs $ authorize token request

  -- look in lists for those that match a regex.  Go through each list
  -- and see if it matches any of the regexes.
  let body = responseBody response
      decoded :: Maybe TaskListList
      decoded = decode body
  case decoded of
    Just (TaskListList lists) -> do
      let applyMatcher :: TaskList -> (TaskList -> Bool) -> Bool
          applyMatcher list m = m list
          checkList :: TaskList -> Bool
          checkList list = any (applyMatcher list) matchers
          matches = filter checkList lists
          matchTitles = map tlTitle matches
          listIds = map tlId matches
      return $ trace ("Matching list titles " ++ L.intercalate "," matchTitles) listIds
    Nothing -> do
      putStrLn $ "Tasks.getLists: Failed to parse " ++ show body
      return []

issue_user = "google-tasks"

convertTask :: String -> Task -> Issue
convertTask u t =
  let completionEvent = IssueEvent <$> (fmap getTime $ tCompleted t)
                        <*> (pure issue_user)
                        <*> (pure $ IssueStatusChange Closed)
      note = IssueComment <$> tNotes t
      notesEvent = IssueEvent <$> (pure $ getTime $ tUpdated t)
                   <*> (pure issue_user)
                   <*> note
      events = catMaybes [notesEvent, completionEvent]
  in Issue u (abs $ hash $ tId t) u (tStatus t) [] (tTitle t) "google-tasks" (tUrl t) events

getList :: Maybe FilePath -> OAuth2Client -> String -> String -> IO ([Issue])
getList tokenpath client user listid = do
  token <- setupToken tokenpath client
  putStrLn $ "Requesting list id " ++ listid
  request <-
    parseUrl ("https://www.googleapis.com/tasks/v1/lists/" ++ listid ++ "/tasks")
  response <- withManager $ httpLbs $ authorize token request
  let body = responseBody response
  let list = decode body :: Maybe Tasks
  putStrLn $ "getList(" ++ listid ++ "): parsed to " ++ show list
  return $ maybe [] (\l -> map (convertTask user)  $ tsTasks l) list

