{-# LANGUAGE OverloadedStrings #-}
module Sync.Retrieve.Google.Tasks where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Time.Clock.POSIX
import Data.Issue
import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Time.LocalTime
import Network.Google.OAuth2
import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)
import System.Locale (defaultTimeLocale)
import Text.Regex.Posix

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Vector as V
import qualified Data.Text as T

data RFCTime = RFCTime UTCTime deriving (Eq, Show)
getTime (RFCTime t) = t

data Task = Task
            { tId :: String
            , tTitle :: String
            , tPosition :: Int
            , tUpdated :: RFCTime
            , tNotes :: Maybe String
            , tStatus :: IssueStatus
            , tCompleted :: Maybe RFCTime
            , tDue :: Maybe RFCTime
            } deriving (Eq, Show)

data TaskList = TaskList
              { tlId :: String
              , tlTitle :: String
              , tLUpdated :: RFCTime
              , tlTasks :: [Task]
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
     else RFCTime $ posixSecondsToUTCTime (fromIntegral 0)

instance FromJSON IssueStatus where
  parseJSON (String s) =
    case s of
      "needsAction" -> return Open
      "completed" -> return Closed
      otherwise -> mzero
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
       v .:? "due"
  parseJSON _ = mzero

instance FromJSON TaskList where
  parseJSON (Object v) = TaskList <$>
                         v .: "id" <*>
                         v .: "title" <*>
                         v .: "updated" <*>
                         v .: "tasks"
  parseJSON _ = mzero

instance FromJSON TaskListList where
  parseJSON (Array a) = do
    let lists = V.toList a
    args <- mapM parseJSON lists
    return $ TaskListList args
  parseJSON _ = mzero

setupToken :: Maybe FilePath -> OAuth2Client -> IO OAuth2Token
setupToken tokenpath client = do
    let scopes = ["https://www.googleapis.com/auth/tasks.readonly"]
    token <- getAccessToken client scopes Nothing
    return token

authorize token request = request
       { requestHeaders = [(hAuthorization, B8.pack $ "Bearer " <> token)] }

compilePattern :: String -> (TaskList -> Bool)
compilePattern ('+':rest) = \task -> tlId task == rest
compilePattern pat =
  \task -> pat =~ tlTitle task

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
  let (Just lists) = decode $ responseBody response
      applyMatcher :: TaskList -> (TaskList -> Bool) -> Bool
      applyMatcher list m = m list
      checkList :: TaskList -> Bool
      checkList list = any (applyMatcher list) matchers
      matches = filter checkList lists
  return $ map tlTitle matches

issue_user = "google-tasks"

convertTask :: Task -> Issue
convertTask t =
  let completionEvent = IssueEvent <$> (fmap getTime $ tCompleted t)
                        <*> (pure issue_user)
                        <*> (pure $ IssueStatusChange Closed)
      note = IssueComment <$> tNotes t
      notesEvent = IssueEvent <$> (pure $ getTime $ tUpdated t)
                   <*> (pure issue_user)
                   <*> note
      events = catMaybes [notesEvent, completionEvent]
  in Issue (tId t) 0 issue_user (tStatus t) [] (tTitle t) "google-tasks" events

getList :: Maybe FilePath -> OAuth2Client -> String -> IO ([Issue])
getList tokenpath client listid = do
  token <- setupToken tokenpath client
  request <- parseUrl ("https://www.googleapis.com/tasks/v1/users/@me/lists/" ++ listid)
  response <- withManager $ httpLbs $ authorize token request
  let (Just list) = decode $ responseBody response
  return $ map convertTask (tlTasks list)
