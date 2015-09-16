{-# LANGUAGE OverloadedStrings #-}
module Sync.Retrieve.Google.Tasks where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Hashable
import Data.Issue
import Data.Maybe
import Data.Monoid
--import Data.Time
import Debug.Trace
import Network.Google.OAuth2
import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)
import Text.Regex.Posix
import Data.Time.Locale.Compat

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Time.Clock as CLK
import qualified Data.Time.Clock.POSIX as CP
import qualified Data.Time.LocalTime as LT
import qualified Data.Time.Format as TF

data RFCTime = RFCTime CLK.UTCTime deriving (Eq, Show)
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

data TaggedPattern = TaggedPattern String [String] deriving (Eq, Show)
data TaggedList = TaggedList TaskList [String] deriving (Eq, Show)

tlList (TaggedList t _) = t
tlTags (TaggedList _ ts) = ts

fromRfc3399 :: T.Text -> RFCTime
fromRfc3399 str =
  -- Formats pulled from Data.Time.RFC3399, which didn't compile for me.
  let formats = ["%FT%TZ", "%FT%T%z", "%FT%T%Q%z", "%FT%T%QZ"]
      tryParse fmt = TF.parseTime defaultTimeLocale fmt $ T.unpack str
      parse_successes = filter isJust $ map tryParse formats
  in if length parse_successes > 0
     then RFCTime $ LT.zonedTimeToUTC $ fromJust $ head parse_successes
     else trace ("Failed parse of " ++ (T.unpack str)) $ RFCTime $ CP.posixSecondsToUTCTime (fromIntegral 0)

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

-- | Returns lists for the given user and list-regexs.
getLists :: Maybe FilePath ->
            OAuth2Client ->
            [(TaskList -> Maybe [String])] ->
            IO ([TaggedList])
getLists tokenpath client matchers = do
  let applyMatchers :: TaskList -> Maybe TaggedList
      applyMatchers list =
        let applyMatcher l m = do  -- Maybe monad
              tags <- m l
              return $ TaggedList l tags
            applications = mapMaybe (applyMatcher list) matchers
        in if length applications > 0
           then Just $ TaggedList list $ concatMap tlTags applications
           else Nothing
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
      let matches :: [TaggedList]
          matches = mapMaybe applyMatchers lists
          matchTitles = map (tlTitle . tlList) matches
          cnt = show $ length matchTitles
      return  matches
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
  request <-
    parseUrl ("https://www.googleapis.com/tasks/v1/lists/" ++ listid ++
              "/tasks")
  response <- withManager $ httpLbs $ authorize token request
  let body = responseBody response
  let list = decode body :: Maybe Tasks
  return $ maybe [] (\l -> map (convertTask user)  $ tsTasks l) list

