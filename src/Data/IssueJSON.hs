{-# LANGUAGE OverloadedStrings #-}

module Data.IssueJSON where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Issue
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HM

instance FromJSON IssueStatus where
  parseJSON (String s) =
    case s of
      "open" -> return Open
      "active" -> return Active
      "closed" -> return Closed
      otherwise -> mzero
  parseJSON _ = mzero

instance ToJSON IssueStatus where
  toJSON s = toJSON $ map toLower $ show s

instance FromJSON Issue where
  parseJSON (Object v) = Issue <$>
                         v .: "origin" <*>
                         v .: "number" <*>
                         v .: "user" <*>
                         v .: "status" <*>
                         v .: "tags" <*>
                         v .: "summary" <*>
                         v .: "type" <*>
                         v .: "events"
  parseJSON _ = mzero

instance ToJSON Issue where
    toJSON (Issue o nu us st tgs smm ty evts) = object [
      "origin" .= o, "number" .= nu, "user" .= us,
      "status" .= st, "tags" .= tgs, "summary" .= smm,
      "type" .= ty, "events" .= evts]

instance FromJSON IssueEvent where
  parseJSON (Object v) = IssueEvent <$>
                         v .: "when" <*>
                         v .: "user" <*>
                         v .: "details"
  parseJSON _ = mzero

instance ToJSON IssueEvent where
  toJSON (IssueEvent w u dt) = object [
    "when" .= w, "user" .= u, "details" .= dt ]

instance FromJSON IssueEventDetails where
  parseJSON (Object v) =
    case HM.lookup "type" v of
      Just "status" -> IssueStatusChange <$> v.: "newStatus"
      Just "comment" -> IssueComment <$> v .: "comment"
      Just "owner" -> IssueOwnerChange <$> v .: "new"
      Just "label" -> IssueLabelChange
                      <$> v .:? "new" .!= []
                      <*> v .:? "removed" .!= []
      Just "milestone" -> IssueMilestoneChange
                          <$> v .:? "new"
                          <*> v .:? "old"
      otherwise -> mzero

instance ToJSON IssueEventDetails where
  toJSON (IssueStatusChange ns) = object [ ("type", "status"), "newStatus" .= ns ]
  toJSON (IssueComment cmm) = object [ ("type" , "comment"), "comment" .= cmm ]
  toJSON (IssueOwnerChange no) = object [ ("type" , "owner"), "new" .= no ]
  toJSON (IssueLabelChange nw rm) =
    let news = if length nw > 0 then ["new" .= nw] else []
        rems = if length rm > 0 then ["removed" .= rm] else []
    in object $ [("type" , "label")] ++ (news ++ rems)
  toJSON (IssueMilestoneChange new old) =
    let cns label val = maybe [] (\s -> [label .= s]) val
    in object $ [("type", "milestone")] ++  ((cns "new" new) ++ (cns "old" old))

