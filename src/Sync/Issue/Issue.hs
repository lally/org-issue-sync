module Sync.Issue.Issue where

data IssueStatus = Open | Active | Closed deriving (Eq, Show)

data Issue = Issue
             { origin :: String
             , number :: Int
             , user :: String
             , status :: IssueStatus
             , tags :: [String]
             , summary :: String
             } deriving (Eq, Show)

