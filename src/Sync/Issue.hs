module Sync.Issue where
import Data.Maybe (mapMaybe)
data IssueStatus = Open | Active | Closed deriving (Eq, Show)

data Issue = Issue
             { origin :: String
             , number :: Int
             , user :: String
             , status :: IssueStatus
             , tags :: [String]
             , summary :: String
             , iType :: String
             } deriving (Show)

issueEqual l r =
  (origin l == origin r) && (number l == number r) && (iType l == iType r)

instance Eq Issue where
  (==) l r = issueEqual l r

data IssueDelta = IssueDelta { idProperty :: String
                             , idOldValue :: String
                             , idNewValue :: String } deriving (Eq, Show)

issueDelta :: Issue -> Issue -> [IssueDelta]
issueDelta left right =
  let funcs = [ ("User", user)
              , ("Status", show . status)
              , ("Tags", show . tags)
              , ("Summary", summary) ]
      applyF l r (nm, propFn)  =
        if (propFn l /= propFn r)
        then Just $ IssueDelta nm  (propFn l) (propFn r)
        else Nothing
  in mapMaybe (applyF left right) funcs
