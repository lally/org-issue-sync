module Sync.Issue where
import Data.Maybe (mapMaybe)
import Data.Ord
import Data.Hashable
import Data.Bits

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

instance Ord Issue where
  (<=) l r =  (iType l <= iType r) && (origin l <= origin r) && (
    number l <= number r)

instance Hashable Issue where
  hashWithSalt salt iss =
    (hashWithSalt salt $ origin iss) `xor` (
      hashWithSalt salt $ number iss) `xor` (
      hashWithSalt salt $ iType iss)

-- | Updates between issues.  Probably useless.
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
