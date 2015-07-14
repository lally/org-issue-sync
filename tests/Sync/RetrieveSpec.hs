module Sync.RetrieveSpec where

import Sync.Retrieve
import Data.Issue
import Data.OrgMode
import Data.List (intercalate)
import Data.Time.Calendar
import Data.Time.Clock

import Control.Exception (assert)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))
import Text.Printf

data SetSource = SetSource { sAuth :: Maybe String, sIssues ::  [Issue] }
  deriving (Eq, Show)

makeUtcTime = UTCTime (ModifiedJulianDay 0) 0

detailsPulledMarker =
  IssueEvent (makeUtcTime) "test" (IssueComment "DETAILS_UPDATED")
issueDetailsWereFetched iss = detailsPulledMarker `elem` (events iss)
getDetails iss =
  assert (not (issueDetailsWereFetched iss)) $ iss {
    events = detailsPulledMarker:(events iss) }

instance LoadableSource SetSource where
  isMember iss src =
    src `elem` (sIssues iss)
  fetchList auth src =
    return $ assert (auth == (sAuth src)) (sIssues src)
  fetchDetails auth src iss = do
    let validIssueAndAuth  = auth == (sAuth src) && iss `elem` (sIssues src)
    return $ assert validIssueAndAuth $ Just $ getDetails iss

sI n sum = Issue "SetSource" n "testuser" Open [] sum "test" "" []

simpleNewItems = SetSource Nothing [sI 1 "foo", sI 2 "bar", sI 3 "baz"]
simpleIssueFile = IssueFile "" (OrgDocView [] (OrgDoc [] [] []))

spec :: Spec
spec = do
  describe "gets new issues" $ do
    it "returns new issues fetched" $ do
      nrItems <- countNew Nothing [] simpleNewItems
      nrItems `shouldBe` 3
    it "only loads them once" $ do
      let repeats = map FetchedIssue $ take 1 $ sIssues simpleNewItems
      nrItems <- countNew Nothing repeats simpleNewItems
      nrItems `shouldBe` 3

  describe "merges issues" $ do
    it "marks merges" $ do
      let markLoaded iss = LoadedIssue True simpleIssueFile iss
          loaded = map markLoaded $ take 1 $ sIssues simpleNewItems
      nrItems <- countMerge Nothing loaded simpleNewItems True
      nrItems `shouldBe` 1
    it "ignores stub or archived issues" $ do
      let markLoaded iss = LoadedIssue False simpleIssueFile iss
          loaded = map markLoaded $ take 1 $ sIssues simpleNewItems
      nrItems <- countLoaded Nothing loaded simpleNewItems
      nrUpdatedItems <- countMerge Nothing loaded simpleNewItems True
      nrItems `shouldBe` 1
      nrUpdatedItems `shouldBe` 0

-- TODO(lally): not sure if I still need this.
--describe "updates orphans" $ do
--    it "updates orphans" $ undefined
--    it "ignores stub or archived orphans" $ undefined

countPred :: (LoadableSource s) => (InputIssue -> Bool) -> Maybe String ->
             [InputIssue] -> s -> IO (Int)
countPred pred auth existing src = do
  gotten <- loadSource auth existing src
  return $ length $ filter pred gotten

countNew :: (LoadableSource s) => Maybe String -> [InputIssue] -> s -> IO (Int)
countNew =
  let isNew (FetchedIssue _) = True
      isNew _ = False
  in countPred isNew

-- The last Bool argument is whether it should count updated issues,
-- or non-updated issues.
countMerge :: (LoadableSource s) => Maybe String -> [InputIssue] -> s -> Bool -> IO (Int)
countMerge a e s u =
  let isMerge (MergedIssue i _ _ _) = u == issueDetailsWereFetched i
      isMerge _ = False
  in countPred isMerge a e s

countLoaded :: (LoadableSource s) => Maybe String -> [InputIssue] -> s -> IO (Int)
countLoaded a e s =
  let isLoaded (LoadedIssue _ _ _) = True
      isLoaded _ = False
  in countPred isLoaded a e s

