module Sync.RetrieveSpec where

import Sync.Retrieve
import Data.Issue
import Data.Time.Calendar
import Data.Time.Clock

import Control.Exception (assert)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))
--import Test.QuickCheck.Gen (elements)
--import Test.QuickCheck.Modifiers (getPositive)
import Text.Printf

data SetSource = SetSource { sAuth :: Maybe String, sIssues ::  [Issue] }
  deriving (Eq, Show)

makeUtcTime = UTCTime (ModifiedJulianDay 0) 0

updatedMarker = IssueEvent (makeUtcTime) "test" (IssueComment "UPDATED")
issueWasUpdated iss = updatedMarker `elem` (events iss)
updateIssue iss =
  assert (not (issueWasUpdated iss)) $ iss { events = updatedMarker:(events iss)}
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

sI n sum = Issue "SetSource" n "testuser" Open [] sum "test" []

simpleNewItems = SetSource Nothing [sI 1 "foo", sI 2 "bar", sI 3 "baz"]

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
{--
  describe "merges issues" $ do
    it "marks merges" $ undefined
    it "ignores stub or archived issues" $ undefined

  describe "updates orphans" $ do
    it "updates orphans" $ undefined
    it "ignores stub or archived orphans" $ undefined
--}
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

countMerge :: (LoadableSource s) => Maybe String -> [InputIssue] -> s -> IO (Int)
countMerge a e s =
  let isMerge (MergedIssue _ _ _ _) = True
      isMerge _ = False
  in countPred isMerge a e s

countLoaded :: (LoadableSource s) => Maybe String -> [InputIssue] -> s -> IO (Int)
countLoaded a e s =
  let isLoaded (LoadedIssue _ _ _) = True
      isLoaded _ = False
  in countPred isLoaded a e s

