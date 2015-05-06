module Sync.Retrieve where
import Control.Monad
import Data.Hashable
import Data.OrgMode
import Data.Issue
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Sync.Retrieve.Google.Code as GC
import qualified Sync.Retrieve.GitHub.GitHub as GH

class LoadableSource s where
  isMember :: s -> Issue -> Bool
  fetchList :: Maybe String -> s -> IO ([Issue])
  fetchDetails :: Maybe String -> s -> Issue -> IO (Maybe Issue)

data GoogleCodeSource = GoogleCodeSource
                        { gcRepo :: String
                        , gcSearchTerms :: [String]
                        } deriving (Eq, Show)

instance LoadableSource GoogleCodeSource where
  isMember (GoogleCodeSource repo tags) iss =
    iType iss == "googlecode" && origin iss == repo
  fetchList oauth (GoogleCodeSource repo tags) =
    GC.fetch repo tags
  fetchDetails oauth (GoogleCodeSource repo tags) iss = do
    details <- GC.fetchDetails (origin iss) (number iss)
    return $ Just iss { events = details }

data GitHubSource = GitHubSource
                    { ghUser :: String
                    , ghProject :: String
                    , ghTags :: [String]
                    } deriving (Eq, Show)

instance LoadableSource GitHubSource where
  isMember ghs iss =
    iType iss == "github" &&
    (ghUser ghs ++ "/" ++ ghProject ghs) == origin iss
  fetchList oauth ghs =
    GH.fetch oauth (ghUser ghs) (ghProject ghs) (Just Open) (ghTags ghs)
  fetchDetails oauth ghs iss =
    liftM Just $ GH.fetchDetails oauth (ghUser ghs) (ghProject ghs) iss

data GoogleTasksSource = GoogleTasksSource
                         { gtUser :: String
                         , gtOAuthFile :: Maybe FilePath
                           -- ^ May be inherited.
                         , gtListPatterns :: [String]
                         } deriving (Eq, Show)

-- TODO(lally): Re-arch GoogleTasksSource to fit the
-- fetchList/fetchDetails model.  instance LoadableSource
-- GoogleTasksSource where


data IssueFile = IssueFile
                 { ifPath :: FilePath
                 , ifDoc :: OrgDocView Issue }

instance Eq IssueFile where
  a == b = ifPath a == ifPath b

instance Ord IssueFile where
  compare a b = compare (ifPath a) (ifPath b)

instance Show IssueFile where
  show = ifPath

-- |We get issues from multiple sources, track that, and their states.
data InputIssue = FetchedIssue { fiIssue :: Issue } -- ^From a network source.
                  -- |An issue loaded from an org file.
                | LoadedIssue { liUpdate:: Bool
                                -- ^Whether to update
                              , liFile:: IssueFile
                              , liIssue:: Issue }
                  -- |An issue that was loaded from an org file, then
                  -- updated from the network.
                | MergedIssue { fetchedIssue :: Issue
                              , loadedIssue :: Issue
                              , loadedFile :: IssueFile
                              , loadedUpdate :: Bool
                                -- ^If it's been updated with details.
                              }
                deriving (Show)

issueof :: InputIssue -> Issue
issueof (FetchedIssue fi) = fi
issueof (LoadedIssue _ _ is) = is
issueof (MergedIssue fi _ _ _) = fi

instance Hashable InputIssue where
  hashWithSalt i s = hashWithSalt i (issueof s)

instance Eq InputIssue where
  a == b = issueof a == issueof b

eqIssue a b = issueof a == issueof b

instance Ord InputIssue where
  compare a b = compare (issueof a) (issueof b)


-- |Updates an existing issue in a set of them, merging in |iss|.
updateSet :: HM.HashMap Issue InputIssue -> InputIssue -> HM.HashMap Issue InputIssue
updateSet set iss =
  let kIss = issueof iss
      -- Combine an issue we found in this fetch run with one we found
      -- before in a file load.  We shouldn't get Merged or Fetched
      -- issues on the RHS, as they should've been filtered out before
      -- we got here.
      mergeIssue (FetchedIssue newIss) (LoadedIssue upd file oldIss) =
        MergedIssue newIss oldIss file upd
  in case HM.lookup kIss set of
    Just oldIss -> HM.insert kIss (mergeIssue iss oldIss) set
    Nothing -> set

wantDetails (FetchedIssue _) = True
wantDetails (MergedIssue _ _ _ upd) = upd
-- So, these are issues we didn't find in a fetch.  Do we want
-- to keep getting any details on them at all?  If they're in a
-- stub file, we may still want to know if they're closed,
-- right?  Yes.
wantDetails li@(LoadedIssue upd _ iss) =
  upd && wantAnyUpdate li

-- Whether we should even touch the file with the issue in it.  This
-- is true for everything but a file with an :ARCHIVE: tag on it
wantAnyUpdate (LoadedIssue _ _ iss) =
  not ("ARCHIVE" `elem` (tags iss))
wantAnyUpdate _ = True

mergeFetchedWithLoaded :: [InputIssue] -> [InputIssue] -> [InputIssue]
mergeFetchedWithLoaded existing new =
  let pairedIssues = map (\i -> (issueof i, i)) existing
  in map snd $ HM.toList $ foldl updateSet (HM.fromList pairedIssues) new

loadGCSource :: [InputIssue] -> GoogleCodeSource -> IO ([InputIssue])
loadGCSource existing src = do
  let (GoogleCodeSource repo terms) = src
  loaded <- GC.fetch repo terms
  return $ mergeFetchedWithLoaded existing $ map FetchedIssue loaded

previouslyFetched (FetchedIssue _) = True
previouslyFetched (MergedIssue _ _ _ _) = True
previouslyFetched _ = False

fetchIssue :: (LoadableSource s) => s -> Maybe String -> InputIssue -> IO (InputIssue)
fetchIssue s oauth (FetchedIssue iss) = do
  updIss <- fetchDetails oauth s iss
  return $ FetchedIssue (fromJust updIss)
fetchIssue s oauth (LoadedIssue True file iss) = do
  updIss <- fetchDetails oauth s iss
  return $ LoadedIssue True file (fromJust updIss)
fetchIssue s oauth (MergedIssue fi ld fl False) = do
  updIss <- fetchDetails oauth s fi
  return $ MergedIssue (fromJust updIss) (fromJust updIss) fl True

-- |Load GitHub sources, given an |oauth| token, a GitHubSource
-- (|src|), and a list of existing issues.  Returns an updated version
-- of |existing| with MergedIssue elements replacing LoadedIssues when
-- we have new data, and FetchedIssue added for new issues.
loadSource :: (LoadableSource s) => Maybe String -> [InputIssue] -> s -> IO ([InputIssue])
loadSource oauth allExisting src = do
  let (existing, nonMembers) = partition (\i -> isMember src $ issueof i) allExisting
  incoming <- mapM (return . FetchedIssue) =<< fetchList oauth src
  -- Break issues up into dup, new, merge, and orphans.
  -- We come in with a set of Existing issues, which get categorized
  -- into Merge or Orphan
  -- New: Returned as FetchedIssue, with details.
  -- Dup: Came in as a FetchedIssue, and we don't need to do anything to it.
  -- Merge: Fetch details if |wantDetails|
  -- Orphan: Do full fetch if |wantAnyUpdate|.  We still get
  --         details, but have a lower bar for doing so.
  let dup = intersect incoming $ filter previouslyFetched existing
      new = incoming \\ existing
      (merges, orphans) =
        partition previouslyFetched $ map snd combined
        where
          combined = HM.toList $ foldl updateSet pExisting (incoming \\ dup)
          pExisting = HM.fromList $ map (\i -> (issueof i, i)) existing
      (updOrphans, ignOrphans) = partition wantDetails orphans
  fetchedNew <- mapM (fetchIssue src oauth) new
  fetchedMerges <- mapM (fetchIssue src oauth) merges
  fetchedOrphans <- mapM (fetchIssue src oauth) orphans
  return $ (fetchedNew ++ fetchedMerges ++ fetchedOrphans ++ dup ++
            ignOrphans ++ nonMembers)
