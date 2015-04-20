module Sync.Retrieve where
import Data.Hashable
import Data.OrgMode
import Data.Issue
import Data.List
import qualified Data.HashMap.Strict as HM

import qualified Sync.Retrieve.Google.Code as GC
import qualified Sync.Retrieve.GitHub.GitHub as GH

data GoogleCodeSource = GoogleCodeSource
                        { gcRepo :: String
                        , gcSearchTerms :: [String]
                        } deriving (Eq, Show)

data GitHubSource = GitHubSource
                    { ghUser :: String
                    , ghProject :: String
                    , ghTagLists :: [String]
                    } deriving (Eq, Show)

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
                              , liFile:: IssueFile
                              , liIssue:: Issue }
                  -- |An issue that was loaded from an org file, then
                  -- updated from the network.
                | MergedIssue { fetchedIssue :: Issue
                              , loadedIssue :: Issue
                              , loadedFile :: IssueFile
                              , loadedUpdate :: Bool }
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

-- Only allow this sort (Fetched + Loaded) of merging, the rest are
-- all indicators of bugs.
mergeIssue :: InputIssue -> InputIssue -> InputIssue
mergeIssue (FetchedIssue new_iss) (LoadedIssue upd file old_iss) =
  MergedIssue new_iss old_iss file upd

update_set :: HM.HashMap Issue InputIssue -> InputIssue -> HM.HashMap Issue InputIssue
update_set set iss =
  let k_iss = issueof iss
  in case HM.lookup k_iss set of
    Just old_iss -> HM.insert k_iss (mergeIssue iss old_iss) set
    Nothing -> set
match_set :: HM.HashMap Issue InputIssue -> InputIssue -> HM.HashMap Issue InputIssue
match_set set iss =
  let k_iss = issueof iss
  in case HM.lookup k_iss set of
    Just old_iss -> HM.insert k_iss (mergeIssue iss old_iss) set
    Nothing -> set

fetched (_, LoadedIssue _ _ _) = False
fetched _ = True

want_details (FetchedIssue _) = True
want_details (MergedIssue _ _ _ upd) = upd
-- So, these are issues we didn't find in a fetch.  Do we want
-- to keep getting any details on them at all?  If they're in a
-- stub file, we may still want to know if they're closed,
-- right?  Yes.
want_details (LoadedIssue upd _ _) = upd

mergeFetchedWithLoaded :: [InputIssue] -> [InputIssue] -> [InputIssue]
mergeFetchedWithLoaded existing new =
  let paired_issues = map (\i -> (issueof i, i)) existing
  in map snd $ HM.toList $ foldl update_set (HM.fromList paired_issues) new

loadGCSource :: [InputIssue] -> GoogleCodeSource -> IO ([InputIssue])
loadGCSource existing src = do
  let (GoogleCodeSource repo terms) = src
  loaded <- GC.fetch repo terms
  return $ mergeFetchedWithLoaded existing $ map FetchedIssue loaded

-- |Load GitHub sources, given an |oauth| token, a GitHubSource
-- (|src|), and a list of existing issues.  Returns an updated version
-- of |existing| with MergedIssue elements replacing LoadedIssues when
-- we have new data, and FetchedIssue added for new issues.
loadGHSource :: Maybe String -> [InputIssue] -> GitHubSource -> IO ([InputIssue])
loadGHSource oauth existing src = do
  let (GitHubSource user project tags) = src
      origin_str = user ++ "/" ++ project
      (gh_issues, others) = partition (
        \i -> let iss = issueof i
              in iType iss == "github" && origin iss == origin_str) existing

  raw_all_open <- GH.fetch oauth user project (Just Open) tags
  let all_open = map FetchedIssue raw_all_open
      paired_issues = map (\i -> (issueof i, i)) gh_issues
      open_merged = foldl update_set (HM.fromList paired_issues) all_open
      missing_from_fetch = filter (not . fetched) $ HM.toList open_merged
  -- Won't see issues that we'd loaded before that have since CLOSED.
  -- Do a second fetch here, (Just Closed), with the tags, to see if
  -- any of the ones in |existing| that weren't in the first fetch
  -- show up in the second.
  raw_all_closed <- GH.fetch oauth user project (Just Closed) tags

  let all_closed = map FetchedIssue raw_all_closed
      closed_merged = foldl match_set (HM.fromList missing_from_fetch) all_closed
      (to_update, left_alone) =
        partition want_details $
        map snd ((HM.toList open_merged) ++ (HM.toList closed_merged))
      updateIssue (FetchedIssue _) iss = FetchedIssue iss
      updateIssue (MergedIssue f li lf lup) iss = MergedIssue iss li lf lup
      -- We may want to make this a policy option: should we get some
      -- updates on issues we'e discovered that no longer fit our
      -- fetch (e.g., tags) criteria?  Or are they definitionally
      -- irrelevant?
      updateIssue (LoadedIssue upd fl _) iss = LoadedIssue upd fl iss
      getDetails inp_iss = do
        let iss = issueof inp_iss
            (user, slashproj) = break (== '/') $ origin iss
            proj = drop 1 slashproj
        raw_iss <- GH.fetchDetails oauth user project iss
        return $ updateIssue inp_iss raw_iss
  updated <- mapM getDetails to_update
  return $ updated ++ left_alone ++ others

{-
Ok, what goes here?

Centralize the merge process.  I'll have to figure out if InputIssue
goes here, and if not, where I pull the relevant data out.

Perhaps I pass a list of issues *not* to get data about?  A 'light'
update list?  That'd let us return the full list, but the results have
to be interpreted.  Perhaps a pair (light, full)?  But the light list
still has to get matched up, in order to update the tags/node status.
Also, those would have to avoid updating the ISSUE STATUS child.

Perhaps the logic should look at events and if empty, just leave ISSUE
EVENTS in place?  Ugh.  There's some ugly noise here I'd like to clean
up more, semantically.

-}
