{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- TODO(lally): trim down these exports.
module OrgSync where
import qualified Data.HashMap.Strict as HM
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Sync.Retrieve.GoogleCode.GoogleCode as GC
import qualified Sync.Retrieve.GitHub.GitHub as GH
--import Control.Exception.Base (handle)
import Control.Monad (join)
import Data.List (nub, (\\), sort, sortBy, intersect, groupBy, intercalate, partition)
import Data.Hashable
import Data.Maybe
import System.FilePath.Glob
import Debug.Trace
import Control.Applicative
import Data.OrgMode
import Data.OrgMode.Text
import Data.OrgMode.Doc
import Data.OrgMode.OrgDocView
import Data.Issue
import Data.IssueCache
import Data.OrgIssue
import System.IO
import System.IO.Error

data GoogleCodeSource = GoogleCodeSource
                        { gcRepo :: String
                        , gcSearchTerms :: [String]
                        } deriving (Eq, Show)

data GitHubSource = GitHubSource
                    { ghUser :: String
                    , ghProject :: String
                    , ghTagLists :: [String]
                    } deriving (Eq, Show)

data RunConfiguration = RunConfiguration
                        { rcScanFiles :: [FilePath]
                        , rcStubFiles :: [FilePath]
                        , rcOutputFile :: FilePath
                        , rcGitHubOAuth :: Maybe String
                        , rcGitHubSources :: [GitHubSource]
                        , rcGoogleCodeSources :: [GoogleCodeSource]
                        , rcCache :: Maybe IssueStore
                        } deriving (Eq, Show)

data IssueFile = IssueFile
                 { ifPath :: FilePath
                 , ifDoc :: OrgDocView Issue
                 }

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
                | MergedIssue { miFetchedIssue :: Issue
                              , miLoadedIssue :: Issue }
                deriving (Eq, Show)

issueof :: InputIssue -> Issue
issueof (FetchedIssue fi) = fi
issueof (LoadedIssue _ _ is) = is

instance Hashable InputIssue where
  hashWithSalt i s = hashWithSalt i (issueof s)


-- |Semantics: When not fetching issues, we can still scan the cache for
-- issues, and put them in.  But we may not write new ones out
-- (roWriteNewIssues), or not update existing issues (roUpdateIssues).
data RunOptions = RunOptions
                  { roFetchIssues :: Bool
                  , roWriteNewIssues :: Bool
                  , roVerbose :: Bool
                  , roUpdateIssues :: Bool
                  } deriving (Eq, Show)

getImmediateChildren :: HM.HashMap DCT.Name DCT.Value -> [T.Text]
getImmediateChildren hmap = nub $ map (T.takeWhile (/= '.')) $ HM.keys hmap

getChildrenOf :: HM.HashMap DCT.Name DCT.Value -> T.Text -> [T.Text]
getChildrenOf hmap parent =
  let prefix = parent `T.append` "."
      prefixLen = T.length prefix
      allKeys = HM.keys hmap
      allChildren = filter (T.isPrefixOf prefix) allKeys
  in nub $ map (T.takeWhile (/= '.')) $ map (T.drop prefixLen) $ allChildren

valToList :: DCT.Value -> [String]
valToList (DCT.List els) = map T.unpack $ mapMaybe DCT.convert els
valToList _ = []

getMultiList :: HM.HashMap DCT.Name DCT.Value -> T.Text -> [[String]]
getMultiList config key =
  let res = HM.lookup key config in
  case res of
    Nothing -> []
    -- Look at first element.  If it's a string, convert the key as
    -- [String], otherwise as [[String]]
    Just (DCT.List (x:xs)) ->
      case x of
        DCT.String s ->
          [mapMaybe (\s -> T.unpack <$> DCT.convert s) (x:xs)]
        DCT.List ys ->
          filter (\l -> length l > 0) $ map valToList (x:xs)
        otherwise -> []
    Just (DCT.List []) -> []
    otherwise -> trace ("looking up key " ++ (show key) ++ " results in "
                        ++ show res) []

loadGCSources :: HM.HashMap DCT.Name DCT.Value -> [GoogleCodeSource]
loadGCSources config =
  let repos = getChildrenOf config "google-code.projects"
      -- TODO: make this just [GoogleCodeSource]
      getRepoData :: T.Text -> [GoogleCodeSource]
      getRepoData repo =
        let rPlus s = repo `T.append` s
            repoTerms = getMultiList config ("google-code.projects."
                                             `T.append` repo
                                             `T.append` ".terms")
            makeGC :: [String] -> GoogleCodeSource
            makeGC term = GoogleCodeSource (T.unpack repo) term
        in if length repoTerms > 0
           then map makeGC repoTerms
           else [makeGC []]
  in concatMap getRepoData repos

loadGHSources :: HM.HashMap DCT.Name DCT.Value -> [GitHubSource]
loadGHSources config =
  let repos = getChildrenOf config "github.projects"
      getRepoData :: T.Text -> [GitHubSource]
      getRepoData repo =
        let repoName = T.append "github.projects." repo
            projectNames = getChildrenOf config repoName
        in concatMap (getProjectData repo) projectNames

      getProjectData :: T.Text -> T.Text -> [GitHubSource]
      getProjectData user project =
        let prefix = (
              "github.projects." `T.append` user `T.append` "."
              `T.append` project)
            tag_prefix = prefix `T.append` ".tags"
            tags = getMultiList config tag_prefix
            makeGH taglist = GitHubSource (T.unpack user) (
              T.unpack project) taglist
        in if length tags > 0
           then map makeGH tags
           else [makeGH []]
  in concatMap getRepoData repos

loadFileGlob :: Maybe [String] -> IO (Maybe [FilePath])
loadFileGlob pats =
  case pats of
    (Just xs) -> do files <- mapM glob xs
                    return (Just $ concat files)
    Nothing -> return Nothing

loadConfig :: DCT.Config -> IO (Maybe RunConfiguration)
loadConfig config = do
  configmap <- DC.getMap config
  file_patterns <- DC.lookup config "scan_files"
  file_list <- loadFileGlob file_patterns
  stub_patterns <- DC.lookup config "stub_files"
  stub_list <- loadFileGlob stub_patterns
  output_file <- DC.lookup config "output_file"
  cache <- DC.lookup config "cache_dir"
  github_oauth <- DC.lookup config "github.auth_token" :: IO (Maybe T.Text)
  cache_dir <- if isJust cache
               then do let (Just store) = cache
                       res <- makeStore store
                       case res of
                         Nothing -> do putStrLn $ "Could not find cache_dir " ++ store
                                       return res
                         otherwise -> return res
               else return Nothing
  let gh_list = loadGHSources configmap
      gc_list = loadGCSources configmap
      gh_auth = fmap T.unpack github_oauth
  return $ RunConfiguration <$> file_list <*> stub_list <*> output_file <*>
    (pure gh_auth) <*> (pure gh_list) <*> (pure gc_list) <*> (pure cache_dir)

loadOrgIssues :: (NodeUpdate a) => FilePath -> IO (OrgDocView a)
loadOrgIssues file = do
  fh <- openFile file ReadMode
  fileText <- hGetContents fh
  let !len = length fileText
      doc = orgFile fileText
      view = generateDocView doc
  return view

describeConfiguration :: RunConfiguration -> IO ()
describeConfiguration runcfg = do
  let scan_files = rcScanFiles runcfg
      output = rcOutputFile runcfg
      github = rcGitHubSources runcfg
      googlecode = rcGoogleCodeSources runcfg
      descGithub (GitHubSource user project tags) =
        let tagstr = if length tags > 0
                     then " with tags " ++ (intercalate ", " tags)
                     else ""
        in user ++ "/" ++ project ++ tagstr
      descGoogleCode (GoogleCodeSource repo terms) = repo ++ (
        if length terms > 0
        then " with search terms " ++ (intercalate ", " terms)
        else "")
  putStrLn $ "Will search for issues in these files:\n\t" ++ (
    intercalate "\n\t" scan_files)
  if length github > 0
    then do putStrLn $ "Will scan GitHub for these projects:\n\t" ++ (
              intercalate "\n\t" $ map descGithub github)
    else return ()
  if length googlecode > 0
    then do putStrLn $ "Will scan Google Code for these projects:\n\t" ++ (
              intercalate "\n\t" $ map descGoogleCode googlecode)
    else return ()
  putStrLn $ "Will put new issues into " ++ output

-- | Type, Repo, Issue
  {-
categorize :: [InputIssue] -> [(String, [(String, [Issue])])]
categorize issues =
  let byType = groupBy (\a b -> (iType a == iType b)) issues
      byOrigin s = map (\s -> (origin $ head s, s)) $ groupBy (
        \a b -> (origin a == origin b)) s
  in map (\s -> (iType $ head s, byOrigin s)) byType
-}
-- (type, [(repo, issue)]
sortByFirsts :: (Ord a) => [(a, b)] -> [(a, b)]
sortByFirsts = sortBy (\a b -> compare (fst a) (fst b))
groupByFirsts :: (Eq a) => [(a, b)] -> [[(a, b)]]
groupByFirsts = groupBy (\a b -> (fst a) == (fst b))
orderIssues :: (Ord a, Ord b) => [(a, [(a, [b])])] ->  [(a, [(a, [b])])]
orderIssues tris =
  let concatSeconds abs = let h = fst $ head abs
                              snds = concatMap snd abs
                          in (h, snds)
      bySecond (t, ri) = let bySeconds = groupByFirsts ri
                         in (t, map concatSeconds $ bySeconds)
  in map (bySecond . concatSeconds) $ groupByFirsts tris

printIssues :: [InputIssue] -> String
printIssues issues =
  intercalate "\n" $ map show issues
{-
-- | Print a reasonably-well formatted list of issues, grouped by type
-- and repo first.  Would be better with pretty-printer support, to
-- get terminal width, etc.
printIssues :: [(String, [(String, [Issue])])] -> String
printIssues issues =
  let printSingleIssue s = show $ number s
      printOriginList :: [(String, [Issue])] -> String
      printOriginList elems =
          let printOrigin (origin, iss) =
                 origin ++ ": " ++ (intercalate " " (
                                       map printSingleIssue iss))
          in intercalate "\n\t" $ map printOrigin elems
      printTypeList :: (String, [(String, [Issue])]) -> String
      printTypeList (type_name, elems) =
          type_name ++ ":\n\t" ++ (printOriginList elems) ++ "\n"
  in intercalate "\n  " $ map printTypeList $ orderIssues issues
-}

showDuplicateIssues dup_existing_issues existing_issue_filelist =
  if length dup_existing_issues > 0
  then do putStr ("Found " ++ (show $ length dup_existing_issues)
                  ++ " duplicate issues:\n  ")
          let idIssue iss = (iType iss) ++ ":" ++ (origin iss) ++ "# " ++ (
                show $ number iss)
              showiss (iss, cnt) =
                (show cnt) ++ ": " ++ idIssue iss
              showfile (fn, iss) =
                fn ++ ":\n" ++ (printIssues iss)
          putStrLn (intercalate "\n" (
                       map showiss dup_existing_issues))
          putStrLn "All issues by file:"
          putStrLn (intercalate "\n" $
                    map showfile existing_issue_filelist)
          return ()
  else return ()

loadIssueFile :: FilePath -> IO IssueFile
loadIssueFile path = do
  let emptyDoc = OrgDocView [] (OrgDoc [] [])
      ret_empty _ = return emptyDoc
  doc <- catchIOError (loadOrgIssues path) ret_empty
  return $ IssueFile path doc

issueIndex :: Bool -> IssueFile -> [(InputIssue, IssueFile)]
issueIndex update ifile =
  map (\i -> (LoadedIssue update ifile i, ifile)) $ getRawElements $ ifDoc ifile

generateIssueFileText :: [(InputIssue, IssueFile)] -> Bool -> String
generateIssueFileText [] _ = ""
generateIssueFileText issuelist verbose =
  let issue_set = S.fromList $ map (issueof . fst) issuelist
      orig_doc = ifDoc . snd . head $ issuelist
      new_doc = updateDoc issue_set orig_doc
  in (intercalate "\n" $ map tlText $ getTextLines new_doc) ++ "\n"

-- | Swaps the Issues in in 'index' with those in 'issues'.  Those not
-- used in the swap are returned as the first part of the pair, and
-- the swapped result in the second.
swapIssuesInIndex :: [(Issue, IssueFile)] -> [Issue] -> ([Issue], [(Issue, IssueFile)])
swapIssuesInIndex index issues =
  let issmap = HM.fromList index
      (unused, used) = partition (\i -> isNothing $ HM.lookup i issmap) issues
      updated :: [(Issue, IssueFile)]
      updated = map (\k -> (k, fromJust $ HM.lookup k issmap)) used
  in (unused, updated)

-- | Generate a new copy of the OrgDoc in IssueFile, with the new
-- versions of the issues replacing the old.  Note that the 'snd'
-- element of each item in the list must be the same, but we only use
-- the first (list) element's snd.
updateIssueFile :: Bool -> [(InputIssue, IssueFile)] -> IO ()
updateIssueFile verbose issuelist = do
  let path = ifPath $ snd $ head issuelist
      num_issues = show $ length issuelist
  putStrLn $ "** Rewriting " ++ path ++ " for " ++ num_issues ++ " issues."
  writeFile path (generateIssueFileText issuelist verbose)

loadIssuesFromConfiguration :: RunConfiguration -> IO [Issue]
loadIssuesFromConfiguration runcfg = do
  let orig_scan_files = rcScanFiles runcfg
      scan_files = nub orig_scan_files
  raw_existing_issues <- mapM loadIssueFile scan_files
  let docs = map ifDoc raw_existing_issues
      issues = map fst $ concatMap ovElements docs
  return issues

fetchIssues runcfg verbose = do
  let github_auth = rcGitHubOAuth runcfg
      github = rcGitHubSources runcfg
      googlecode = rcGoogleCodeSources runcfg
  -- Load the issues from our sources
  let loadGHSource oauth (GitHubSource user project tags) =
        GH.fetch oauth user project (Just Open) tags
      loadGCSource (GoogleCodeSource repo terms) =
        GC.fetch repo terms

  if verbose
    then do putStrLn $ "Loading from " ++ (show $ length github) ++ (
              " GitHub queries...")
    else return ()

  gh_issues <- mapM (loadGHSource github_auth) github

  if verbose
    then do putStrLn $ "Loading from " ++ (show $ length googlecode) ++ (
              " GoogleCode queries...")
    else return ()

  gc_issues <- mapM loadGCSource googlecode

  let found_issues = nub $ sort $ concat (gh_issues ++ gc_issues)

  if isJust $ rcCache runcfg
    then do let (Just store) = rcCache runcfg
            mapM (saveIssue store) found_issues
            if verbose
              then do putStrLn $ "Saved " ++ (show $ length found_issues) ++
                        " issues to cache"
                      return ()
              else return ()
    else return ()
  return found_issues

runConfiguration :: RunConfiguration -> RunOptions -> IO ()
runConfiguration runcfg options = do
  -- Scan all the existing files.
  let orig_scan_files = rcScanFiles runcfg
      output = rcOutputFile runcfg
      github = rcGitHubSources runcfg
      googlecode = rcGoogleCodeSources runcfg
      (RunOptions fetch write verbose update) = options
      scan_files = nub orig_scan_files
      have_cache = isJust $ rcCache runcfg

  raw_existing_issues <- mapM loadIssueFile scan_files

  let existing_issue_map = HM.fromList $
                           concatMap (issueIndex True) raw_existing_issues
      existing_issues = HM.keys existing_issue_map

  raw_found_issues <-
    if fetch
    then fetchIssues runcfg verbose
    else if have_cache
         then do let (Just cache) = rcCache runcfg
                 isss <- loadAllIssues cache
                 if verbose
                   then putStrLn $ "Loaded " ++ (show $ length isss) ++ " issues from cache:\n" ++ (printIssues $ map FetchedIssue isss)
                   else return ()
                 return isss
         else return []
  let found_issues = map FetchedIssue raw_found_issues

  let new_issues = found_issues \\ existing_issues
      -- scan for changed issues
      changed_issues_byfile =
        let changed = found_issues \\ new_issues
            idx = map (\k -> (k, fromJust $ HM.lookup k existing_issue_map )) changed
            sorted = sortBy (\(_,a) (_,b) -> compare a b) idx
        in groupBy (\(_,a) (_,b) -> a == b) sorted

  -- For each changed file, load it up, update the issue->nodes, and
  -- then re-write the files.
  if write
    then do putStrLn ("Writing to files: " ++ (
                         intercalate "," (map (ifPath . snd) $
                                          map head changed_issues_byfile)))
            mapM_ (updateIssueFile verbose) changed_issues_byfile
    else return ()

  if verbose
    then do putStrLn $ "Found " ++ (show $ length new_issues) ++ " new issues"
            -- showDuplicateIssues dup_existing_issues existing_issue_filelist
            putStr $ "Existing issues:\n  "
            putStrLn $ printIssues existing_issues
            putStr $ "Issues found from queries:\n  "
            putStrLn $ printIssues found_issues
            putStr $ "New Issues:\n  "
            putStrLn $ printIssues new_issues
    else return ()

  if write
    then do putStrLn $ "Writing issues to " ++ output
            appendIssues output $ map issueof new_issues
    else return ()
  return ()


