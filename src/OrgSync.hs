{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- TODO(lally): trim down these exports.
module OrgSync where
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Sync.Retrieve.GoogleCode.GoogleCode as GC
import qualified Sync.Retrieve.GitHub.GitHub as GH
import qualified System.Console.Terminal.Size as TS
import Sync.Retrieve
import Control.Monad (join, foldM)
import Data.List
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

data RunConfiguration = RunConfiguration
                        { rcScanFiles :: [FilePath]
                        , rcStubFiles :: [FilePath]
                        , rcOutputFile :: FilePath
                        , rcGitHubOAuth :: Maybe String
                        , rcGitHubSources :: [GitHubSource]
                        , rcGoogleCodeSources :: [GoogleCodeSource]
                        , rcCache :: Maybe IssueStore
                        } deriving (Eq, Show)

-- |Semantics: When not fetching issues, we can still scan the cache for
-- issues, and put them in.  But we may not write new ones out
-- (roWriteNewIssues), or not update existing issues (roUpdateIssues).
data RunOptions = RunOptions
                  { roFetchIssues :: Bool
                  , roWriteNewIssues :: Bool
                  , roVerbose :: Bool
                  , roUpdateIssues :: Bool
                  } deriving (Eq, Show)

commentLn :: Bool -> String -> IO ()
commentLn verbose s = if verbose
                      then putStrLn s
                      else return ()

loadOrgIssues :: (NodeUpdate a) => FilePath -> IO (OrgDocView a)
loadOrgIssues file = do
  fh <- openFile file ReadMode
  fileText <- hGetContents fh
  let !len = length fileText
      doc = orgFile fileText
      view = generateDocView doc
  return view

-- | Print a reasonably-well formatted list of issues, grouped by type
-- and repo first.  Would be better with pretty-printer support, to
-- get terminal width, etc.
printIssues :: Int -> [InputIssue] -> String
printIssues width inpissues =
  let issues = map issueof inpissues
      byType = aggregateBy iType issues
      byTypeAndRepo = map (\(t, iss) -> (t, aggregateBy origin iss)) byType
      printIssueList :: String -> [Int] -> String
      printIssueList orign issuenums =
        let rawString = orign ++ ": " ++ (intercalate ", " $
                                          map show $ sort issuenums)
            kIndentLevel = 4
            indent = replicate kIndentLevel ' '
            doubleIndent = replicate (kIndentLevel*2)
            rawWrappedLines = lines $
              wrapStringVarLines (width - kIndentLevel:
                                  repeat (width - 2*kIndentLevel)) rawString
            wrapped = (indent ++ (head rawWrappedLines)):(
              map (\s -> indent ++ indent ++ s) (tail rawWrappedLines))
        in intercalate "\n" wrapped
      printIssuesForType :: (String, [(String, [Issue])]) -> String
      printIssuesForType (ty, rest) =
        let printRestElem :: (String, [Issue]) -> String
            printRestElem (orig, iss) = printIssueList orig $ map number iss
        in ty ++ ":\n" ++ (intercalate "\n" $ map printRestElem rest)
  in intercalate "\n" $ map printIssuesForType byTypeAndRepo

loadIssueFile :: FilePath -> IO IssueFile
loadIssueFile path = do
  let ret_empty _ = return $ OrgDocView [] (OrgDoc [] [])
  doc <- catchIOError (loadOrgIssues path) ret_empty
  return $ IssueFile path doc

generateIssueFileText :: IssueFile -> [InputIssue] -> String
generateIssueFileText _ [] = ""
generateIssueFileText file issuelist =
  let issue_set = S.fromList $ map issueof issuelist
      orig_doc = ifDoc file
      new_doc = updateDoc issue_set orig_doc
  in (intercalate "\n" $ map tlText $ getTextLines new_doc) ++ "\n"

-- | Generate a new copy of the OrgDoc in IssueFile, with the new
-- versions of the issues replacing the old.  Note that the 'snd'
-- element of each item in the list must be the same, but we only use
-- the first (list) element's snd.
updateIssueFile :: Bool -> IssueFile -> [InputIssue] -> IO ()
updateIssueFile verbose file issuelist = do
  let num_issues = show $ length issuelist
      path = ifPath file
  commentLn verbose $ "** Rewriting " ++ path ++ " for " ++ num_issues ++ " issues."
  writeFile path (generateIssueFileText file issuelist)

-- Load in files to scan, and stubs to ignore.  Note that ignore files
-- take precedence, so if a file is in both scan and ignore, the
-- issues in that file will be ignored.
loadIssuesFromConfiguration :: RunConfiguration -> IO [InputIssue]
loadIssuesFromConfiguration runcfg = do
  let orig_scan_files = rcScanFiles runcfg
      scan_files = nub orig_scan_files
      orig_stub_files = rcStubFiles runcfg
      stub_files = nub orig_stub_files
  raw_existing_issues <- mapM loadIssueFile scan_files
  raw_stub_issues <- mapM loadIssueFile stub_files
  let getIssues save issuefile =
        let raw_issues = map fst $ ovElements $ ifDoc issuefile
        in map (LoadedIssue save issuefile) raw_issues
      existing_issues = concatMap (getIssues True) (
        (nub $ sort raw_existing_issues) \\ (nub $ sort raw_stub_files))
      stub_issues = concatMap (getIssues False) (nub $ sort raw_stub_issues)
      issue_map :: HS.HashSet InputIssue
      issue_map = foldl (flip HS.insert) (HS.fromList existing_issues) existing_issues
  return $ nub $ sort $ HS.toList issue_map

fetchIssues runcfg verbose existing = do
  let github_auth = rcGitHubOAuth runcfg
      github = rcGitHubSources runcfg
      googlecode = rcGoogleCodeSources runcfg
  -- Load the issues from our sources
      cL = commentLn verbose

  -- TODO: problem: now I've got multiple lists here, back from each
  -- fetch.  I'd rather do some sort of monadic fold over the list...
  cL $ "Loading from " ++ (show $ length github) ++ " GitHub queries..."
  post_gh_issues <- foldM (loadGHSource github_auth) existing github
--  post_gh_issues <- mapM (loadGHSource github_auth existing) github

  cL $ "Loading from " ++ (show $ length googlecode) ++ " GoogleCode queries..."
  post_gc_issues <- foldM loadGCSource post_gh_issues googlecode
--  post_gc_issues <- mapM (loadGCSource post_gh_issues) googlecode

  let found_issues = nub $ sort $ post_gc_issues

  if isJust $ rcCache runcfg
    then do let (Just store) = rcCache runcfg
            mapM (saveIssue store) $ map issueof found_issues
            cL $ "Saved " ++ (show $ length found_issues) ++ " issues to cache"
    else return ()
  return found_issues

-- |Applies |f| to each element in |elems|, and then groups by the
-- same result.  The groups are in (result, [elem]) format, where each
-- [f e| e <- elems] == result.
aggregateBy f elems =
  let sortPfx f a b = compare (f a) (f b)
      eqPfx f a b = (f a) == (f b)
      pairs = map (\i -> (f i, i)) elems
      grouped = groupBy (eqPfx fst) $ sortBy (sortPfx fst) pairs
      aggEach es = (fst $ head es, map snd es)
  in map aggEach grouped

runConfiguration :: RunConfiguration -> RunOptions -> IO ()
runConfiguration runcfg options = do
  -- Scan all the existing files. TODO: use loadIssuesFromConfiguration
  let output = rcOutputFile runcfg
      github = rcGitHubSources runcfg
      googlecode = rcGoogleCodeSources runcfg
      (RunOptions fetch write verbose update) = options
      have_cache = isJust $ rcCache runcfg
      cL = commentLn verbose
  raw_winsize <- TS.size
  let kWidth = case raw_winsize of
        Nothing -> 80
        Just win -> TS.width win
  file_issues <- loadIssuesFromConfiguration runcfg

  cL "\n\n** Done loading **\n"

  repo_issues <-
    if fetch
    then do raw_iss <- fetchIssues runcfg verbose file_issues
            return $ nub $ sort raw_iss
    else if have_cache
         then do let (Just cache) = rcCache runcfg
                 raw_iss <- loadAllIssues cache
                 let issues = map FetchedIssue $ nub $ sort raw_iss
                 cL  $ "Loaded " ++ (show $ length issues) ++
                   " issues from cache:\n" ++ (printIssues kWidth issues)
                 return issues
         else return []

  let new_issues = repo_issues \\ file_issues
      changed_issues_raw = file_issues `intersect` repo_issues
      changed_issues =
        let makeMerge fi =
              let fetched = fromJust $ find (eqIssue fi) repo_issues
                  loaded = liIssue fi
                  update = liUpdate fi
                  file = liFile fi
              in MergedIssue (issueof fetched) loaded file update
        in map makeMerge changed_issues_raw
      -- Scan for changed issues.
      fileOf (MergedIssue _ _ f _) = f
      changed_issues_byfile :: [(IssueFile, [InputIssue])]
      changed_issues_byfile = aggregateBy fileOf changed_issues

  -- TODO: separate changed_issues into ones to update and ones not to.
  -- Then get details on the former set.
  
  cL ("Found " ++ (show $ length changed_issues) ++ " Changed issues.")
  cL ("Writing to files: " ++ (
                        intercalate "," (map (ifPath . fst) $ changed_issues_byfile)))

  -- For each changed file, load it up, update the issue->nodes, and
  -- then re-write the files.
  if write
    then do mapM_ (\(f, iss) -> updateIssueFile True f iss) changed_issues_byfile
    else return ()

  -- showDuplicateIssues dup_file_issues existing_issue_filelist
  cL $ "\n>> " ++ (show $ length file_issues) ++ " existing issues [file]:"
  cL $ printIssues kWidth file_issues
  cL $ "\n>> " ++ (show $ length repo_issues) ++ " issues found in queries [repo]:"
  cL $ printIssues kWidth repo_issues
  cL $ "\n>> " ++ (show $ length new_issues) ++ " new issues [new]:"
  cL $ printIssues kWidth new_issues

  if write
    then do cL $ "Writing issues to " ++ output
            appendIssues output $ map issueof new_issues
    else return ()
  return ()


