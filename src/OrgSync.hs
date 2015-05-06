{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- TODO(lally): trim down these exports.
module OrgSync where
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import qualified Data.Text as T
import qualified Data.Set as S
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
                        , rcGoogleTaskSources :: [GoogleTasksSource]
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
  let retEmpty _ = return $ OrgDocView [] (OrgDoc [] [])
  doc <- catchIOError (loadOrgIssues path) retEmpty
  return $ IssueFile path doc

generateIssueFileText :: IssueFile -> [InputIssue] -> String
generateIssueFileText _ [] = ""
generateIssueFileText file issuelist =
  let issueSet = S.fromList $ map issueof issuelist
      origDoc = ifDoc file
      newDoc = updateDoc issueSet origDoc
  in (intercalate "\n" $ map tlText $ getTextLines newDoc) ++ "\n"

-- | Generate a new copy of the OrgDoc in IssueFile, with the new
-- versions of the issues replacing the old.  Note that the 'snd'
-- element of each item in the list must be the same, but we only use
-- the first (list) element's snd.
updateIssueFile :: Bool -> IssueFile -> [InputIssue] -> IO ()
updateIssueFile verbose file issuelist = do
  let numIssues = show $ length issuelist
      path = ifPath file
  commentLn verbose $ "** Rewriting " ++ path ++ " for " ++ numIssues ++ " issues."
  writeFile path (generateIssueFileText file issuelist)

-- Load in files to scan, and stubs to ignore.  Note that ignore files
-- take precedence, so if a file is in both scan and ignore, the
-- issues in that file will be ignored.
loadIssuesFromConfiguration :: RunConfiguration -> IO [InputIssue]
loadIssuesFromConfiguration runcfg = do
  let origScanFiles = rcScanFiles runcfg
      scanFiles = nub origScanFiles
      origStubFiles = rcStubFiles runcfg
      stubFiles = nub origStubFiles
  let getIssues save issuefile =
        let rawIssues = map fst $ ovElements $ ifDoc issuefile
        in map (LoadedIssue save issuefile) rawIssues
      getIssuesM save issueFile =
        return $ getIssues save issueFile
  rawExistingIssues <- mapM loadIssueFile scanFiles
  rawStubIssues <- mapM loadIssueFile stubFiles
  let existingIssues = concatMap (getIssues True) (
        (nub $ sort rawExistingIssues) \\ (nub $ sort rawStubIssues))
      stubIssues = concatMap (getIssues False) (nub $ sort rawStubIssues)
      issueMap :: HS.HashSet InputIssue
      issueMap = foldl (flip HS.insert) (HS.fromList existingIssues) stubIssues
  return $ nub $ sort $ HS.toList issueMap

-- |Fetches issues from all sources in =runcfg=, saves them to cache
-- (if specified), and returns them.
fetchIssues runcfg verbose existing = do
  let githubAuth = rcGitHubOAuth runcfg
      github = rcGitHubSources runcfg
      googlecode = rcGoogleCodeSources runcfg
  -- Load the issues from our sources
      cL = commentLn verbose

  cL $ "Loading from " ++ (show $ length github) ++ " GitHub queries..."
  postGhIssues <- foldM (loadSource githubAuth) existing github

  cL $ "Loading from " ++ (show $ length googlecode) ++ " GoogleCode queries..."
  postGcIssues <- foldM (loadSource Nothing) postGhIssues googlecode

  let foundIssues = nub $ sort $ postGcIssues

  if isJust $ rcCache runcfg
    then do let (Just store) = rcCache runcfg
            mapM (saveIssue store) $ map issueof foundIssues
            cL $ "Saved " ++ (show $ length foundIssues) ++ " issues to cache"
    else return ()
  return foundIssues

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
  let output = rcOutputFile runcfg
      github = rcGitHubSources runcfg
      googlecode = rcGoogleCodeSources runcfg
      (RunOptions fetch write verbose update) = options
      haveCache = isJust $ rcCache runcfg
      cL = commentLn verbose
  rawWinsize <- TS.size
  let kWidth = case rawWinsize of
        Nothing -> 80
        Just win -> TS.width win
  fileIssues <- loadIssuesFromConfiguration runcfg

  cL "\n\n** Done loading **\n"

  repoIssues <- if fetch
                then do
                  rawIss <- fetchIssues runcfg verbose fileIssues
                  return $ nub $ sort rawIss
                else if haveCache
                     then do
                       let (Just cache) = rcCache runcfg
                       rawIss <- loadAllIssues cache
                       let issues = map FetchedIssue $ nub $ sort rawIss
                       cL  $ "Loaded " ++ (show $ length issues) ++
                             " issues from cache:\n" ++ (printIssues kWidth issues)
                       return issues
                     else return []

  -- Split issues up:
  -- * New ones that weren't on disk. (new)
  --   These get put into the output file.
  --
  -- * Issues that we have on disk and weren't in the repo (orphans)
  --    Perhaps the tags have changed so that they don't show up in
  --    our normal fetch.
  --    * That aren't closed or ARCHIVEd.  So This should be optional.
  --      Update them.
  --    * If we can't find them, mark them ARCHIVEd and put in an
  --      IssueEvent saying that they're no longer available.  Make
  --      sure that we don't do this for failed fetches.
  --    These get fetched as deemed necessary.
  --
  -- * Issues that we have on disk and were in the repo (merge)
  --    * And want to ignore all details on.
  --    * And want to update.
  -- Coallate this into a set of issues to update on files

  let newIssues = repoIssues \\ fileIssues
      mergeIssues =
        map makeMerge $ fileIssues `intersect` repoIssues
        where
          makeMerge f =
            MergedIssue fIss (liIssue f) (liFile f) (liUpdate f)
            where fIss = issueof $ fromJust $ find (eqIssue f) repoIssues

  let fileOf (MergedIssue _ _ f _) = f
      shouldUpdate (file, issues) = any wantAnyUpdate issues
      (mergeIssuesByfile, ignoreIssuesByFile) =
        partition shouldUpdate $ aggregateBy fileOf mergeIssues

  cL ("Found " ++ (show $ length mergeIssues) ++ " Changed issues.")
  cL ("Writing to files: " ++ (
         intercalate "," (map (ifPath . fst) mergeIssuesByfile)))
  cL ("Not writing to files: " ++ (
         intercalate "," $ map (ifPath . fst) ignoreIssuesByFile))

  -- For each changed file, load it up, update the issue->nodes, and
  -- then re-write the files.
  if write
    then do mapM_ (\(f, iss) -> updateIssueFile verbose f iss) mergeIssuesByfile
    else return ()

  cL $ "\n>> " ++ (show $ length fileIssues) ++ " existing issues [file]:"
  cL $ printIssues kWidth fileIssues
  cL $ "\n>> " ++ (show $ length repoIssues) ++ " issues found in queries [repo]:"
  cL $ printIssues kWidth repoIssues
  cL $ "\n>> " ++ (show $ length newIssues) ++ " new issues [new]:"
  cL $ printIssues kWidth newIssues

  if write
    then do cL $ "Writing issues to " ++ output
            appendIssues output $ map issueof newIssues
    else return ()
  return ()


