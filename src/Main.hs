{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import qualified Data.Text as T
import qualified Sync.Retrieve.GoogleCode.GoogleCode as GC
import qualified Sync.Retrieve.GitHub.GitHub as GH
import qualified Data.HashMap.Strict as HM
import Control.Applicative
import Control.Monad (liftM2, liftM3, mplus, join)
import Control.Monad.Catch (catchIOError)
import Data.List (nub, (\\), sort, intersect, groupBy)
import Debug.Trace
import Data.Maybe
import Sync.Push.OrgMode
import Sync.Retrieve.OrgMode.OrgMode
import Sync.Issue.Issue
import Data.List (intercalate)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.FilePath.Glob

-- Config Format:
-- scan_files = ["~/org/*.org"]
-- output_file = "./test.org"
-- github {
--   uproxy {
--      api_key = "" -- still unused.
--      projects = {
--        uproxy {
--          tags = []
--        }
--      }
--   }
--   freedomjs {
--      api_key = "" -- still unused.
--      projects = {
--        freedom-for-chrome {
--          tags = []
--        }
--      }
--   }
-- }
--
-- google-code {
--   webrtc {
--     terms = [["sctp"], ["datachannel"]]
--   }
-- }

data GoogleCodeSource = GoogleCodeSource
                        { gcRepo :: String
                        , gcSearchTerms :: [String]
                        } deriving (Eq, Show)

data GitHubSource = GitHubSource
                    { ghUser :: String
                    , ghProject :: String
                    , ghTagLists :: [String]
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
    otherwise -> trace ("looking up key " ++ (show key) ++ " results in " ++ show res) []

loadGCSources :: HM.HashMap DCT.Name DCT.Value -> [GoogleCodeSource]
loadGCSources config =
  let repos = getChildrenOf config "google-code.projects"
      -- TODO: make this just [GoogleCodeSource]
      getRepoData :: T.Text -> [GoogleCodeSource]
      getRepoData repo =
        let rPlus s = repo `T.append` s
            repoTerms = getMultiList config ("google-code.projects." `T.append` repo `T.append` ".terms")
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
        let prefix = "github.projects." `T.append` user `T.append` "." `T.append` project
            tag_prefix = prefix `T.append` ".tags"
            tags = getMultiList config tag_prefix
            makeGH taglist = GitHubSource (T.unpack user) (T.unpack project) taglist
        in if length tags > 0
           then map makeGH tags
           else [makeGH []]
  in concatMap getRepoData repos

data CommandOptions = Options
                      { optPrintConfig :: Bool
                      , optCommandFile :: String
                      , optWriteOutput :: Bool
                      , optFetchIssues :: Bool
                      , optScanOutput :: Bool
                      } deriving (Eq, Show)

defaultOptions = Options
  { optPrintConfig = False
  , optCommandFile = "./org-issue-sync.conf"
  , optWriteOutput = True
  , optFetchIssues = True
  , optScanOutput = True }

data RunConfiguration = RunConfiguration
                        { rcScanFiles :: [FilePath]
                        , rcOutputFile :: FilePath
                        , rcGitHubSources :: [GitHubSource]
                        , rcGoogleCodeSources :: [GoogleCodeSource]
                        } deriving (Eq, Show)

loadConfig :: DCT.Config -> IO (Maybe RunConfiguration)
loadConfig config = do
  configmap <- DC.getMap config
  file_patterns <- DC.lookup config "scan_files"
  raw_file_list <- case file_patterns of
                        (Just xs) -> do files <- mapM glob xs
                                        return (Just $ concat files)
                        Nothing -> return Nothing
  output_file <- DC.lookup config "output_file"
  let gh_list = loadGHSources configmap
      gc_list = loadGCSources configmap
      file_list = Just raw_file_list
  return $ RunConfiguration <$> raw_file_list <*> output_file <*> (pure gh_list) <*> (pure gc_list)

loadOrgIssues :: FilePath -> IO ([Issue])
loadOrgIssues file = do
  fh <- openFile file ReadMode
  fileText <- hGetContents fh
  let !len = length fileText
  issues <- getOrgIssues fileText
  return issues

describeConfiguration :: RunConfiguration -> IO ()
describeConfiguration (RunConfiguration scan_files output github googlecode) = do
  let descGithub (GitHubSource user project tags) = user ++ "/" ++ project ++ (
                                                    if length tags > 0
                                                    then " with tags " ++ (intercalate ", " tags)
                                                    else "")
      descGoogleCode (GoogleCodeSource repo terms) = repo ++ (
        if length terms > 0
        then " with search terms " ++ (intercalate ", " terms)
        else "")
  putStrLn $ "Will search for issues in these files:\n\t" ++ intercalate "\n\t" scan_files
  if length github > 0
    then do putStrLn $ "Will scan GitHub for these projects:\n\t" ++ (
              intercalate "\n\t" $ map descGithub github)
    else return ()
  if length github > 0
    then do putStrLn $ "Will scan Google Code for these projects:\n\t" ++ (
              intercalate "\n\t" $ map descGoogleCode googlecode)
    else return ()
  putStrLn $ "Will put new issues into " ++ output

runConfiguration :: RunConfiguration -> Bool -> Bool -> Bool -> IO ()
runConfiguration (RunConfiguration orig_scan_files output github googlecode) fetch write verbose = do
  -- Scan all the existing files.
  let scan_files = nub orig_scan_files
  raw_existing_issues <- mapM loadOrgIssues scan_files
  let existing_issue_filelist = zip scan_files raw_existing_issues
      sorted_existing_issues = sort $ concat raw_existing_issues
      existing_issues = nub sorted_existing_issues
      dup_existing_issues = map (\issues -> (head issues, length issues)) $ filter (\n -> length n > 1) $ groupBy (==) sorted_existing_issues

  -- Load the issues from our sources
  let loadGHSource (GitHubSource user project tags) =
        GH.fetch Nothing user project (Just Open) tags
      loadGCSource (GoogleCodeSource repo terms) =
        GC.fetch repo terms
  if verbose
  then do putStrLn $ "Loading from " ++ (show $ length github) ++ " GitHub queries..."
  else return ()
  gh_issues <- if fetch
               then do mapM loadGHSource github
               else return []
  if verbose
  then do putStrLn $ "Loading from " ++ (show $ length googlecode) ++ " GoogleCode queries..."
  else return ()
  gc_issues <- if fetch
               then do mapM loadGCSource googlecode
               else return []
  let found_issues = nub $ sort $ concat (gh_issues ++ gc_issues)
      new_issues = found_issues \\ existing_issues

  let idIssue iss = (iType iss) ++ ":" ++ (origin iss) ++ "# " ++ (show $ number iss)
      categorize :: [Issue] -> [(String, [(String, [Issue])])]
      categorize issues =
        let byType = groupBy (\a b -> (iType a == iType b)) issues
            byOrigin s = map (\s -> (origin $ head s, s)) $ groupBy  (\a b -> (origin a == origin b)) s
        in map (\s -> (iType $ head s, byOrigin s)) byType
      printIssues :: [(String, [(String, [Issue])])] -> String
      printIssues issues =
        let printSingleIssue s = show $ number s
            printOriginList :: [(String, [Issue])] -> String
            printOriginList elems =
                let printOrigin (origin, iss) =
                       origin ++ ": " ++ (intercalate " " $ map printSingleIssue iss)
                in intercalate "\n\t" $ map printOrigin elems
            printTypeList :: (String, [(String, [Issue])]) -> String
            printTypeList (type_name, elems) =
                type_name ++ ":\n\t" ++ (printOriginList elems) ++ "\n"
        in intercalate "\n  " $ map printTypeList issues
  if verbose
  then do putStrLn $ "Found " ++ (show $ length new_issues) ++ " new issues"
          if length dup_existing_issues > 0
          then do putStr $ "Found " ++ (show $ length dup_existing_issues) ++ " duplicate issues:\n  "
                  putStrLn $ intercalate "\n" $ map (\(iss, cnt) -> (show cnt) ++ ": " ++ idIssue iss) dup_existing_issues
                  putStrLn "All issues by file:"
                  putStrLn $ intercalate "\n" $ map (\(filename, issues) -> filename ++ ":\n" ++ (printIssues $ categorize issues)) existing_issue_filelist
          else return ()
          putStr $ "Existing issues:\n  "
          putStrLn $ printIssues $ categorize existing_issues
          putStr $ "Issues found from queries:\n  "
          putStrLn $ printIssues $ categorize found_issues
          putStr $ "New Issues:\n  "
          putStrLn $ printIssues $ categorize new_issues
  else return ()
  if write
    then do putStrLn $ "Writing issues to " ++ output
            appendIssues output new_issues
    else return ()
  return ()


options :: [OptDescr (CommandOptions -> CommandOptions)]
options =
    [ Option ['v']     ["verbose"]
        (NoArg (\ opts -> opts { optPrintConfig = True }))
        "Print run configuration and its evaluation."
    , Option ['d']     ["dryrun"]
        (NoArg (\ opts -> opts { optFetchIssues = False, optWriteOutput = False }))
        "Do no actual I/O."
    , Option ['i']     ["input"]
        (ReqArg (\ f opts -> opts { optCommandFile = f }) "FILE")
        "Configuration file name."
    , Option ['f']     ["nofetch"]
        (NoArg (\ opts -> opts { optFetchIssues = False }))
        "Do not fetch issues."
    , Option ['s']     ["scaninput"]
        (NoArg (\ opts -> opts { optScanOutput = True }))
        "Always scan the output file for issues."
    , Option ['w']     ["nowrite"]
        (NoArg (\ opts -> opts { optWriteOutput = False }))
        "Do not write new issues to output file."
    ]

main :: IO ()
main = do
  argv <- getArgs
  case getOpt Permute options argv of
    (_,_,errs@(e:es)) -> ioError (userError (concat errs ++ usageInfo "Org Issue Sync" options))
    (o,n,[]  ) ->
      do let opts = foldl (flip id) defaultOptions o
         if length n > 0
           then do putStrLn $ "Ignoring unused arguments: " ++ (intercalate " " n)
           else return ()
         raw_configs <- DC.load [ DCT.Required (optCommandFile opts) ]
         config <- loadConfig raw_configs
         if isNothing config
           then do putStrLn "No valid configuration found.  Exiting."
                   res <- exitFailure
                   exitWith res
           else return ()
         let (Just prerunconfig) = config
         let runconfig = if optScanOutput opts
                         then prerunconfig { rcScanFiles = (rcOutputFile prerunconfig):(rcScanFiles prerunconfig) }
                         else prerunconfig
         if optPrintConfig opts
           then describeConfiguration runconfig
           else return ()
         runConfiguration runconfig (optFetchIssues opts) (optWriteOutput opts) (optPrintConfig opts)
         res <- exitSuccess
         exitWith res
