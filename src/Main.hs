{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import OrgSync
import Sync.Retrieve
import Control.Applicative
import Control.Monad (liftM2, liftM3, mplus, join)
import Control.Monad.Catch (catchIOError)
import Data.List (nub, (\\), sort, intersect, groupBy)
import Debug.Trace
import Data.Maybe
import Data.OrgMode
import Data.Issue
import Data.IssueCache
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

data CommandOptions = Options
                      { optPrintConfig :: Bool
                      , optCommandFile :: String
                      , optWriteOutput :: Bool
                      , optFetchIssues :: Bool
                      , optScanOutput :: Bool
                      , optDumpIssues :: Bool
                      , optDiffFile :: String
                      , optUpdateIssues :: Bool
                      } deriving (Eq, Show)

defaultOptions = Options
  { optPrintConfig = False
  , optCommandFile = "./org-issue-sync.conf"
  , optWriteOutput = True
  , optFetchIssues = True
  , optScanOutput = True
  , optDumpIssues = False
  , optDiffFile = ""
  , optUpdateIssues = True}

options :: [OptDescr (CommandOptions -> CommandOptions)]
options =
    [ Option ['v']     ["verbose"]
        (NoArg (\ opts -> opts { optPrintConfig = True }))
        "Print run configuration and its evaluation."
    , Option ['d']     ["dryrun"]
        (NoArg (\ opts -> opts { optFetchIssues = False,
                                 optWriteOutput = False }))
        "Do no actual I/O."
    , Option ['c']     ["config"]
        (ReqArg (\ f opts -> opts { optCommandFile = f }) "FILE")
        "Configuration file name."
    , Option ['U']     ["update"]
        (NoArg (\ opts -> opts { optUpdateIssues = False }))
        "Do not update issues in scanned files."
    , Option ['F']     ["nofetch"]
        (NoArg (\ opts -> opts { optFetchIssues = False }))
        "Do not fetch issues."
    , Option ['s']     ["scaninput"]
        (NoArg (\ opts -> opts { optScanOutput = True }))
        "Always scan the output file for issues."
    , Option ['W']     ["nowrite"]
        (NoArg (\ opts -> opts { optWriteOutput = False }))
        "Do not write new issues to output file."
    , Option ['D']     ["dump"]
        (NoArg (\ opts -> opts { optDumpIssues = True }))
        "Dump all issues to stdout"
    , Option ['C']     ["compare"]
        (ReqArg (\ f opts -> opts { optDiffFile = f }) "FILE")
        "Diff scan_files against this file."
    ]


describeConfiguration :: RunConfiguration -> IO ()
describeConfiguration runcfg = do
  let scan_files = rcScanFiles runcfg
      stub_files = rcStubFiles runcfg
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
    Nothing -> return $ Just []


main :: IO ()
main = do
  argv <- getArgs
  case getOpt Permute options argv of
    (_,_,errs@(e:es)) -> ioError (
      userError (concat errs ++ usageInfo "Org Issue Sync" options))
    (o,n,[]  ) ->
      do let opts = foldl (flip id) defaultOptions o
         if length n > 0
           then do putStrLn $ "Ignoring unused arguments: " ++ (
                     intercalate " " n)
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
                         then prerunconfig {
                           rcScanFiles = (rcOutputFile prerunconfig):(
                              rcScanFiles prerunconfig) }
                         else prerunconfig
             runopts =
               RunOptions (optFetchIssues opts) (optWriteOutput opts) (
                 optPrintConfig opts) (optUpdateIssues opts)
         if optPrintConfig opts
           then describeConfiguration runconfig
           else return ()
         runConfiguration runconfig runopts
         res <- exitSuccess
         exitWith res
