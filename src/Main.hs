{-# LANGUAGE BangPatterns, OverloadedStrings #-}
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
import System.Environment (getArgs, getEnvironment)
import System.Exit
import System.IO
import System.FilePath.Glob
import Text.Regex.Posix

import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Network.Google.OAuth2 as OA
import qualified Sync.Retrieve.Google.Tasks as GT

-- Config Format:
-- scan_files = ["~/org/*.org"]
-- output_file = "./test.org"
--
-- google-code {
--   webrtc {
--     terms = [["sctp"], ["datachannel"]]
--   }
-- }
--

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
      googletasks = rcGoogleTaskSources runcfg
      descGithub (GitHubSource user project tags) =
        let tagstr = if length tags > 0
                     then " with tags " ++ (intercalate ", " tags)
                     else ""
        in user ++ "/" ++ project ++ tagstr
      descGoogleCode (GoogleCodeSource repo terms) = repo ++ (
        if length terms > 0
        then " with search terms " ++ (intercalate ", " terms)
        else "")
      descGoogleTasks (GoogleTasksSource u _ _ _ lp) =
        u ++ "/[" ++ (intercalate ";" $ map show lp) ++ "]"
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
  if length googletasks > 0
     then do putStrLn $ "Will scan Google Tasks for these projects:\n\t" ++ (
               intercalate "\n\t" $ map descGoogleTasks googletasks)
     else return ()
  putStrLn $ "Will put new issues into " ++ output


loadConfig :: DCT.Config -> IO (Maybe RunConfiguration)
loadConfig config = do
  environ <- getEnvironment
  let keyMap = HM.fromList environ
      home = HM.lookupDefault "" "HOME" keyMap
  configmap <- DC.getMap config
  file_patterns <- DC.lookup config "scan_files"
  file_list <- loadFileGlob file_patterns
  stub_patterns <- DC.lookup config "stub_files"
  stub_list <- loadFileGlob stub_patterns
  raw_output_file <- DC.lookup config "output_file"
  let output_file = fmap (resolveTilde home) raw_output_file
  raw_cache <- DC.lookup config "cache_dir"
  let cache = fmap (resolveTilde home) raw_cache
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
      gt_list = loadGTSources home configmap
      gh_auth = fmap T.unpack github_oauth
  return $ RunConfiguration <$> file_list <*> stub_list <*> output_file <*>
    (pure gh_auth) <*> (pure gh_list) <*> (pure gc_list) <*>
    (pure gt_list) <*> (pure cache_dir)

-- | Returns immediate children of root.
getImmediateChildren :: HM.HashMap DCT.Name DCT.Value -> [T.Text]
getImmediateChildren hmap = nub $ map (T.takeWhile (/= '.')) $ HM.keys hmap

-- | Returns immediate children of the given parent.
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

-- |Get a list of list of Strings, unless it's really just a list of
-- strings, in which case we just have one inner list.  Look at the
-- first value type in the keyed list to determine which.
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

-- | Force result into [[String]] result, even if the inner list only
-- has one element.
multiListForced :: HM.HashMap DCT.Name DCT.Value -> T.Text -> [[String]]
multiListForced config key =
  let res = HM.lookup key config
      convList :: [DCT.Value] -> [[String]]
      convList [] = []
      convList (x:xs) =
        case x of
          DCT.List ys ->
            (map T.unpack $ mapMaybe DCT.convert ys):(convList xs)
          DCT.String s ->
            [T.unpack s]:(convList xs)
          otherwise -> []
  in case res of
    Nothing -> []
    Just (DCT.List xs) ->
      convList xs
    otherwise -> trace ("Key " ++ (show key) ++ " was not a list, but: " ++ show res) []

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

{-
 github {
  auth_token = ""
   repo {
      project {
        tags = []
      }
   }
 }
-}
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


{-
 Parses a google-tasks block.
 google-tasks {
   gmail {
     oauth-file = "filename.auth"
     user = "gmail@user.account"
     client-id = ""
     client-secret = ""
     lists = [[".*", "TAG1", "TAG2"], ["FooBar", "FOO", "BAR"]]
   }
 }
-}
compilePattern :: [String] -> GoogleTasksPattern
compilePattern (('+':rest):tags) =
  let matchId task =
        let matches = (GT.tlId task) == rest
            matchDesc = "[" ++ rest ++ ": matching " ++ GT.tlId task ++ ": " ++
                        show matches ++ "]"
        in if matches then Just tags else Nothing
  in GoogleTasksPattern rest tags matchId

compilePattern (pat:tags) =
  let matchTitle task =
        let matches = GT.tlTitle task =~ pat :: Bool
            matchDesc = "[" ++ pat ++ ": matching " ++ GT.tlTitle task ++ ": " ++
                        show matches ++ "]"
        in if matches then Just tags else Nothing
  in GoogleTasksPattern pat tags matchTitle

loadGTSources :: String -> HM.HashMap DCT.Name DCT.Value -> [GoogleTasksSource]
loadGTSources home config =
  let sources = getChildrenOf config "google-tasks"
      getSource src =
        let key s = "google-tasks." `T.append` src `T.append` "." `T.append` s
            raw_oauth = (HM.lookup (key "oauth-file") config) >>= DCT.convert
            oauth :: Maybe FilePath
            oauth = fmap (resolveTilde home) raw_oauth
            clientId = (HM.lookup (key "client-id") config) >>= DCT.convert
            clientSecret = (HM.lookup (key "client-secret") config) >>= DCT.convert
            user = (HM.lookup (key "user") config) >>= DCT.convert
            patterns = fmap compilePattern $ multiListForced config (key "lists")
            client = OA.OAuth2Client <$> clientId <*> clientSecret
            sources = [GoogleTasksSource <$> (pure $ T.unpack src) <*>
                       (fmap T.unpack user) <*> (pure oauth) <*> client <*>
                       (pure patterns)]
        in catMaybes sources
  in concatMap getSource sources

resolveTilde :: String -> String -> String
resolveTilde home ('~':'/':path) = home ++ "/" ++ path
resolveTilde home path = path

loadFileGlob :: Maybe [String] -> IO (Maybe [FilePath])
loadFileGlob pats =
  case pats of
    (Just xs) -> do environ <- getEnvironment
                    let keyMap = HM.fromList environ
                        home = HM.lookupDefault "" "HOME" keyMap
                    files <- mapM glob $ map (resolveTilde home) xs
                    putStrLn $ "Globbed (" ++ (intercalate ", " xs) ++ ") to:" ++  (intercalate ", " $ concat files)
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
