{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import qualified Data.Text as T
import qualified Sync.Retrieve.GoogleCode.GoogleCode as GC
import qualified Sync.Retrieve.GitHub.GitHub as GH
import qualified Data.HashMap.Strict as HM
import Control.Applicative
import Control.Monad (liftM2, mplus, join)
import Control.Monad.Catch (catchIOError)
import Data.List (nub, (\\))
import Debug.Trace
import Data.Maybe
import Sync.Push.OrgMode
import Sync.Retrieve.OrgMode.OrgMode
import Sync.Issue.Issue
import Data.List (intercalate)
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

traceShowArg :: (Show a) => String -> a -> a
traceShowArg s a = trace (s ++ ": " ++ (show a)) a

getChildrenOf :: HM.HashMap DCT.Name DCT.Value -> T.Text -> [T.Text]
getChildrenOf hmap parent =
  let prefix = parent `T.append` "."
      prefixLen = T.length prefix
      allKeys = traceShowArg ((show prefix) ++ ": All keys") $ HM.keys hmap
      allChildren = traceShowArg ((show parent) ++ ": Children") $ filter (T.isPrefixOf prefix) allKeys
  in nub $ map (T.takeWhile (/= '.')) $ map (T.drop prefixLen) $ allChildren

valToList :: DCT.Value -> [String]
valToList (DCT.List els) = map T.unpack $ mapMaybe DCT.convert els
valToList _ = []

getMultiList :: HM.HashMap DCT.Name DCT.Value -> T.Text -> [[String]]
getMultiList config key =
  let res = traceShowArg ("Looking up key " ++ show key) $ HM.lookup key config in
  case res of
    Nothing -> []
    -- Look at first element.  If it's a string, convert the key as
    -- [String], otherwise as [[String]]
    Just (DCT.List (x:xs)) ->
      case x of
        DCT.String s ->
          [mapMaybe (\s -> T.unpack <$> DCT.convert s) (x:xs)]
        DCT.List ys ->
          traceShowArg ("unwinding list of lists" ) $ filter (\l -> length l > 0) $ map valToList (x:xs)
        otherwise -> []
    otherwise -> trace ("looking up key " ++ (show key) ++ " results in " ++ show res) []

loadGCSources :: HM.HashMap DCT.Name DCT.Value -> [GoogleCodeSource]
loadGCSources config =
  let repos = traceShowArg "Getting google-code children" $ getChildrenOf config "google-code.projects"
      -- TODO: make this just [GoogleCodeSource]
      getRepoData :: T.Text -> [GoogleCodeSource]
      getRepoData repo =
        let rPlus s = repo `T.append` s
--            repoName = join $ DCT.convert <$> HM.lookup (rPlus ".repo") config
            repoTerms = getMultiList config ("google-code.projects." `T.append` repo `T.append` ".terms")
            makeGC :: [String] -> GoogleCodeSource
            makeGC term = GoogleCodeSource (T.unpack repo) term
        in if length repoTerms > 0
           then map makeGC repoTerms
           else [makeGC []]
  in concatMap getRepoData repos

loadGHSources :: HM.HashMap DCT.Name DCT.Value -> [GitHubSource]
loadGHSources config =
  let repos = traceShowArg "Getting github children" $ getChildrenOf config "github.projects"
      getRepoData :: T.Text -> [GitHubSource]
      getRepoData repo =
        let repoName = T.append "github.projects." repo
            projectNames = getChildrenOf config repoName
        in traceShowArg ((show repo) ++ " (prefix is " ++ (show repoName) ++ ") with project names " ++ (intercalate ", " $ map show projectNames))  $ concatMap (getProjectData repo) projectNames

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

data RunConfiguration = RunConfiguration
                        { rcScanFiles :: [FilePath]
                        , rcOutputFile :: FilePath
                        , rcGitHubSources :: [GitHubSource]
                        , rcGoogleCodeSources :: [GoogleCodeSource]
                        } deriving (Eq, Show)

loadConfig :: DCT.Config -> IO (Maybe RunConfiguration)
loadConfig config = do
  configmap <- DC.getMap config
  file_patterns <- DC.lookup config "scan_files" -- :: IO (Maybe [T.Text])
  raw_file_list <- case file_patterns of
                        (Just xs) -> do files <- mapM glob xs
                                        return (Just $ concat files)
                        Nothing -> return Nothing
  output_file <- DC.lookup config "output_file" -- :: IO (Maybe T.Text)
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

runConfiguration :: RunConfiguration -> IO ()
runConfiguration (RunConfiguration scan_files output github googlecode) = do
  -- Scan all the existing files.
  existing_issues <- mapM loadOrgIssues scan_files
  -- Load the issues from our sources
  let loadGHSource (GitHubSource user project tags) =
        GH.fetch Nothing user project (Just Open) tags
      loadGCSource (GoogleCodeSource repo terms) =
        GC.fetch repo terms
  gh_issues <- mapM loadGHSource github
  gc_issues <- mapM loadGCSource googlecode
  let new_issues = (concat (gh_issues ++ gc_issues)) \\ (concat existing_issues)
  appendIssues output new_issues
  return ()

main :: IO ()
main = do
  let filename = "/home/lally/Work/org-issue-sync/test.org"
      auth = "REPLACE_ME_WITH_AN_ACCESS_TOKEN"
      config_file = "/home/lally/Work/org-issue-sync/org-issue-sync.conf"
  raw_configs <- DC.load [ DCT.Required config_file ]
  config <- loadConfig raw_configs
  putStrLn ">>> Configuration loaded: "
  putStrLn $ show config
  res <- exitSuccess
  exitWith res

  firstIssues <- GC.fetch "webrtc" ["lally@webrtc.org"]
  otherIssues <- GH.fetch Nothing "uproxy" "uproxy" (Just Open) []
  lastIssues <- GC.fetch "chromium" ["lally@chromium.org"]

  let issues = firstIssues ++ otherIssues ++ lastIssues
  putStrLn $ "============================================\n"
  putStrLn $ "Fetched " ++ (show $ length issues) ++ " issues"
  oldIssues <- catchIOError (loadOrgIssues filename) (\_ -> return [])
  let deltas = getIssueDeltas oldIssues issues
  putStrLn $ "============================================\n"
  putStrLn $ (show $ length $ newIssues deltas) ++ " new issues, and " ++
    (show $ length $ changes deltas) ++ " issues changed properties."
  putStrLn $ "============================================\n"
  putStrLn $ "Changed issues: \n" ++ (show $ changes deltas)
  putStrLn $ "============================================\n"
  putStrLn $ "Writing new issues to " ++ filename
  putStrLn $ "============================================\n"
  appendIssues filename (newIssues deltas)
  return ()

