{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import qualified Data.Text as T
import qualified Sync.Retrieve.GoogleCode.GoogleCode as GC
import qualified Sync.Retrieve.GitHub.GitHub as GH
import qualified Data.HashMap.Strict as HM
import Control.Applicative
import Control.Monad (liftM2, mplus)
import Control.Monad.Catch (catchIOError)
import Data.List (nub, (\\))
import Data.Maybe
import Sync.Push.OrgMode
import Sync.Retrieve.OrgMode.OrgMode
import Sync.Issue.Issue
import Data.List (intercalate)
import System.IO
import System.FilePath.Glob

-- Config Format:
-- repo-type {
--   _param = value
--   name { params }
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

loadGCSources :: HM.HashMap DCT.Name DCT.Value -> [GoogleCodeSource]
loadGCSources config =
  let repos = getImmediateChildren config
      pullList :: DCT.Value -> [String]
      pullList (DCT.List els) = map T.unpack $ mapMaybe DCT.convert els
      pullList _ = []
      getRepoData :: T.Text -> Maybe GoogleCodeSource
      getRepoData repo =
        let repoName = HM.lookup (repo `T.append` ".repo") config
            repoNameCvt = DCT.convert $ fromJust repoName
            repoNameStr = maybe "(invalid google-code source)" T.unpack $ repoNameCvt
            repoTerms = HM.lookupDefault (DCT.List []) (repo `T.append` ".terms") config
        in if isJust repoName
           then Just $ GoogleCodeSource repoNameStr $ pullList repoTerms
           else Nothing
  in mapMaybe getRepoData repos

loadGHSources :: HM.HashMap DCT.Name DCT.Value -> [GitHubSource]
loadGHSources config =
  let repos = getImmediateChildren config
      getRepoData :: T.Text -> Maybe GitHubSource
      getRepoData repo =
        let repoUser = HM.lookup (repo `T.append` ".user") config
            repoNameCvt = DCT.convert $ fromJust repoUser
            repoUserStr = maybe "(invalid github user)" T.unpack repoNameCvt
            repoProject = HM.lookup (repo `T.append` ".projects") config
            repoProjectStr = maybe "(invalid github project)" T.unpack $ DCT.convert $ fromJust repoProject
        in if isJust repoUser && isJust repoProject
           then Just $ GitHubSource repoUserStr repoProjectStr []
           else Nothing
  in mapMaybe getRepoData repos

data RunConfiguration = RunConfiguration
                        { rcScanFiles :: [FilePath]
                        , rcOutputFile :: FilePath
                        , rcGitHubSources :: [GitHubSource]
                        , rcGoogleCodeSources :: [GoogleCodeSource]
                        } deriving (Eq, Show)

loadConfig :: DCT.Config -> IO (Maybe RunConfiguration)
loadConfig config = do
  gcmap <- DC.getMap $ DC.subconfig "google-code" config
  ghmap <- DC.getMap $ DC.subconfig "github" config
  file_patterns <- DC.lookup config "scan_files" -- :: IO (Maybe [T.Text])
  raw_file_list <- case file_patterns of
                        (Just xs) -> do files <- mapM glob xs
                                        return (Just $ concat files)
                        Nothing -> return Nothing
  output_file <- DC.lookup config "output_file" -- :: IO (Maybe T.Text)
  let gh_list = loadGHSources ghmap
      gc_list = loadGCSources gcmap
      file_list = Just raw_file_list
  return $ RunConfiguration <$> raw_file_list <*> output_file <*> (Just gh_list) <*> (Just gc_list)

loadOrgIssues :: FilePath -> IO ([Issue])
loadOrgIssues file = do
  fh <- openFile file ReadMode
  fileText <- hGetContents fh
  let !len = length fileText
  issues <- getOrgIssues fileText
  return issues

runConfiguration :: RunConfiguration -> IO ()
runConfiguration (RunConfiguration scan_files output github googlecode) = do
  -- Scan all the existin files.
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

