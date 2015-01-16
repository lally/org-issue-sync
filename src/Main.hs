{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import qualified Data.Text as T
import qualified Sync.Retrieve.GoogleCode.GoogleCode as GC
import qualified Sync.Retrieve.GitHub.GitHub as GH
import qualified Data.HashMap.Strict as HM
import Data.List (nub)
import Data.Maybe
import Sync.Push.OrgMode
import Sync.Retrieve.OrgMode.OrgMode
import Sync.Issue.Issue
import Data.List (intercalate)
import System.IO

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
            repoNameStr = maybe "(invalid google-code source)" T.unpack $ DCT.convert $ fromJust repoName
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
            repoUserStr = maybe "(invalid github user)" T.unpack $ DCT.convert $ fromJust repoUser
            repoProject = HM.lookup (repo `T.append` ".project") config
            repoProjectStr = maybe "(invalid github project)" T.unpack $ DCT.convert $ fromJust repoProject
        in if isJust repoUser && isJust repoProject
           then Just $ GitHubSource repoUserStr repoProjectStr
           else Nothing
  in mapMaybe getRepoData repos

loadConfig :: DCT.Config -> IO ([GoogleCodeSource], [GitHubSource])
loadConfig config = do
  gcmap <- DC.getMap $ DC.subconfig "google-code" config
  ghmap <- DC.getMap $ DC.subconfig "github" config
  return (loadGCSources gcmap, loadGHSources ghmap)

main :: IO ()
main = do
  let filename = "/home/lally/Work/org-issue-sync/test.org"
      auth = "REPLACE_ME_WITH_AN_ACCESS_TOKEN"
  firstIssues <- GC.fetch "webrtc" ["lally@webrtc.org"]
  otherIssues <- GH.fetch Nothing "uproxy" "uproxy" (Just Open) []
  lastIssues <- GC.fetch "chromium" ["lally@chromium.org"]
  let issues = firstIssues ++ otherIssues ++ lastIssues
  putStrLn $ "============================================\n"
  putStrLn $ "Fetched issues: " ++ (intercalate "\n" $ map show issues)
  fh <- openFile filename ReadMode
  oldFileText <- hGetContents fh
  let !len = length oldFileText
  oldIssues <- getOrgIssues filename $ oldFileText
  let deltas = getIssueDeltas oldIssues issues
  putStrLn $ "============================================\n"
  putStrLn $ (show $ length $ newIssues deltas) ++ " new issues, and " ++
    (show $ length $ changes deltas) ++ " issues changed properties."
  putStrLn $ "============================================\n"
  putStrLn $ "Changed issues: \n" ++ (intercalate "\n" $ map show $ changes deltas)
  putStrLn $ "============================================\n"
  putStrLn $ "Writing new issues to " ++ filename
  putStrLn $ "============================================\n"
  appendIssues filename (newIssues deltas)
  return ()

