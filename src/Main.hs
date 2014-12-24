{-# LANGUAGE BangPatterns #-}
import qualified Sync.Retrieve.GoogleCode.GoogleCode as GC
import qualified Sync.Retrieve.GitHub.GitHub as GH
import Sync.Push.OrgMode
import Sync.Retrieve.OrgMode.OrgMode
import Sync.Issue.Issue
import Data.List (intercalate)
import System.IO

main :: IO ()
main = do
  let filename = "/home/lally/Work/org-issue-sync/test.org"
      auth = "REPLACE_ME_WITH_AN_ACCESS_TOKEN"
  firstIssues <- GC.fetch "webrtc" ["lally@webrtc.org"]
  otherIssues <- GH.fetch Nothing "uproxy" "uproxy" (Just Open) []
  let issues = firstIssues ++ otherIssues
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

