import Sync.Retrieve.GoogleCode.GoogleCode
import Sync.Push.OrgMode
import Sync.Retrieve.OrgMode.OrgMode
import Data.List (intercalate)

main :: IO ()
main = do
  let filename = "/home/lally/Work/org-issue-sync/test.org"
  issues <- fetch "webrtc" ["lally@webrtc.org"]
  putStrLn $ "============================================\n"
  putStrLn $ "Fetched issues: " ++ (intercalate "\n" $ map show issues)
  oldIssues <- getOrgIssues filename
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

