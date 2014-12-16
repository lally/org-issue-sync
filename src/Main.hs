import Sync.Retrieve.GoogleCode.GoogleCode
import Sync.Push.OrgMode
import Data.List (intercalate) 
main :: IO ()
main = do
  let filename = "/home/lally/Work/org-issue-sync/test.org"
  issues <- fetch "webrtc" ["lally@webrtc.org"]
  putStrLn $ "Fetched issues: " ++ (intercalate "\n" $ map show issues)
  putStrLn $ "Writing to " ++ filename
  appendIssues filename issues
  return ()

