import Sync.Retrieve.GoogleCode.GoogleCode
import Data.List (intercalate) 
main :: IO ()
main = do
	issues <- fetch "webrtc" ["lally@webrtc.org"]
        putStrLn $ "Fetched issues: " ++ (intercalate "\n" $ map show issues)
        return ()

