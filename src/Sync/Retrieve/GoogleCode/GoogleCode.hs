module Sync.Retrieve.GoogleCode.GoogleCode where

import qualified Data.Csv as CSV
import Data.Char (toLower)
import Data.Vector (Vector, toList)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Sync.Issue.Issue
import Network.HTTP
import Network.URI

-- ID%20Pri%20Mstone%20ReleaseBlock%20Area%20Status%20Owner%20Summary
type CSVRow = (Int, Int, String, String, String, String, String, String, String)

-- | Takes a project and a list of tags, returns [Issue]
fetch :: String -> [String] -> IO ([Issue])
fetch project tags = do
   let esctags = map (escapeURIString isUnescapedInURI) tags
       query = intercalate "%20" esctags
       uri = "http://code.google.com/p/" ++ project ++ "/issues/csv?can=2&q=" ++
             query ++ "&colspec=ID%20Pri%20Mstone%20ReleaseBlock%20Area%20Status%20Owner%20Summary"
   body <- simpleHTTP (getRequest uri) >>= getResponseBody

   -- ID, Pri,	Mstone, ReleaseBlock,	Area,	Status, Owner, Summary, Labels
   let res = (CSV.decode CSV.HasHeader $ pack body) :: Either String (Vector (Int, Int, String, String, String, String, String, String, String))
   let xlate stat = maybe Open id $ M.lookup stat $ M.fromList [("assigned", Open), ("closed", Closed), ("open", Open)]
       makeIssue (id, prio, mstone, relblock, area, status, owner, summary, labels) =
         Issue project id owner (xlate $ map toLower status) (splitOn "," labels)
   case res of
     Left err -> do putStrLn $ "Failed parse from '" ++ uri ++ "': " ++ err
                    return []
     Right vals -> return $ map makeIssue $ toList vals
