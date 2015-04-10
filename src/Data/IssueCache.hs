module Data.IssueCache where

import Data.Aeson
import Data.Issue
import Data.IssueJSON
import Data.List (intercalate)
import Data.Maybe
import System.IO
import System.Directory
import System.FilePath.Glob
import qualified Data.ByteString.Lazy as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy.Char8 as BSC (unpack)

data IssuePath = IssuePath { ipType :: String
                           , ipOrigin :: String
                           , ipNumber :: Int
                           } deriving (Eq, Show)
-- A directory
data IssueStore = IssueStore FilePath deriving (Eq, Show)

makePath :: Issue -> IssuePath
makePath iss =
  IssuePath (iType iss) (origin iss) (number iss)

getFileDir :: IssueStore -> IssuePath -> FilePath
getFileDir (IssueStore dir) (IssuePath ty o num) =
  let xlate c
        | c == '/' = '_'
        | otherwise = c
  in dir ++ "/" ++ ty ++ "/" ++ (map xlate o)

getFileName :: IssueStore -> IssuePath -> FilePath
getFileName st p@(IssuePath ty o num) =
  (getFileDir st p) ++ "/" ++ (show num) ++ ".json"

makeStore :: FilePath -> IO (Maybe IssueStore)
makeStore path = do
  is_dir <- doesDirectoryExist path
  if is_dir
    then return (Just (IssueStore path))
    else return Nothing

saveIssue :: IssueStore -> Issue -> IO ()
saveIssue store iss = do
  let ipath = makePath iss
      fname = getFileName store ipath
      dirname = getFileDir store ipath
      encoded = encode iss
  createDirectoryIfMissing True dirname
  BS.writeFile fname encoded

tryReadIssue :: FilePath -> IO (Maybe Issue)
tryReadIssue fname = do
        readable <- doesFileExist fname
        if readable
          then do contents <- BS.readFile fname
                  return $ decode contents
          else return Nothing

lookupIssues :: IssueStore -> [IssuePath] -> IO ([Maybe Issue])
lookupIssues store paths = do
  let getFilePath path = getFileName store path
  mapM tryReadIssue $ map getFilePath paths

loadAllIssues :: IssueStore -> IO ([Issue])
loadAllIssues (IssueStore dir) = do
  let pattern = "*/*/*.json"
      loadIssue fname = do
        contents <- BS.readFile fname
        return $ decode contents
  paths <- globDir1 (compile pattern) dir
  loaded <- mapM loadIssue paths
  return $ catMaybes loaded
