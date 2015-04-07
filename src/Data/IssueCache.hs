module Data.IssueCache where

import qualified Data.ByteString.Lazy as BS (readFile, writeFile)
import Data.Issue
import Data.IssueJSON
import Data.Aeson
import System.IO
import System.Directory

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

lookupIssues :: IssueStore -> [IssuePath] -> IO ([Maybe Issue])
lookupIssues store paths = do
  let tryReadIssue :: IssuePath -> IO(Maybe Issue)
      tryReadIssue path = do
        let fname = getFileName store path
        readable <- doesFileExist fname
        if readable
          then do contents <- BS.readFile fname
                  return $ decode contents
          else return Nothing
  mapM tryReadIssue paths
