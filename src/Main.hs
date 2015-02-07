{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import qualified Data.Text as T
import qualified Sync.Retrieve.GoogleCode.GoogleCode as GC
import qualified Sync.Retrieve.GitHub.GitHub as GH
import qualified Data.HashMap.Strict as HM
import OrgSync
import Control.Applicative
import Control.Monad (liftM2, liftM3, mplus, join)
import Control.Monad.Catch (catchIOError)
import Data.List (nub, (\\), sort, intersect, groupBy)
import Debug.Trace
import Data.Maybe
import Sync.OrgMode
import Sync.Issue
import Data.List (intercalate)
import System.Console.GetOpt
import System.Environment (getArgs)
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

data CommandOptions = Options
                      { optPrintConfig :: Bool
                      , optCommandFile :: String
                      , optWriteOutput :: Bool
                      , optFetchIssues :: Bool
                      , optScanOutput :: Bool
                      , optDumpIssues :: Bool
                      , optDiffFile :: String
                      } deriving (Eq, Show)

defaultOptions = Options
  { optPrintConfig = False
  , optCommandFile = "./org-issue-sync.conf"
  , optWriteOutput = True
  , optFetchIssues = True
  , optScanOutput = True
  , optDumpIssues = False
  , optDiffFile = ""}

options :: [OptDescr (CommandOptions -> CommandOptions)]
options =
    [ Option ['v']     ["verbose"]
        (NoArg (\ opts -> opts { optPrintConfig = True }))
        "Print run configuration and its evaluation."
    , Option ['d']     ["dryrun"]
        (NoArg (\ opts -> opts { optFetchIssues = False,
                                 optWriteOutput = False }))
        "Do no actual I/O."
    , Option ['i']     ["input"]
        (ReqArg (\ f opts -> opts { optCommandFile = f }) "FILE")
        "Configuration file name."
    , Option ['f']     ["nofetch"]
        (NoArg (\ opts -> opts { optFetchIssues = False }))
        "Do not fetch issues."
    , Option ['s']     ["scaninput"]
        (NoArg (\ opts -> opts { optScanOutput = True }))
        "Always scan the output file for issues."
    , Option ['w']     ["nowrite"]
        (NoArg (\ opts -> opts { optWriteOutput = False }))
        "Do not write new issues to output file."
    , Option ['D']     ["dump"]
        (NoArg (\ opts -> opts { optDumpIssues = True }))
        "Dump all issues to stdout"
    , Option ['d']     ["diff"]
        (ReqArg (\ f opts -> opts { optDiffFile = f }) "FILE")
        "Diff scan_files against this file. Implies -f"
    ]

main :: IO ()
main = do
  argv <- getArgs
  case getOpt Permute options argv of
    (_,_,errs@(e:es)) -> ioError (
      userError (concat errs ++ usageInfo "Org Issue Sync" options))
    (o,n,[]  ) ->
      do let opts = foldl (flip id) defaultOptions o
         if length n > 0
           then do putStrLn $ "Ignoring unused arguments: " ++ (
                     intercalate " " n)
           else return ()
         raw_configs <- DC.load [ DCT.Required (optCommandFile opts) ]
         config <- loadConfig raw_configs
         if isNothing config
           then do putStrLn "No valid configuration found.  Exiting."
                   res <- exitFailure
                   exitWith res
           else return ()
         let (Just prerunconfig) = config
         let runconfig = if optScanOutput opts
                         then prerunconfig {
                           rcScanFiles = (rcOutputFile prerunconfig):(
                              rcScanFiles prerunconfig) }
                         else prerunconfig
         if optPrintConfig opts
           then describeConfiguration runconfig
           else return ()
         runConfiguration runconfig (optFetchIssues opts) (
           optWriteOutput opts) (optPrintConfig opts)
         res <- exitSuccess
         exitWith res
