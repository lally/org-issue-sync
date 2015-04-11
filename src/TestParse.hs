{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Data.List (intercalate)
import Data.OrgMode
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.FilePath.Glob

data TOutputMode = TODiff | TOResult deriving (Eq, Show)

data TOptions = TOptions { toOutput :: TOutputMode } deriving (Eq, Show)

dfltOptions = TOptions { toOutput = TODiff }

options :: [OptDescr (TOptions -> TOptions) ]
options =
  [ Option ['d'] ["diffout"]
      (NoArg (\opts -> opts { toOutput = TODiff }))
      "Output a diff of input and process output."
  , Option ['p'] ["parseout"]
      (NoArg (\opts -> opts { toOutput = TOResult }))
      "Output the parse result of parsing the input."
  ]

processFile :: TOptions -> String -> IO ()
processFile opts fname = do
  infile <- readFile fname
  let parsed = orgFile infile
      intext = lines infile
      outtext = (map tlText $ getTextLines parsed)
      diffsFound = getGroupedDiff intext outtext
      fmtDiff :: Diff [String] -> String
      fmtDiff (First s) =
        "- " ++ (intercalate "\n- " s)
      fmtDiff (Second s) =
        "+ " ++ (intercalate "\n+ " s)
      fmtDiff (Both a b) =
        let len = max (length a) (length b)
            as :: [String]
            as = a ++ repeat ">>>"
            bs :: [String]
            bs = b ++ repeat "<<<"
            merge :: (String, String) -> String
            merge (a,b) = a ++ " -> " ++ b
        in if a == b
           then "(no difference)"
           else intercalate "\n" $ take len $ map merge $ zip as bs
      diff :: String
      diff = if length diffsFound > 0
             then let ndiffs = show $ length diffsFound
                      prefix = ndiffs ++ " differences found:\n"
                      report :: String
                      report = intercalate "\n" $ map fmtDiff diffsFound
                  in prefix ++ report
             else "No differences."
      result = case toOutput opts of
        TODiff -> diff
        TOResult -> intercalate "\n" outtext
  putStrLn result


-- |A utility to parse input text and show diff-outputs after
-- different simple transforms.  For testing and debugging.
main :: IO ()
main = do
  argv <- getArgs
  case getOpt Permute options argv of
    (_, _, errs@(e:es)) -> ioError (
      userError (concat errs ++ usageInfo "Org Test Parse" options))
    (o, files, []) -> do
      let opts = foldl (flip id) dfltOptions o
      mapM_ (processFile opts) files
