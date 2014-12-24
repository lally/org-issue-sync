module Sync.Push.OrgMode where

import Sync.Issue.Issue
import Text.StringTemplate
import Data.Char (toUpper)
import Data.List (intercalate)

-- Dumb v0.1: just splat issues at the end of files.
makeIssueOrgHeading :: Int -> Issue -> String
makeIssueOrgHeading depth issue =
  let templateStr = unlines [
        "$prefix$ $TODO$ $summary$ $tags$",
        "$indent$ :PROPERTIES:",
        "$indent$ :ISSUENUM: $num$",
        "$indent$ :ISSUEORIGIN: $origin$",
        "$indent$ :ISSUEUSER: $user$",
        "$indent$ :END:\n"]
      attribs = [("prefix", take depth $ repeat '*'),
                 ("indent", take depth $ repeat ' '),
                 ("TODO", map toUpper $ show $ status issue),
                 ("num", show $ number issue),
                 ("summary", summary issue),
                 ("tags", if length (tags issue) > 0
                          then ":" ++ (intercalate ":" $ tags issue) ++ ":"
                          else ""),
                 ("origin", origin issue),
                 ("user", user issue)]
      template = newSTMP templateStr
      filledTempl = setManyAttrib attribs template
  in toString filledTempl

appendIssues :: FilePath -> [Issue] -> IO ()
appendIssues file issues = do
  let headings = intercalate "\n" $ map (makeIssueOrgHeading 2) issues
  appendFile file headings

