module Sync.Push.OrgMode where

import Sync.Issue.Issue
import Text.StringTemplate
import Data.Char (toUpper, isAlphaNum)
import Data.List (intercalate)

-- https://github.com/freedomjs/freedom-pgp-e2e/issues/6#issuecomment-69795153
-- https://code.google.com/p/webrtc/issues/detail?id=3592
makeIssueUrl :: Issue -> String
makeIssueUrl issue =
  let num = show $ number issue
      org = origin issue
  in if '/' `elem` org
     then "https://www.github.com/" ++ org ++ "/issues/" ++ num
     else "https://code.google.com/p/" ++ org ++ "/issues/detail?id=" ++ num

-- Dumb v0.1: just splat issues at the end of files.
makeIssueOrgHeading :: Int -> Issue -> String
makeIssueOrgHeading depth issue =
  let cleanChar c
        | isAlphaNum c = c
        | c == '-' = c
        | otherwise = '_'
      cleanTag tag = map cleanChar tag
      templateStr = unlines [
        "$prefix$ $TODO$ $summary$ $tags$",
        "$indent$ :PROPERTIES:",
        "$indent$ :ISSUENUM: $num$",
        "$indent$ :ISSUEORIGIN: $origin$",
        "$indent$ :ISSUEUSER: $user$",
        "$indent$ :ISSUETYPE: $type$",
        "$indent$ :END:\n",
        "$indent$ - [[$url$][Issue Link]]\n"]
      attribs = [("prefix", take depth $ repeat '*'),
                 ("indent", take depth $ repeat ' '),
                 ("TODO", map toUpper $ show $ status issue),
                 ("num", show $ number issue),
                 ("summary", summary issue),
                 ("tags", if length (tags issue) > 0
                          then ":" ++ (intercalate ":" $ map cleanTag $ tags issue) ++ ":"
                          else ""),
                 ("type", iType issue),
                 ("origin", origin issue),
                 ("url", makeIssueUrl issue),
                 ("user", user issue)]
      template = newSTMP templateStr
      filledTempl = setManyAttrib attribs template
  in toString filledTempl

appendIssues :: FilePath -> [Issue] -> IO ()
appendIssues file issues = do
  let headings = intercalate "\n" $ map (makeIssueOrgHeading 2) issues
  appendFile file headings

