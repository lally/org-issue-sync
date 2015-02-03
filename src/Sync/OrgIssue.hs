module Sync.OrgIssue where
import Sync.OrgMode
import Sync.Issue
import Control.Monad
import Data.Char (toUpper, isAlphaNum)
import Data.List
import Data.Maybe (mapMaybe, fromJust, catMaybes)
import Debug.Trace (trace)
import Text.Parsec
import Text.Regex.Posix
import Text.StringTemplate

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
  let templateStr = unlines [
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
                          then ":" ++ (intercalate ":" $ tags issue) ++ ":"
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

getOrgIssue :: Node -> Maybe Issue
getOrgIssue n =
  let draw = propDrawer n
      hasOrigin = hasKey "ISSUEORIGIN" draw
      hasNum = hasKey "ISSUENUM" draw
      hasUser = hasKey "ISSUEUSER" draw
      hasType = hasKey "ISSUETYPE" draw
      drawerOf (ChildDrawer d) = Just d
      drawerOf _ = Nothing
      drawersOf nd = mapMaybe drawerOf $ nChildren nd
      drawerNameIs s d = drName d == s
      hasPropDrawer nd = any (drawerNameIs "PROPERTIES") $ drawersOf nd
      propDrawer nd = head $ filter (drawerNameIs "PROPERTIES") $ drawersOf nd
      hasKey k d = let matched = filter (\(kk,_) -> kk == k) $ drProperties d
                   in length matched > 0
      valOf k d = let matched = filter (\(kk,_) -> kk == k) $ drProperties d
                  in head $ map snd matched
      mapStatus Nothing = Open
      mapStatus (Just (Prefix s)) = case s of
        "ACTIVE" -> Active
        "CLOSED" -> Closed
        "DONE" -> Closed
        "TODO" -> Open
        "OPEN" -> Open
        _ -> Open
  in if (hasPropDrawer n && hasOrigin && hasNum && hasUser && hasType)
     then Just $ Issue (valOf "ISSUEORIGIN" draw) (read $ valOf "ISSUENUM" draw) (
       valOf "ISSUEUSER" draw) (mapStatus $ nPrefix n) (nTags n) (nTopic n) (
       valOf "ISSUETYPE" draw)
     else Nothing

-- |Pull special properties from the argument Node, and generate a new
-- TextLine to replace the header line of that Node, representing this
-- (changed) Issue.  Presumably, the Node represents an older revision
-- of this Issue.  Wipe all tags *except# those with an @ in them,
-- which we don't generate ourselves, but must have been put in by a
-- user.
updateNodeIssue :: Issue -> Node -> TextLine
updateNodeIssue iss nd =
  TextLine 0 text lineno
  where text = prefix ++ " " ++ todo ++ " " ++ summ ++ (all_tags)
        prefix = take 2 $ repeat '*'
        lineno = tlLineNum $ head $ getTextLines nd
        todo = case (status iss) of
          Open -> "OPEN"
          Active -> "ACTIVE"
          Closed -> "CLOSED"
        summ = summary iss
        preseved_tags = filter (elem '@') $ nTags nd
        cleanChar c
          | isAlphaNum c = c
          | otherwise = '_'
        cleanTag tag = map cleanChar tag
        clean_tags = map cleanTag $ tags iss
        all_tags = if (length (tags iss) > 0)
                   then " :" ++ (intercalate ":" (clean_tags ++ preseved_tags)) ++ ":"
                   else ""

instance NodeUpdate Issue where
  updateNodeLine = updateNodeIssue

getOrgIssues :: String -> [Issue]
getOrgIssues contents =
  let doc = orgFile contents
  in map fst $ ovElements $ generateDocView getOrgIssue doc

data IssueChanges = IssueChanges
                    { newIssues :: [Issue]
                    , changes :: [(String, Int, [IssueDelta])]
                    } deriving (Eq)

instance Show IssueChanges where
  show (IssueChanges new changed) =
    let summary iss = origin iss ++ "#" ++ (show $ number iss)
        showChg (a, b, _) = a ++ "#" ++ (show b)
        firstHeader = (show $ length new) ++ " new issues: \n   "
        firstBody = intercalate "\n   " $ map summary new
        secondHeader = "\nAnd " ++ (show $ length changed) ++ " issues changed: \n   "
        secondBody = intercalate "\n   " $ map showChg changed
    in  firstHeader ++ firstBody ++ secondHeader ++ secondBody

getIssueDeltas :: [Issue] -> [Issue] -> IssueChanges
getIssueDeltas prior cur =
  let priors = zip prior prior
      curs = zip cur cur
      sames = intersect prior cur
      new = cur \\ prior
      genDelta (p,c) =
        let changes = issueDelta p c
        in if length changes > 0
           then Just (origin p, number p, changes)
           else Nothing
      zipLookup vals k = (k, fromJust $ lookup k vals)
  in IssueChanges new  $ mapMaybe genDelta $ map (zipLookup curs) sames
