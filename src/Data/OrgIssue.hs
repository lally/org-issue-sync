{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- TODO(lally): trim down these imports.
module Data.OrgIssue where
import Data.OrgMode
import Data.Issue
import Control.Monad
import Control.Exception (handle)
import Data.Char (toUpper, isAlphaNum)
import Data.List
import Data.Maybe (mapMaybe, fromJust, catMaybes)
import Debug.Trace (trace)
import Text.Parsec
import Text.Regex.Posix

-- * Issue <-> Org Mapping

-- https://github.com/freedomjs/freedom-pgp-e2e/issues/6#issuecomment-69795153
-- https://code.google.com/p/webrtc/issues/detail?id=3592
makeIssueUrl :: Issue -> String
makeIssueUrl issue =
  let num = show $ number issue
      org = origin issue
  in if '/' `elem` org
     then "https://www.github.com/" ++ org ++ "/issues/" ++ num
     else "https://code.google.com/p/" ++ org ++ "/issues/detail?id=" ++ num

makeIssueOrgHeading :: Int -> Int -> Issue -> TextLine
makeIssueOrgHeading fstLine dpth issue =
  let depth = if dpth < 1 then 16 else dpth
      prefix = take depth $ repeat '*'
      todo = map toUpper $ show $ status issue
      summ = summary issue
      tgs = if length (tags issue) > 0
            then " :" ++ (intercalate ":" $ tags issue) ++ ":"
            else ""
  in TextLine depth (prefix ++ " " ++ todo ++ " " ++ summ ++ tgs) fstLine

makeIssueOrgDrawer :: Int -> Int -> Issue -> [TextLine]
makeIssueOrgDrawer fstLine depth issue =
  let props = [
        ("ISSUENUM", show $ number issue),
        ("ISSUEORIGIN", origin issue),
        ("ISSUEUSER", user issue),
        ("ISSUETYPE", iType issue)]
  in makeDrawerLines fstLine depth "PROPERTIES" props

-- Dumb v0.1: just splat issues at the end of files.
makeIssueOrgNode :: Int -> Int -> Issue -> String
makeIssueOrgNode fstLine depth issue =
  let indent = take depth $ repeat ' '
      url = indent ++ "- [[" ++ (makeIssueUrl issue) ++ "][Issue Link]]"
      heading = makeIssueOrgHeading fstLine depth issue
      body = makeIssueOrgDrawer (fstLine + 1) depth issue
      node_text = [tlText $ heading] ++ (map tlText body)
  in unlines $ node_text ++ [url]

appendIssues :: FilePath -> [Issue] -> IO ()
appendIssues file issues = do
  let headings = intercalate "\n" $ map (makeIssueOrgNode (-1) 2) issues
  appendFile file headings

issueStatus :: IssueStatus -> String
issueStatus stat = map toUpper $ show stat

statusIssue :: String -> Maybe IssueStatus
statusIssue s =
  case s of
    "ACTIVE" -> Just Active
    "CLOSED" -> Just Closed
    "DONE" -> Just Closed
    "TODO" -> Just Open
    "OPEN" -> Just Open
    _ -> Nothing

-- * Reading and Mutating Issue Nodes

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
      mapStatus (Just (Prefix s)) = case (statusIssue s) of
        Just st -> st
        Nothing -> Open
  in if (hasPropDrawer n && hasOrigin && hasNum && hasUser && hasType)
     then Just $ Issue (valOf "ISSUEORIGIN" draw) (
       read $ valOf "ISSUENUM" draw) (
       valOf "ISSUEUSER" draw) (mapStatus $ nPrefix n) (nTags n) (nTopic n) (
       valOf "ISSUETYPE" draw)
     else Nothing

instance NodeUpdate Issue where
  -- Fill in the Node, and generate a new TextLine for it.
  -- This will get rather complicated later.
  updateNodeLine iss node =
    case getOrgIssue node of
      Just old_iss ->
        if old_iss == iss
        then let old_line = head $ getTextLines node
                 preserved_tags =
                   let allTags = nTags node
                       pres = filter (elem '@') allTags
                   in pres
                 new_iss = iss { tags = (tags iss) ++ preserved_tags }
                 heading = makeIssueOrgHeading (tlLineNum old_line) (
                   nDepth node) new_iss
                 new_node = node { nLine = heading
                                 , nPrefix =
                                   Just $ Prefix $ map toUpper (
                                     issueStatus $ status iss)
                                 , nTags = (tags iss) ++ preserved_tags
                                 , nTopic = summary iss }
             in Just new_node
        else Nothing
      Nothing -> Nothing

-- |Pull special properties from the argument Node, and generate a new
-- TextLine to replace the header line of that Node, representing this
-- (changed) Issue.  Presumably, the Node represents an older revision
-- of this Issue.  Wipe all tags *except# those with an @ in them,
-- which we don't generate ourselves, but must have been put in by a
-- user.
updateNodeIssue :: Issue -> Node -> TextLine
updateNodeIssue iss nd =
  TextLine indent text lineno
  where text = prefix ++ " " ++ todo ++ " " ++ summ ++ (all_tags)
        node_line = head $ getTextLines nd
        lineno = tlLineNum node_line
        indent = tlIndent node_line
        prefix = take indent $ repeat '*'
        todo = case (status iss) of
          Open -> "OPEN"
          Active -> "ACTIVE"
          Closed -> "CLOSED"
        summ = summary iss
        preseved_tags = filter (elem '@') $ nTags nd
        all_tags = if (length (tags iss) > 0)
                   then " :" ++ (intercalate ":" (
                                    (tags iss) ++ preseved_tags)) ++ ":"
                   else ""

getOrgIssues :: String -> [Issue]
getOrgIssues contents =
  let doc = orgFile contents
  in map fst $ ovElements $ generateDocView getOrgIssue doc

-- * Issue Deltas

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
        numChanged = show $ length changed
        secondHeader = "\nAnd " ++ numChanged ++ " issues changed: \n   "
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

