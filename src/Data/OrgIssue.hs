{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- TODO(lally): trim down these imports.
module Data.OrgIssue where
import Data.OrgMode
import Data.OrgMode.Text
import Data.Issue
import Control.Monad
import Control.Exception (handle)
import Data.Char (toUpper, isAlphaNum, isSpace)
import Data.List
import Data.Maybe (mapMaybe, fromJust, catMaybes, isJust)
import Data.Monoid
import Debug.Trace (trace)
import Text.Parsec
import Text.Regex.Posix

-- * Issue <-> Org Mapping

-- https://github.com/freedomjs/freedom-pgp-e2e/issues/6
-- https://code.google.com/p/webrtc/issues/detail?id=3592
makeIssueUrl :: Issue -> String
makeIssueUrl issue =
  let num = show $ number issue
      org = origin issue
  in if '/' `elem` org
     then "https://www.github.com/" ++ org ++ "/issues/" ++ num
     else "https://code.google.com/p/" ++ org ++ "/issues/detail?id=" ++ num

makeIssueOrgHeading :: LineNumber -> Int -> Issue -> TextLine
makeIssueOrgHeading fstLine dpth issue =
  let depth = if dpth < 1 then 16 else dpth
      prefix = take depth $ repeat '*'
      todo = map toUpper $ show $ status issue
      summ = summary issue
      tgs = if length (tags issue) > 0
            then " :" ++ (intercalate ":" $ tags issue) ++ ":"
            else ""
  in TextLine depth (prefix ++ " " ++ todo ++ " " ++ summ ++ tgs) fstLine

makeIssueOrgDrawer :: LineNumber -> Int -> Issue -> [TextLine]
makeIssueOrgDrawer fstLine depth issue =
  let props = [
        ("ISSUENUM", show $ number issue),
        ("ISSUEORIGIN", origin issue),
        ("ISSUEUSER", user issue),
        ("ISSUETYPE", iType issue)]
  in makeDrawerLines fstLine depth "PROPERTIES" props

-- Dumb v0.1: just splat issues at the end of files.
makeIssueOrgNode :: LineNumber -> Int -> Issue -> String
makeIssueOrgNode fstLine depth issue =
  let indent = take depth $ repeat ' '
      heading = makeIssueOrgHeading fstLine depth issue
      drawer = makeIssueOrgDrawer (mappend fstLine (Line 1)) depth issue
      url_text = indent ++ "- [[" ++ (makeIssueUrl issue) ++ "][Issue Link]]"
      url = TextLine depth url_text (mconcat [fstLine, Line 1, Line (length drawer)])
      body = drawer ++ [url] ++ (
        getTextLines $ makeIssueSubNode (depth+1) (
           mconcat [fstLine, Line 2, Line $ length drawer]) issue)
      node_text = [tlText $ heading] ++ (map tlText body)
  in unlines $ node_text

appendIssues :: FilePath -> [Issue] -> IO ()
appendIssues file issues = do
  let headings = intercalate "\n" $ map (makeIssueOrgNode NoLine 2) issues
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
       valOf "ISSUETYPE" draw) []
     else Nothing

makeIssueLine :: Int -> LineNumber -> IssueEvent -> [NodeChild]
makeIssueLine depth line_nr (IssueEvent when user details) =
  let line = TextLine depth "" line_nr
      linePrefix = (take depth $ repeat ' ')
      eventPrefix s = linePrefix ++ "- <" ++
                      (show when) ++ "> " ++
                      user ++ ": " ++ s
      eventLine s = line { tlText = eventPrefix s }
      eventText s = ChildText $ eventLine s
  in case details of
    IssueStatusChange status -> [eventText (show status)]
    IssueComment comment ->
      let all_lines = wrapLine (78 - depth) (eventLine comment)
          prefixedFirstLine = head all_lines
          prefixedRemain =
            map (prefixLine (linePrefix ++ "  ")) $ tail all_lines
      in map ChildText (prefixedFirstLine:prefixedRemain)
    IssueOwnerChange owner -> [eventText $ "New Owner: " ++ owner]
    IssueLabelChange new old ->
      [eventText $ prefix ++ middle ++ suffix]
      where
        prefix = if length new > 0
                 then "New Labels: " ++ (intercalate "," new)
                 else ""
        suffix = if length old > 0
                 then "Old Labels: " ++ (intercalate "," old)
                 else ""
        middle = if length old > 0 && length new > 0
                 then ", "
                 else ""
    IssueMilestoneChange newms oldms ->
      [eventText $ "Milestone " ++ old ++ " -> " ++ new]
      where
        fromMaybe :: String -> Maybe String -> String
        fromMaybe s (Just t) = t
        fromMaybe s Nothing = s
        old = fromMaybe "(no prior milestone)" oldms
        new = fromMaybe "(no new milestone)" newms

makeIssueSubNode :: Int -> LineNumber -> Issue -> NodeChild
makeIssueSubNode depth fst_line iss =
  let prefix = take depth $ repeat '*'
      issueStartingFrom _ [] = []
      issueStartingFrom line_nr (e:es) =
        let cur_child = makeIssueLine depth line_nr e
        in cur_child ++ (issueStartingFrom (mappend line_nr (Line $ length cur_child)) es)
      children = issueStartingFrom (mappend fst_line (Line 1)) $ events iss
  in ChildNode $ Node depth Nothing [] children "ISSUE EVENTS" (
    TextLine 0 (prefix++" ISSUE EVENTS") fst_line)

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
                 isGeneratedChild (ChildNode nd) = nTopic nd == "ISSUE EVENTS"
                 isGeneratedChild _ = False
                 oldChildNode = filter isGeneratedChild $ nChildren node
                 oldChildNodeLineNr =
                   if length oldChildNode > 0
                   then tlLineNum . head . getTextLines . head $ oldChildNode
                   else NoLine
                 regularChildren =
                   filter (not . isGeneratedChild) $ nChildren node
                 childNode =
                   makeIssueSubNode (1 + nDepth node) oldChildNodeLineNr iss
                 new_node = node { nLine = heading
                                 , nPrefix =
                                   Just $ Prefix $ map toUpper (
                                     issueStatus $ status iss)
                                 , nTags = (tags iss) ++ preserved_tags
                                 , nTopic = summary iss
                                 , nChildren = childNode:regularChildren }
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
{-
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

 -}
