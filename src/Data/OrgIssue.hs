{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- TODO(lally): trim down these imports.
module Data.OrgIssue where
import Data.OrgMode
import Data.OrgMode.Text
import Data.Either (isLeft)
import Data.Issue
import Control.Monad
import Control.Exception (handle, assert)
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

-- Dumb v0.1: just splat issues at the end of files.
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
    TextLine depth (prefix++" ISSUE EVENTS") fst_line)

isGeneratedChild (ChildNode nd) = nTopic nd == "ISSUE EVENTS"
isGeneratedChild _ = False

findSafeChildInsertion node =
  let wouldBecomeChild (ChildNode nd) = nDepth nd > (1 + nDepth node)
      wouldBecomeChild _ = True
  in length $ filter wouldBecomeChild $ nChildren node

origChildIndex :: [NodeChild] -> Maybe Int
origChildIndex children =
  let indices = findIndices isGeneratedChild children
  in if length indices > 0
     then Just $ head indices
     else Nothing

finalChildIndex node children =
  let origIdx = origChildIndex children
      safePoint = findSafeChildInsertion node
  in maybe safePoint (\o -> max o safePoint) origIdx

replaceAtIndex :: a -> Int -> [a] -> [a]
replaceAtIndex c n [] = [c]
replaceAtIndex c n lst@(x:xs)
  | n > 0 = x:(replaceAtIndex c (n-1) xs)
  | n == 0 = c:xs
  | otherwise = lst ++ [c]

updateOrgIssueNodeLine iss node =
  -- Put ISSUE EVENTS after any children that would otherwise fall
  -- underneath this new node.  That is, any nodes of depth (parent
  -- +2) or more, or any non-node children.
  -- Put the child at the greater of:
  -- - The first place where it wouldn't eat other children in a re-parse
  -- - The old index.
  let -- puts the new ISSUE EVENTS child |chld| in place of the old
      -- one, or at the end if we didn't find one.
      updateChild chld children =
        replaceAtIndex chld (finalChildIndex node children) $
        filter (not . isGeneratedChild) children
      preservedTags tags = filter (elem '@') tags
      statusPrefix = Just $ Prefix $ map toUpper (issueStatus $ status iss)
  in case getOrgIssue node of
    Just old_iss ->
      if old_iss == iss
      then let new_iss = iss { tags = (tags iss) ++ (preservedTags $ nTags node) }
               heading = makeIssueOrgHeading NoLine (nDepth node) new_iss
               childNode = makeIssueSubNode (1 + nDepth node) NoLine iss
               new_node = node { nLine = heading
                               , nPrefix = statusPrefix
                               , nTags = tags new_iss
                               , nTopic = summary iss
                               , nChildren = updateChild childNode (nChildren node) }
           in Just new_node
      else Nothing
    Nothing -> Nothing

instance NodeUpdate Issue where
  findItemInNode = getOrgIssue
  -- Fill in the Node, and generate new TextLines for it.
  updateNodeLine = updateOrgIssueNodeLine

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
  in map fst $ ovElements $ generateDocView doc
