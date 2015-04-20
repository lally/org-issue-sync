module Sync.Retrieve.GoogleCode.Parse (parseIssueText) where

import Data.OrgMode.Text
import Text.HTML.TagSoup
import Text.StringLike
import Debug.Trace
import Data.Maybe
import Data.Issue
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.Locale


data DepthTaggedTag str = DepthTaggedTag Int (Tag str) deriving (Eq)

instance (Show str) => Show (DepthTaggedTag str) where
  show (DepthTaggedTag depth tag) =
    (take depth $ repeat ' ') ++ show tag

-- | Problem: the tag stream can be bad.  We're seeing unclosed <br>
-- tags.  Unsurprising, but we must repair it.  So, if we maintain a
-- stack: [div div div b] and see a /div, we should pop the stack
-- backwards until we see the matching tag.
matchDepth :: [Tag String] -> [Tag String] -> ([DepthTaggedTag String], [Tag String])
matchDepth [] remain = ([], remain)
matchDepth stack [] = ([], [])
matchDepth stack (t:ts) =
  let depth = length stack
      indent = (take depth $ repeat ' ')
      matchesTag s (TagOpen n _) = s == n
      doesntMatchTag s (TagOpen n _) = s /= n
  in case t of
    TagOpen n _ ->
      let (tl, rem) = matchDepth (t:stack) ts
      in ((DepthTaggedTag depth t):tl, rem)
    TagClose n ->
      let new_stack =
            let fixed = dropWhile (doesntMatchTag n) stack
            in if length fixed > 0
               then tail fixed
               else []
          (tl, rem) = matchDepth new_stack ts
      in ((DepthTaggedTag (length new_stack) t):tl, rem)
    otherwise ->
      let (tl, rem) = matchDepth stack ts
      in ((DepthTaggedTag depth t):tl, rem)

-- | Return the open/close pair of tags (and include all the ones in
-- between) that match the pattern.
matchExternalTag :: (TagRep t) => t -> [Tag String] -> ([Tag String], [Tag String])
matchExternalTag pattern input =
  let startOfStream = dropWhile (~/= pattern) input
      (found, rem) = matchDepth [head startOfStream] (tail startOfStream)
      stripDepth (DepthTaggedTag _ tag) = tag
  in if length startOfStream > 0
     then ((head startOfStream):(map stripDepth found), rem)
     else ([], [])

extractHistoryTags :: [Tag String] -> [Tag String]
extractHistoryTags tags =
  fst $ matchExternalTag "<td class=\"vt issuedescription\"" tags

takeMatchingChildren pat [] = []
takeMatchingChildren pat tgs =
  let (chld, dropped) = matchExternalTag pat tgs
  in chld:(takeMatchingChildren pat dropped)

takeDivChildren tgs = takeMatchingChildren "<div" tgs

-- | Takes output from extractHistoryTags and separates them out into individual groups of tags
separateChildrenDivs tags =
  filter (\l -> length l > 0) $ takeDivChildren tags

trim xs =
  let rstrip xs = reverse $ lstrip $ reverse xs
      lstrip = dropWhile isWhiteSpace
      isWhiteSpace c
        | c == ' ' = True
        | c == '\r' = True
        | c == '\n' = True
        | otherwise = False
  in lstrip $ rstrip xs


parseIssueDescription tags =
  let user = innerText $ take 3 $ dropWhile (~/= "<a class=userlink") $ tags
      desc = innerText $ take 3 $ dropWhile (~/= "<pre") $ tags
  in (user, trim desc)

scanStatusChange :: UTCTime -> String -> String -> IssueEvent
scanStatusChange when user value =
  let newStat = case trim value of
        "Started" -> Active
        "Fixed" -> Closed
        otherwise -> Open
      statChange = IssueStatusChange newStat
  in IssueEvent when user statChange

scanOwnerChange :: UTCTime -> String -> String -> IssueEvent
scanOwnerChange when user value =
  let statChange = IssueOwnerChange $ trim value
  in IssueEvent when user statChange

scanLabelChange :: UTCTime -> String -> String -> IssueEvent
scanLabelChange when user value =
  let unsub :: String -> Bool
      unsub s = head s == '-'
      newLabels = filter (\x -> not . unsub $ x) $ words value
      oldLabels = map tail $ filter unsub $ words value
      labelChange = IssueLabelChange newLabels oldLabels
  in IssueEvent when user labelChange

scanUpdateBox :: UTCTime -> String -> [Tag String] -> [Maybe IssueEvent]
scanUpdateBox when user [] = []
scanUpdateBox when user tags =
  let stat_update_text = "Status:"
      owner_update_text = "Owner:"
      label_update_text = "Labels:"
      (h, t) = matchExternalTag "<b" tags
      key = trim $ innerText h
      value = trim $ innerText $ takeWhile (~/= "<br") t
      rest = dropWhile (~/= "<br") t
      recognized_update =
        if key == stat_update_text
        then Just $ scanStatusChange when user value -- ("STAT_UPDATE", value)
        else if key == owner_update_text
             then Just $ scanOwnerChange when user value
             else if key == label_update_text
                  then Just $ scanLabelChange when user value
                  else Nothing
  in recognized_update:(scanUpdateBox when user rest)

-- | Classify the update and put out any relevant IssueUpdates from it.
parseIssueUpdate tags =
  let no_comment = "(No comment was entered for this change.)"
      date_text = trim . innerText . fst . matchExternalTag "<span class=date" $ tags
      parsed_date = parseTime defaultTimeLocale "%b %e, %Y" date_text
      when = maybe (UTCTime (ModifiedJulianDay 0) 0) id parsed_date
      user = innerText $ take 3 $ dropWhile (~/= "<a class=userlink") $ tags
      comment =
        normalizeInputText $ trim $ innerText $ take 4 $ dropWhile (~/= "<pre") tags
      has_no_comment = no_comment == comment
      comment_result = if has_no_comment
                       then Nothing
                       else Just $ IssueEvent when user $ IssueComment comment
      update_box = fst $ matchExternalTag "<div class=box-inner" tags
      updates = scanUpdateBox when user update_box
  in catMaybes (comment_result:(updates))

parseIssueText :: String -> [IssueEvent]
parseIssueText text =
  let hist = extractHistoryTags $ parseTags text
      cl = separateChildrenDivs hist
  in concatMap parseIssueUpdate cl
