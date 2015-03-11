module Data.OrgModeTest where

import Data.OrgMode

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

{--
****** TODO Org doc validity
  - So much is alreday valid due to the 
****** TODO Org Parsing validity
       - Make sure wthat we can parse valid documents
       - A great heavy-weight use of quickcheck!
****** TODO TextLine validity
   - It's the numbers.  What do I care about the numbers for?
   - And why are they with -1s, instead of Maybe Int?
******* TODO Line numbers are consistent
   - Well, Maybe Int is all I need here.
******* TODO Indentation is consistent
   - That's actually good.  I should have a factory function that
     takes care of this.

****** TODO View mutation is valid
--}

propTextLineIndent :: TextLine -> Bool
propTextLineIndent line =
  length $ takeWhile isSpace $ tlText line == tlIndent line

-- | Test TextLineSource OrgDoc.  We should give it nodes that do and
-- don't have line numbers, and correctly assemble them.

linesSemiSorted :: [TextLine] -> Bool
linesSemiSorted [] = True
linesSemiSorted [x] = True
linesSemiSorted (x:y:ys)
  | hasNumber x && hasNumber y = x <= y && linesSemiSorted y:ys
  | otherwise = linesSemiSorted y:ys

relativeLineOrderPreserved :: [TextLine] -> [TextLine] -> Bool
relativeLineOrderPreserved [] _ = True
relativeLineOrderPreserved x [] = False
relativeLineOrderPreserved (o:os) superset =
  let remaining_superset = dropWhile (/= o) superset
  in (length remaining_superset > 0) && relativeLineOrderPreserved os remaining_superset

linesMergeProperly :: [TextLine] -> [TextLine] -> [TextLine] -> Bool
linesMergeProperly firsts seconds merged =
  linesSemiSorted firsts && linesSemiSorted seconds &&
  linesSemiSorted merged && relativeLineOrderPreserved firsts merged &&
  relativeLineOrderPreserved seconds merged



-- | Test OrgDocZipper.  We should be able to take a Node -> Maybe
-- Node, (and verify that its line numbers are all dead!) and
-- re-integrate it into the document correctly.

