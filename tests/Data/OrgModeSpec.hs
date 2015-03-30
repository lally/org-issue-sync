module Data.OrgModeSpec (spec, linesSemiSorted) where

import Data.ArbOrgNode
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.OrgMode
import Data.Char (isSpace, isPrint, chr, toUpper)
import Data.Foldable (foldlM)
import Data.List (sort, intercalate)
import qualified Data.Map as M
import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen (elements)
import Test.QuickCheck.Modifiers (getPositive)


-- This is obviously a crap placeholder.  What we want is a mutation
-- of a tree that results in TextLines.
propTextLineIndent :: TextLine -> Bool
propTextLineIndent line =
  tlIndent line >= 0

data TestNode = TestNode Node deriving (Eq)

instance Show TestNode where
  show (TestNode node) = intercalate "\n" $ map show $ getTextLines node

instance Arbitrary TestNode where
  arbitrary = do
    nd <- arbitrary
    return (TestNode nd)

data FullyNumberedNode = FullyNumberedNode Node deriving (Eq)
instance Show FullyNumberedNode where
  show (FullyNumberedNode node) = intercalate "\n" $ map show $ getTextLines node

instance Arbitrary FullyNumberedNode where
  arbitrary = do
    let lnfunc (Line x) = return (Line (x+1))
    nd <- arbNode (Line 1) 1 lnfunc
    return (FullyNumberedNode nd)

data UnnumberedNode = UnnumberedNode Node deriving (Eq)
instance Show UnnumberedNode where
  show (UnnumberedNode node) = intercalate "\n" $ map show $ getTextLines node

instance Arbitrary UnnumberedNode where
  arbitrary = do
    let lnfunc NoLine = return NoLine
    nd <- arbNode NoLine 1 lnfunc
    return (UnnumberedNode nd)

spec :: Spec
spec = do
  describe "nodes text generation" $ do
    prop "each TextLine is a single line" $ propLinesOfNodeAreSingular
    prop "line numbers are proper" $ propLinesOfNodeSemiSorted
    prop "fully numbered nodes are fully numbered properly" $ propLinesOfNumberedNodeSorted
  describe "indents work right" $ do
    prop "indentation" $ propTextLineIndent
  describe "update works right" $ do
    prop "merge" $ propLinesMergeProperly

-- | Test TextLineSource OrgDoc.  We should give it nodes that do and
-- don't have line numbers, and correctly assemble them.

propLinesOfNodeSemiSorted :: TestNode -> Bool
propLinesOfNodeSemiSorted (TestNode node) =
  linesSemiSorted (getTextLines node) == True

propLinesOfNumberedNodeSorted :: FullyNumberedNode -> Bool
propLinesOfNumberedNodeSorted (FullyNumberedNode node) =
  linesSorted (getTextLines node) == True

propLinesOfNodeAreSingular :: TestNode -> Bool
propLinesOfNodeAreSingular (TestNode node) =
  let hasNoNewLines tl = all (/= '\n') $ tlText tl
  in all hasNoNewLines (getTextLines node) == True

propLinesMergeProperly :: FullyNumberedNode -> UnnumberedNode -> Bool
propLinesMergeProperly (FullyNumberedNode host) (UnnumberedNode guest) =
  let isChildNode (ChildNode nd) = True
      isChildNode _ = False
      childNodes = filter isChildNode $ nChildren host
      numChildNodes = length childNodes
      firstChild =
        let (ChildNode n) = head childNodes
        in n
      subChild :: Node -> Maybe Node
      subChild inn
        | inn == firstChild = Just guest
        | otherwise = Nothing
      merged = updateNode subChild host
  in (numChildNodes < 1) || linesSemiSorted (getTextLines merged)

linesSorted :: [TextLine] -> Bool
linesSorted lines =
  let numbers = map tlLineNum lines
      sorted xs = xs == (sort xs)
  in all isNumber numbers && sorted numbers

lineNumbersSemiSorted :: [LineNumber] -> Bool
lineNumbersSemiSorted [] = True
lineNumbersSemiSorted [x] = True
lineNumbersSemiSorted (x:y:ys)
  | isNumber x && isNumber y = ((toNumber 0 x) <= (toNumber 0 y)) && (lineNumbersSemiSorted (y:ys))
  | otherwise = lineNumbersSemiSorted (y:ys)

linesSemiSorted :: [TextLine] -> Bool
linesSemiSorted = lineNumbersSemiSorted . (map tlLineNum)

-- Whether the elements of the second set contain the elements of the
-- first set, in relative order.
relativeLineOrderPreserved :: [TextLine] -> [TextLine] -> Bool
relativeLineOrderPreserved [] _ = True
relativeLineOrderPreserved x [] = False
relativeLineOrderPreserved (o:os) superset =
  let remaining_superset = dropWhile (/= o) superset
  in (length remaining_superset > 0) && 
     relativeLineOrderPreserved os remaining_superset

linesMergeProperly :: [TextLine] -> [TextLine] -> [TextLine] -> Bool
linesMergeProperly firsts seconds merged =
  linesSemiSorted firsts && linesSemiSorted seconds && 
  linesSemiSorted merged && relativeLineOrderPreserved firsts merged && 
  relativeLineOrderPreserved seconds merged

-- | Test OrgDocZipper.  We should be able to take a Node -> Maybe
-- Node, (and verify that its line numbers are all dead!) and
-- re-integrate it into the document correctly.

-- | Test OrgDocView
data TestNodeUpdate = TestUpdate String Int deriving (Eq, Show)

instance NodeUpdate TestNodeUpdate where
  findItemInNode node =
    let topic = nTopic node
        firstWord = head . words $ topic
        id = length topic
    in if firstWord == "Update"
       then Just (TestUpdate topic id)
       else Nothing
  updateNodeLine (TestUpdate st nr) nd =
    let old_topic = nTopic nd
        topic_len = length old_topic
        newTopic =
          if nr < topic_len
          then take nr old_topic
          else old_topic ++ (take (nr - topic_len) $ repeat '+')
    in if topic_len /= nr
       then Just $ nd { nTopic = newTopic }
       else Nothing



