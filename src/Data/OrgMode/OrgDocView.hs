module Data.OrgMode.OrgDocView (
  OrgDocZipper(..), OrgDocView(..), generateDocView, getRawElements,
  updateDoc, NodeUpdate(..)
  ) where

import Data.OrgMode.Doc
import Data.OrgMode.Text

import Data.Set (Set(..), member, lookupLE)
import Data.Maybe (mapMaybe, fromJust, catMaybes)

-- | The document is a forest of Nodes, with properties.  The Node
-- Path is the currently-constructing tree of nodes.  The path is
-- sorted by 'nDepth', in descending order, with each element's parent
-- after it in the list.
data OrgDocZipper = OrgDocZipper
                     { ozNodePath :: [Node]
                     , ozNodes :: [Node]
                     , ozProperties :: [OrgFileProperty]
                     } deriving (Eq, Show)

data OrgDocView a = OrgDocView
                    { ovElements :: [(a, Node)]
                    , ovDocument :: OrgDoc
                    }

-- | Generic visitor for updating a Node's values.  Intentionally, we
-- don't allow node deletion, just update.  Preferably, if you want to
-- delete a Node, you should control the parent.  We also have
-- findItemInNode which will construct an 'a' from the Node, which we
-- may then update against a list.
class (Eq a) => NodeUpdate a where
  findItemInNode :: Node -> Maybe a
  updateNodeLine :: a -> Node -> Maybe Node

-- Doesn't assume that xs or ys are individually sorted, which works
-- well for TextLines with no line number mid-stream.
mergeSorted :: (Ord a) => [a] -> [a] -> [a]
mergeSorted [] [] = []
mergeSorted xs [] = xs
mergeSorted [] ys = ys
mergeSorted xl@(x:xs) yl@(y:ys) =
  case compare x y of
    EQ -> x:(mergeSorted xs yl)
    LT -> x:(mergeSorted xs yl)
    GT -> y:(mergeSorted xl ys)

instance TextLineSource OrgDoc where
  getTextLines doc =
    let docLines = concatMap getTextLines $ odNodes doc
        propLines = concatMap getTextLines $ odProperties doc
    in mergeSorted docLines propLines

instance TextLineSource (OrgDocView a)  where
  getTextLines = getTextLines . ovDocument

-- * Constructors
generateDocView :: (NodeUpdate a) => OrgDoc -> OrgDocView a
generateDocView doc =
  let childNode (ChildNode n) = Just n
      childNode _ = Nothing
      childNodes n = mapMaybe childNode $ nChildren n
      scanNode :: (Node -> Maybe a) -> Node -> [(a, Node)]
      scanNode fn n = let hd = fn n
                          entry = maybe [] (\a -> [(a,n)]) hd
                          rest = (concatMap (scanNode fn) $ childNodes n)
                      in (entry++rest)
      scanOrgForest :: (NodeUpdate a) => [Node] -> [(a, Node)]
      scanOrgForest forest =
        concatMap (scanNode findItemInNode) forest

      forest = odNodes doc
      elements = scanOrgForest forest
  in OrgDocView elements doc

getRawElements :: OrgDocView a -> [a]
getRawElements docview =
  map fst $ ovElements docview

-- * Update an OrgMode doc

-- Algorithm: sort the issues by textline line #.  Then, get the
-- textlines of the entire tree, which shall be in ascending order.
-- Replace them as we match the node.

updateElementList :: (NodeUpdate a) => [Node] -> [(a, Node)]
updateElementList nodes =
  let nodeChildScan (ChildNode nd) = nodeScan nd
      nodeChildScan _ = []
      nodeScan nd =
        let children = concatMap nodeChildScan (nChildren nd)
        in case findItemInNode nd of
          Just item -> (item, nd):children
          Nothing -> children
  in concatMap nodeScan nodes

updateDoc :: (Ord a, NodeUpdate a) => Set a -> OrgDocView a -> OrgDocView a
updateDoc new_items doc =
  let nodeUpdater node =
        case findItemInNode node of
          Just item ->
            if member item new_items
            then let new_item = fromJust $ lookupLE item new_items
                 in updateNodeLine new_item node
            else Nothing
          Nothing -> Nothing
      new_nodes =
        map (updateNode nodeUpdater) $ odNodes $ ovDocument doc
      new_doc = (ovDocument doc) { odNodes = new_nodes }
  in doc { ovDocument = new_doc }

