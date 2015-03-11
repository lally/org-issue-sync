{-# LANGUAGE BangPatterns #-}
module Data.OrgMode (
  Prefix(..), Drawer(..), Babel(..), Table(..), NodeChild(..), Node(..),
  OrgFile(..), OrgFileProperty(..), OrgFileElement(..),
  OrgDocView(..), NodeUpdate(..), OrgDoc(..),
  orgFile, generateDocView, getRawElements, updateNode
  ) where
-- TODO(lally): only export the interesting things!

import Data.OrgMode.Text
import Data.OrgMode.Doc
import Data.OrgMode.OrgDocView
-- import Sync.Issue

import Control.Monad
import Data.Char (toUpper, isAlphaNum)
import Data.List
import Data.Maybe (mapMaybe, fromJust, catMaybes, isJust, isNothing)
import Data.Monoid
import Debug.Trace (trace)
import Text.Parsec
import Text.Regex.Posix
import Text.StringTemplate

-- ** Zipper Facilities

-- | Closes up the path for the zipper, up to the specified depth.
appendChildrenUpPathToDepth :: Int -> [Node] -> [Node]
appendChildrenUpPathToDepth _ [] = []
appendChildrenUpPathToDepth depth [n] = [n { nChildren = reverse $ nChildren n }]
appendChildrenUpPathToDepth depth (n:ns)
  | nDepth n > depth =
    let parent = head ns
        parentChildren = nChildren parent
        fixedUpChild = n { nChildren = reverse $ nChildren n }
        updatedParent = parent { nChildren = (ChildNode n):parentChildren }
    in updatedParent : (tail ns)
  | otherwise = (n:ns)

-- | Closes up the path for the zipper, reversing the child-lists as we
-- go (to get them into first->last order).
closeZipPath doczip@(OrgDocZipper path nodes properties) =
  doczip { ozNodePath = [],
           ozNodes = nodes ++ (appendChildrenUpPathToDepth (-1) path) }

addOrgLine :: OrgDocZipper -> OrgLine -> OrgDocZipper
-- First, the simple base case.  Creates a pseudo-root for unattached
-- lines, and inserts the first node into the path.  There can only be
-- one pseudo-root, for lines before the first Node.
addOrgLine doczip@(OrgDocZipper []  nodes properties) orgline =
  let pseudo_root = Node (-1) Nothing [] [] "" emptyTextLine
      pseudRootWithChild c = doczip { ozNodePath = [pseudo_root { nChildren = [c] }] }
  in case orgline of
    (OrgText line) -> pseudRootWithChild $ ChildText line
    (OrgHeader line node) -> doczip { ozNodePath = [node] }
    (OrgDrawer line) -> pseudRootWithChild $ ChildDrawer $ Drawer "" [] [line]
    (OrgPragma line prop) -> doczip { ozProperties = (ozProperties doczip) ++ [prop] }
    (OrgBabel line) -> pseudRootWithChild $ ChildBabel $ Babel [line]
    (OrgTable line) ->  pseudRootWithChild $ ChildTable $ Table [line]

addOrgLine doczip@(OrgDocZipper path@(pn:pns) nodes props) orgline =
  let -- addDrawer should parse as it goes.  But, we have the problem of :END:
      -- followed with more properties.  We can detect this, expensively, by
      -- scanning for :END: in the last line of the existing drawer.
      -- Correctness over speed!
      isEndLine line = ":END:" == (trim $ tlText line)
      openDrawer (Just (ChildDrawer (Drawer _ _ []))) = True
      openDrawer (Just (ChildDrawer (Drawer _ _ lines))) =
        not $ isEndLine $ last lines
      openDrawer _ = False
      parseDrawerName tline =
        let matches = (tlText tline) =~ " *:([A-Za-z]*): *" :: [[String]]
        in if length matches > 0
           then matches !! 0 !! 1
           else ""
      parseDrawerProperty tline =
        let matches = (tlText tline) =~ " *:([-_A-Za-z]*):(.*)" :: [[String]]
        in if length matches > 0
           then [(matches !! 0 !! 1, trim $ matches !! 0 !! 2)]
           else []

      addChildToNode n c = n { nChildren = c:(nChildren n) }
      addChildToLastNode c = doczip { ozNodePath = (addChildToNode pn c):pns }
      updateLastChildOfLastNode c =
        let updatedPn = pn { nChildren = c:(tail $ nChildren pn) }
        in doczip { ozNodePath = updatedPn:pns }
      lastChild = let children = nChildren pn
                  in if null children then Nothing else Just $ last children
      addBabel line = case lastChild of
        Just (ChildBabel (Babel lines)) ->
          updateLastChildOfLastNode $ ChildBabel $ Babel $ lines ++ [line]
        Nothing -> addChildToLastNode (ChildBabel $ Babel [line])

      addTable line = case lastChild of
        Just (ChildTable (Table lines)) ->
          updateLastChildOfLastNode $ ChildTable $ Table $ lines ++ [line]
        Nothing -> addChildToLastNode (ChildTable $ Table [line])

      addDrawer line
        | not (openDrawer lastChild) =
          let drawer = Drawer (parseDrawerName line) [] [line]
          in addChildToLastNode (ChildDrawer drawer)
          -- Parse to get drProperties, and ignore :END:.
        | otherwise =
          let Just (ChildDrawer (Drawer n p lines)) = lastChild
              props = parseDrawerProperty line
              dlines = lines ++ [line]
              update n p =
                updateLastChildOfLastNode $ ChildDrawer $ Drawer n p dlines
          in if isEndLine line
             then update n p
             else update n (p ++ props)

      addNode line node
        | nDepth node > nDepth pn = doczip { ozNodePath = node:path }
        | nDepth node <= nDepth (last path) =
          let closed = closeZipPath doczip
          in closed { ozNodePath = [node] }
        | otherwise =
            let newpath = node:(appendChildrenUpPathToDepth (nDepth node) path)
            in doczip { ozNodePath = newpath}
  in case orgline of
    (OrgText line) -> addChildToLastNode $ ChildText line
    (OrgHeader line node) -> addNode line node
    (OrgDrawer line) -> addDrawer line
    (OrgPragma line prop) ->
      doczip { ozProperties = (ozProperties doczip) ++ [prop] }
    (OrgBabel line) -> addBabel line
    (OrgTable line) -> addTable line

-- | Intentionally fail when we don't have a parse success, which
-- shouldn't happen...
allRight :: Either a b -> b
allRight (Right b) = b

-- * Primary File Parser

-- | Parsing the file efficiently.  Let's keep it non-quadratic.
--   - Split it up into lines
--   - Identify each line, as part of one of the big structure types:
--      - node headers
--      - drawers
--      - file-level properties
--         - babel headers
--      - Lines who's type depend on context (e.g., babel entries or node
--        text)
--   - Then fold the lines over a builder function and a zipper of the
--     tree.
orgFile :: String -> OrgDoc
orgFile fileContents =
  let fileLines = lines fileContents
      categorizedLines =
        map (\(nr, line) -> allRight $ parseLine nr line) $ zip [1..] fileLines
      -- ^ keep the structure reversed, so that the last element's at
      emptyzip = OrgDocZipper [] [] []
      (OrgDocZipper path nodes props) =
        foldl addOrgLine emptyzip categorizedLines
      all_nodes = nodes ++ appendChildrenUpPathToDepth (-1) path
  in OrgDoc all_nodes props

rstrip xs = reverse $ lstrip $ reverse xs
lstrip = dropWhile (== ' ')
strip xs = lstrip $ rstrip xs

orgPropDrawer :: Parsec String st NodeChild
orgPropDrawer = do manyTill space (char ':') <?> "Property Drawer"
                   drawerName <- many1 letter
                   char ':'
                   manyTill space newline
                   let orgProperty = do
                         manyTill space (char ':')
                         propName <- many1 letter
                         char ':'
                         value <- manyTill (satisfy (/= '\n')) (try newline)
                         return (propName, rstrip $ lstrip value)
                   props <- manyTill orgProperty (
                     try $ manyTill space (string ":END:"))
                   manyTill space newline
                   return $ ChildDrawer $ Drawer drawerName props []

emptyTextLine = TextLine 0 "" NoLine

-- Any line that isn't a node.
orgBodyLine :: Parsec String st NodeChild
orgBodyLine = do firstChar <- satisfy (\a -> (a /= '*') && (a /= '#'))
                 if firstChar /= '\n'
                   then do rest <- manyTill anyChar newline
                           let allText = (firstChar : rest)
                               indent = length $ takeWhile (== ' ') allText
                           return $ ChildText $ TextLine indent allText NoLine
                   else return $ ChildText emptyTextLine

orgProperty :: Parsec String st OrgFileElement
orgProperty = do string "#+"
                 name <- many1 letter
                 char ':'
                 many space
                 value <- manyTill anyChar (try newline)
                 return $ OrgTopProperty $ OrgFileProperty name value

babelLine :: Parsec String TextLine OrgLine
babelLine = do
  (string "#+begin_src:") <|> (string "#+end_src") <|>
    (string "#+BEGIN_SRC:") <|> (string "#+END_SRC")
  textLine <- getState
  return $ OrgBabel textLine

fileProperty :: Parsec String TextLine OrgLine
fileProperty = do
  string "#+"
  name <- many1 letter
  char ':'
  many space
  value <- manyTill anyChar (try newline)
  line <- getState
  return $ OrgPragma line $ OrgFileProperty name value

nodeLine :: Parsec String TextLine OrgLine
nodeLine = do
  let tagList = char ':' >> word `endBy1` char ':'
        where word = many1 (letter <|> char '-' <|> digit <|> char '_' <|> char '@')
      validPrefixes = ["TODO", "DONE", "OPEN", "CLOSED", "ACTIVE"]
      orgSuffix = (do tags <- tagList
                      char '\n'
                      return tags) <|> (char '\n' >> return [])
  stars <- many1 $ char '*'
  let depth = length stars
  many1 space
  -- stop this sillyness on the prefix. just pull the first word of the topic.
  -- TODO(lally): don't hard-code the list of prefixes.
  many space
  topic <- manyTill anyChar (try $ lookAhead orgSuffix)
  let topic_words = words topic
      first_word_is_prefix =
        length topic_words > 0 && (head topic_words `elem` validPrefixes)
      prefix = if first_word_is_prefix
                 then Just $ Prefix $ head topic_words
                 else Nothing
      topic_remain = if first_word_is_prefix
                     then snd $ splitAt (length $ head topic_words) topic
                     else topic
  tags <- orgSuffix
  loc <- getState
  let line = OrgHeader loc $ Node depth prefix tags [] (strip topic_remain) loc
  return line

propertyLine :: Parsec String TextLine OrgLine
propertyLine = do
  manyTill space (char ':')
  propName <- many1 (letter <|> char '-' <|> digit <|> char '_' <|> char '@')
  char ':'
  remain <- manyTill (satisfy (/= '\n')) (try newline)
  line <- getState
  return $ OrgDrawer line

bodyLine :: Parsec String TextLine OrgLine
bodyLine = do
  text <- getState
  return $ OrgText text

-- |The incoming state has TextLine within it.
-- TODO(lally): update the state here to hold options we get in the
-- ORG file, like TODO states.
classifyOrgLine :: Parsec String TextLine OrgLine
classifyOrgLine = do
  textLine <- getState
  -- Possibilities:
  --   - #+begin_src:
  --   - #+other:
  --   - ** blah
  --   - :PROPERTY:
  --   - anything else.
  res <- (try babelLine)
          <|> (try fileProperty)
          <|> (try nodeLine)
          <|> (try propertyLine)
          <|> bodyLine -- always matches.
  return res

parseLine :: Int -> String -> Either ParseError OrgLine
parseLine lineno s = do
  let indent = length $ takeWhile (== ' ') s
      line = TextLine indent s (Line lineno)
    in runParser classifyOrgLine line "input" (s ++ "\n")

