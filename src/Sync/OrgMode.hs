module Sync.OrgMode where
-- TODO(lally): only export the interesting things!

import Sync.Issue

import Control.Monad
import Data.Char (toUpper, isAlphaNum)
import Data.List
import Data.Maybe (mapMaybe, fromJust, catMaybes)
import Debug.Trace (trace)
import Text.Parsec
import Text.Regex.Posix
import Text.StringTemplate

-- | Raw data about each line of text.  Currently a bit hacked, with
-- 'tlLineNum == 0' indicating a fake line.
data TextLine = TextLine
                { tlIndent :: Int  -- how long of a whitespace prefix is in tlText?
                , tlText :: String
                , tlLineNum :: Int
                } deriving (Eq)

instance Show TextLine where
  show tl = (show $tlLineNum tl) ++ ":" ++ (tlText tl)

-- | Currently a simple getter.  TODO(lally): extend with enough here
-- to let us write out a modified .org file, preserving as much of the
-- original input document structure as we can.
class TextLineSource s where
  getTextLines :: s -> [TextLine]

data Prefix = Prefix String deriving (Eq)
instance Show Prefix where
  show (Prefix s) = s

data Drawer = Drawer
              { drName :: String
              , drProperties :: [(String, String)]
              , drLines :: [TextLine]
              } deriving (Eq)

instance Show Drawer where
  show (Drawer name props _) =
    "PP<<:" ++ name ++ ":\n"
    ++ concatMap (\(k,v) -> ":" ++ k ++ ": " ++ v ++ "\n") props
    ++ ":END:>>PP\n"

-- |Just store the lines of the babel environment.
data Babel = Babel [TextLine] deriving (Eq, Show)

data Table = Table [TextLine] deriving (Eq, Show)

-- |The body of a node has different parts.  We can put tables here,
-- as well as Babel sections, later.
data NodeChild = ChildText TextLine
               | ChildDrawer Drawer
               | ChildNode Node
               | ChildBabel Babel
               | ChildTable Table
                 deriving (Eq)

instance Show NodeChild where
  show (ChildText s) = show s
  show (ChildDrawer d) = show d
  show (ChildNode n) = show n
  show (ChildBabel (Babel b)) = intercalate "\n" $ map show b
  show (ChildTable (Table t)) = intercalate "\n" $ map show t

instance TextLineSource NodeChild where
  getTextLines (ChildText l) = [l]
  getTextLines (ChildDrawer d) = drLines d
  getTextLines (ChildNode n) = getTextLines n
  getTextLines (ChildBabel (Babel lines)) = lines
  getTextLines (ChildTable (Table lines)) = lines

data Node = Node
            { nDepth :: Int
            , nPrefix :: Maybe Prefix
            , nTags :: [String]
            , nChildren :: [NodeChild]
              -- ^ In reverse order during construction.
            , nTopic :: String
            , nLine :: TextLine
            } deriving (Eq)

instance Show Node where
  show (Node depth prefix tags children topic _) =
    stars ++ " <" ++ pfx ++ "> " ++ topic ++ "<<" ++ tgs ++ ">>\n" ++ rest
    where
      stars = take depth $ repeat '*'
      pfx = show prefix
      tgs = if length tags > 0
            then ":" ++ (intercalate ":" tags) ++ ":"
            else ""
      rest = intercalate "\n" $ map show children

instance TextLineSource Node where
  getTextLines node =
    (nLine node) : (concatMap getTextLines $ nChildren node)

data OrgFile = OrgFile { orgTitle :: String,
                         orgProps :: [(String, String)],
                         orgNodes :: [Node] } deriving (Eq, Show)
data OrgFileProperty = OrgFileProperty { fpName :: String,
                                        fpValue :: String } deriving (Eq, Show)

data OrgFileElement = OrgTopProperty OrgFileProperty
                    | OrgTopLevel { tlNode :: Node }
                    deriving (Eq, Show)

-- | We have one of these per input line of the file.  Some of these
-- we just keep as the input text, in the TextLine (as they need
-- multi-line parsing to understand).
data OrgLine = OrgText TextLine
             | OrgHeader TextLine Node
             | OrgDrawer TextLine
             | OrgPragma TextLine OrgFileProperty
             | OrgBabel TextLine
             | OrgTable TextLine
             deriving (Eq, Show)

instance TextLineSource OrgLine where
  getTextLines (OrgText t) = [t]
  getTextLines (OrgHeader t _) = [t]
  getTextLines (OrgDrawer t) = [t]
  getTextLines (OrgPragma t _) = [t]
  getTextLines (OrgBabel t) = [t]
  getTextLines (OrgTable t) = [t]

-- ^ Backwards!
data OrgElement = OrgElNode Node
                | OrgElPragma OrgFileProperty
                deriving (Eq, Show)

data OrgDoc = OrgDoc
              { odLines :: [OrgLine]
                -- ^ Deprecated
              , odNodes :: [Node]
              , odProperties :: [OrgFileProperty]
              } deriving (Eq, Show)

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
                    } deriving (Show)

generateDocView :: (Node -> Maybe a) -> OrgDoc -> OrgDocView a
generateDocView classifier doc =
  let childNode (ChildNode n) = Just n
      childNode _ = Nothing
      childNodes n = mapMaybe childNode $ nChildren n
      scanNode :: (Node -> Maybe a) -> Node -> [(a, Node)]
      scanNode fn n = let hd = fn n
                          entry = maybe [] (\a -> [(a,n)]) hd
                          rest = (concatMap (scanNode fn) $ childNodes n)
                      in (entry++rest)
      scanOrgForest :: (Node -> Maybe a) -> [Node] -> [(a, Node)]
      scanOrgForest fn forest =
        concatMap (scanNode fn) forest

      forest = odNodes doc
      elements = scanOrgForest classifier forest
  in OrgDocView elements doc

getRawElements :: OrgDocView a -> [a]
getRawElements docview =
  map fst $ ovElements docview

trim xs =
  let rstrip xs = reverse $ lstrip $ reverse xs
      lstrip = dropWhile (== ' ')
  in lstrip $ rstrip xs

-- | Closes up the path for the zipper, up to the specified depth.
appendChildrenUpPathToDepth :: Int -> [Node] -> [Node]
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
  let -- TODO(lally): addDrawer should parse as it goes.  But, we have the
      -- problem of :END: followed with more properties.  We can detect this,
      -- expensively, by scanning for :END: in the last line of the existing
      -- drawer.  Correctness over speed!
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
        let matches = (tlText tline) =~ " *:([A-Za-z_-]*):(.*)" :: [[String]]
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
  in OrgDoc categorizedLines all_nodes props

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

emptyTextLine = TextLine 0 "" 0

-- Any line that isn't a node.
orgBodyLine :: Parsec String st NodeChild
orgBodyLine = do firstChar <- satisfy (\a -> (a /= '*') && (a /= '#'))
                 if firstChar /= '\n'
                   then do rest <- manyTill anyChar newline
                           let allText = (firstChar : rest)
                               indent = length $ takeWhile (== ' ') allText
                           return $ ChildText $ TextLine indent allText 0
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
  (string "#+begin_src:") <|> (string "#+end_src") <|>  (string "#+BEGIN_SRC:") <|> (string "#+END_SRC")
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
        where word = many1 (letter <|> char '-' <|> digit <|> char '_')
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
      first_word_is_prefix = length topic_words > 0 && (head topic_words `elem` validPrefixes)
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
  propName <- many1 letter
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
      line = TextLine indent s lineno
    in runParser classifyOrgLine line "input" (s ++ "\n")

-- Algorithm: sort the issues by textline line #.  Then, get the
-- textlines of the entire tree, which shall be in ascending order.
-- Replace them as we match the node.

class (Eq a) => NodeUpdate a where
  updateNodeLine :: a -> Node -> TextLine

updateView :: (NodeUpdate a) => [a] -> OrgDocView a -> [TextLine]
updateView elems view =
  let sorter (a, anode) (b, bnode) =
        let left_line = tlLineNum $ head $ getTextLines anode
            right_line = tlLineNum $ head $ getTextLines bnode
        in compare left_line right_line
--      sorted_orig_elems :: (NodeUpdate a) => [(a, Node)]
      sorted_orig_elems = sortBy sorter $ ovElements view
--      sorted_just_orig_elems :: (NodeUpdate a) => [a]
      sorted_just_orig_elems = map fst sorted_orig_elems
      matchSorter a b =
        let (Just left) = elemIndex a sorted_just_orig_elems
            (Just right) = elemIndex b sorted_just_orig_elems
        in compare left right
--      sorted_matching_elems :: (NodeUpdate a) => [a]
      sorted_matching_elems = sortBy matchSorter $ filter (
        flip elem sorted_just_orig_elems) elems
      -- the second list should be a proper superset of the first.
      zipSndOfEquivList :: (NodeUpdate a) => [a] -> [(a, Node)] -> [(a, Node)]
      zipSndOfEquivList [] all_b = []
      zipSndOfEquivList (a:as) all_b =
        let ((bfst, bsnd):btail) = dropWhile (\(bf, bs) -> bf /= a) all_b
        in (a,bsnd):(zipSndOfEquivList as btail)
      merged_new_elems = zipSndOfEquivList sorted_matching_elems sorted_orig_elems
      -- | Only swap out the first line of the node.  The rest are
      -- passed through verbatim.
      swapTextLines :: (NodeUpdate a) => [(a, Node)] -> [TextLine] -> [TextLine]
      swapTextLines (n:ns) lines =
        let nNewLine = updateNodeLine (fst n) (snd n)
            nLineStart = tlLineNum $ head $ getTextLines $ snd n
            prefix = takeWhile (\line -> tlLineNum line < nLineStart) lines
        in prefix ++ [nNewLine] ++ (swapTextLines ns $ tail lines)
  in swapTextLines merged_new_elems $ concatMap getTextLines $ odLines (
    ovDocument view)

 
