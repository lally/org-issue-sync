module Sync.Retrieve.OrgMode.OrgMode where
import Sync.Issue.Issue

import Text.Parsec
import Control.Monad
import Data.List
import Data.Maybe (mapMaybe, fromJust)
import Debug.Trace (trace)
-- | I don't think that the parser will do everything I want But I can
-- get pretty close.  Basically, try and parse docs as lists of
-- headings.  Will that work?  Only one way to find out.  Really
-- consider just writing my own.  This one's pretty shit.  Maybe reuse
-- its date parsing?

-- Fuckit, I'm writing a parser.

{-
  Grammar:
  OrgFile :: [Property] [Node]
  Property :: '#+' String ':' String
  Node :: '^[*]+ ' [Prefix]* String TagList? (\n Drawer|\n)
  Prefix :: TODO | DONE | A | B | C -- Really? letters?
  TagList :: String ':' TagList .
  Drawer :: Indent ':' String ':'\n [DrawerProperty] \n Indent ':END:'\n
  DrawerProperty :: Indent ':' String ':' [^\n]*\n

  We'll keep a track of indentation as we go.  It will be used to
remove leading whitespace, but that's it.
-}
-- | Raw data about each line of text.
data TextLine = TextLine
                { tlIndent :: Int  -- how long of a whitespace prefix is in tlText?
                , tlText :: String
                , tlLineNum :: Int
                } deriving (Eq)

instance Show TextLine where
  show tl = (show $tlLineNum tl) ++ ":" ++ (tlText tl)

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
-- multi-line parsing to understand
data OrgLine = OrgText TextLine
             | OrgHeader TextLine Node
             | OrgDrawer TextLine
             | OrgPragma TextLine OrgFileProperty
             | OrgBabel TextLine
             | OrgTable TextLine
             deriving (Eq, Show)

-- ^ Backwards!
data OrgElement = OrgElNode Node
                | OrgElPragma OrgFileProperty
                deriving (Eq, Show)

data OrgDoc = OrgDoc
              { odLines :: [OrgLine]
              , odNodes :: [Node]
              , odProperties :: [OrgFileProperty]
              } deriving (Eq, Show)

data OrgEnvironmentBuilder = OETable Table
                           | OEBabel Babel
                             deriving (Eq, Show)

data OrgDocBuilder = ODBuilder
                     { odbNodePath :: [(Int, Node)]
                     , odEnvironment :: Maybe OrgEnvironmentBuilder
                     } deriving (Eq, Show)

{-
  Parsing the file efficiently.  Let's keep it non-quadratic.
  - Split it up into lines
  - Identify each line, as part of one of the big structure types:
     - node headers
     - drawers
     - file-level properties
        - babel headers
     - Lines who's type depend on context (e.g., babel entries or node
       text)
  - Then fold the lines over a builder function and a zipper of the
    tree.
-}

orgFile file = do
  fileContents <- readFile file
  let fileLines = lines fileContents
      parseLines s = s
      revRawStructure = reverse $ map parseLines fileLines
      -- ^ keep the structure reversed, so that the last element's at
      -- the head of the list.
--      finalStructure =
  return ()

rstrip xs = reverse $ lstrip $ reverse xs
lstrip = dropWhile (== ' ')
strip xs = lstrip $ rstrip xs


{-
Parsing orgNode
*** PREFIX String Taglist

So, let's (try prefix) <|> (try taglist <|> not newline) >> newline

Parts of this problem:
 - The sequencing, combind with optionality.
 - The result types
 -}

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

-- Any line that isn't a node.
orgBodyLine :: Parsec String st NodeChild
orgBodyLine = do firstChar <- satisfy (\a -> (a /= '*') && (a /= '#'))
                 if firstChar /= '\n'
                   then do rest <- manyTill anyChar newline
                           let allText = (firstChar : rest)
                               indent = length $ takeWhile (== ' ') allText
                           return $ ChildText $ TextLine indent allText 0
                   else return $ ChildText (TextLine 0 "" 0)

-- (Depth, prefixes, tags, topic)
orgNodeHead :: Parsec String st (Int, Maybe Prefix, [String], String)
orgNodeHead = do let tagList = char ':' >> word `endBy1` char ':'
                       where word = many1 (letter <|> char '-' <|> digit <|> char '_')

{-                     orgPrefix = do pfx <- string "TODO" <|> string "DONE" <|>
                                           string "OPEN" <|> string "CLOSED" <|>
                                           string "ACTIVE"
                                    return $ [Prefix pfx] -}
                     validPrefixes = ["TODO", "DONE", "OPEN", "CLOSED", "ACTIVE"]
                     orgSuffix = (do tags <- tagList
                                     char '\n'
                                     return tags) <|> (char '\n' >> return [])
                 stars <- many1 $ char '*'
                 let depth = length stars
                 many1 space
                 -- stop this sillyness on the prefix. just pull the first word of the topic.
                 -- TODO(lally): don't hard-code the list of prefixes.
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
                 return (depth, prefix, tags, strip topic_remain)

orgNode = do (depth, prefixes, tags, topic) <- orgNodeHead
             body <- many ((try orgPropDrawer)
                           <|> orgBodyLine
                           <|> (do {newline; return $ ChildText (TextLine 0 "" 0)})
                           <?> "Node Body")
             return $ Node depth prefixes tags body topic

orgProperty :: Parsec String st OrgFileElement
orgProperty = do string "#+"
                 name <- many1 letter
                 char ':'
                 many space
                 value <- manyTill anyChar (try newline)
                 return $ OrgTopProperty $ OrgFileProperty name value

--orgFileElement :: Parsec String st OrgFileElement
-- orgFileElement = do orgProperty <|> (do node <- orgNode
--                                        return $ OrgTopLevel node) <?> "file element"
{-
orgFile :: Parsec String st OrgFile
orgFile = do
  many orgBodyLine
  elements <- many orgFileElement
  let titles = filter titleProp elements
      title = if length titles > 0
                 then fpValue $ last titles
                 else ""
      props = filter (\x -> (not $ titleProp x) && (isProp x)) elements
      nodes = filter nodeProp elements
      isProp (OrgFileProperty _ _) = True
      isProp (OrgTopLevel _) = False
      titleProp (OrgFileProperty "TITLE" _) = True
      titleProp _ = False
      nodeProp (OrgTopLevel _) = True
      nodeProp _ = False
  return $ OrgFile title (zip (map fpName props) (map fpValue props)) (
    map tlNode nodes)
-}
--orgHead :: String -> Either ParseError Node
--orgHead s = parse orgNode "input" s

--parseOrgFile :: FilePath -> String -> IO (Either ParseError OrgFile)
--parseOrgFile fname input = do
--  return $ parse orgFile fname input

babelLine :: Parsec String TextLine OrgLine
babelLine = do
  (string "#+begin_src:") <|> (string "#+end_src")
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
{-      orgPrefix = do pfx <- string "TODO" <|> string "DONE" <|>
                            string "OPEN" <|> string "CLOSED" <|>
                            string "ACTIVE"
                     return $ [Prefix pfx] -}
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

-- | Intentionally fail when we don't have a parse success, which
-- shouldn't happen...
allRight :: Either a b -> b
allRight (Right b) = b

getOrgIssue :: Node -> Maybe Issue
getOrgIssue n =
  let draw = propDrawer n
      hasOrigin = hasKey "ISSUEORIGIN" draw
      hasNum = hasKey "ISSUENUM" draw
      hasUser = hasKey "ISSUEUSER" draw
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
  in if (hasPropDrawer n && hasOrigin && hasNum && hasUser)
     then Just $ Issue (valOf "ISSUEORIGIN" draw) (read $ valOf "ISSUENUM" draw) (
       valOf "ISSUEUSER" draw) (mapStatus $ nPrefix n) (nTags n) (nTopic n)
     else Nothing

getOrgIssues :: FilePath -> String -> IO [Issue]
getOrgIssues = undefined
{- getOrgIssues fname text = do
  res <- parseOrgFile fname text
  case res of
    Left e -> do putStrLn $ show e
                 return []
    Right orgFile -> return $ mapMaybe getOrgIssue $ orgNodes orgFile 
-}
data IssueChanges = IssueChanges
                    { newIssues :: [Issue]
                    , changes :: [(String, Int, [IssueDelta])]
                    } deriving (Eq, Show)

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
 
