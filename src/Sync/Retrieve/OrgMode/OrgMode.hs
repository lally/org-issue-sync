module Sync.Retrieve.OrgMode.OrgMode where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List (intercalate)
-- | I don't think that the parser will do everything I want But I can
-- get pretty close.  Basically, try and parse docs as lists of
-- headings.  Will that work?  Only one way to find out.  Really
-- consider just writing my own.  This one's pretty shit.  Maybe reuse
-- its date parsing?

-- Fuckit, I'm writing a parser.

{-
  Grammar:
nn  OrgFile :: [Property] [Node]
  Property :: '#+' String ':' String
  Node :: '^[*]+ ' [Prefix]* String TagList? (\n Drawer|\n)
  Prefix :: TODO | DONE | A | B | C -- Really? letters?
  TagList :: String ':' TagList .
  Drawer :: Indent ':' String ':'\n [DrawerProperty] \n Indent ':END:'\n
  DrawerProperty :: Indent ':' String ':' [^\n]*\n

  We'll keep a track of indentation as we go.  It will be used to
remove leading whitespace, but that's it.
-}

data Prefix = Prefix String deriving (Eq)
instance Show Prefix where
  show (Prefix s) = s

data Drawer = Drawer
              { drName :: String
              , drProperties :: [(String, String)]
              } deriving (Eq)

instance Show Drawer where
  show (Drawer name props) =
    ":" ++ name ++ ":\n"
    ++ concatMap (\(k,v) -> ":" ++ k ++ ": " ++ v ++ "\n") props
    ++ ":END:\n"

-- |The body of a node has different parts.  We can put tables here,
-- as well as Babel sections, later.
data NodeChild = ChildText String
               | ChildDrawer Drawer
               | ChildNode Node
                 deriving (Eq)

instance Show NodeChild where
  show (ChildText s) = s
  show (ChildDrawer d) = show d
  show (ChildNode n) = show n

data Node = Node
            { nDepth :: Int
            , nPrefixes :: [Prefix]
            , nTags :: [String]
            , nChildren :: [NodeChild]
            , nTopic :: String
            } deriving (Eq)

instance Show Node where
  show (Node depth prefixes tags children topic) =
    stars ++ " " ++ pfx ++ " " ++ topic ++ tgs ++ "\n" ++ rest
    where
      stars = take depth $ repeat '*'
      pfx = concatMap show prefixes
      tgs = if length tags > 0
            then ":" ++ (intercalate ":" tags) ++ ":"
            else ""
      rest = intercalate "\n" $ map show children

data OrgFile = OrgFile { orgTitle :: String,
                         orgProps :: [(String, String)],
                         orgNodes :: [Node] } deriving (Eq, Show)

data OrgFileElement = OrgFileProperty { fpName :: String,
                                        fpValue :: String }
                    | OrgTopLevel { tlNode :: Node }
                    deriving (Eq, Show)


rstrip xs = reverse $ lstrip $ reverse xs
lstrip = dropWhile (== ' ')

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
                   manyTill anyChar (try newline)
                   return $ ChildDrawer $ Drawer drawerName props

-- Any line that isn't a node.
orgBodyLine :: GenParser Char st NodeChild
orgBodyLine = do firstChar <- satisfy (\a -> (a /= '*') && (a /= '#'))
                 rest <- manyTill anyChar newline
                 if firstChar == '\n'
                   then return $ ChildText ""
                   else return $ ChildText $ firstChar : rest

-- (Depth, prefixes, tags, topic)
orgNodeHead :: GenParser Char st (Int, [Prefix], [String], String)
orgNodeHead = do let tagList = char ':' >> word `endBy1` char ':'
                       where word = many1 (letter <|> char '-')

                     orgPrefix = do pfx <- string "TODO" <|> string "DONE" <|>
                                           string "OPEN" <|> string "CLOSED"
                                    return $ [Prefix pfx]

                     orgSuffix = (do tags <- tagList
                                     char '\n'
                                     return tags) <|> (char '\n' >> return [])
                 stars <- many1 $ char '*'
                 let depth = length stars
                 many1 space
                 pfx <- optionMaybe orgPrefix
                 let prefixes = maybe [] id pfx
                 many space
                 topic <- manyTill anyChar (try $ lookAhead orgSuffix)
                 tags <- orgSuffix
                 return (depth, prefixes, tags, topic)

orgNode = do (depth, prefixes, tags, topic) <- orgNodeHead
             body <- many ((try orgPropDrawer)
                           <|> orgBodyLine)
                           <?> "Node Body"
             return $ Node depth prefixes tags body topic

orgProperty = do string "#+"
                 name <- many1 letter
                 char ':'
                 many space
                 value <- manyTill anyChar (try newline)
                 return $ OrgFileProperty name value

orgFileElement :: GenParser Char st OrgFileElement
orgFileElement = orgProperty <|>
                 (do node <- orgNode
                     return $ OrgTopLevel node) <?> "file element"

orgFile :: GenParser Char st OrgFile
orgFile = do
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

orgHead :: String -> Either ParseError Node
orgHead s = parse orgNode "input" s

parseOrgFile :: FilePath -> IO (Either ParseError OrgFile)
parseOrgFile fname = do
  input <- readFile fname
  return $ parse orgFile fname input

