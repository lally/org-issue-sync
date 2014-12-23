module Sync.Retrieve.OrgMode.OrgMode where

import Text.ParserCombinators.Parsec
import Control.Monad
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

data Prefix = Prefix String deriving (Eq, Show)
data Drawer = Drawer
              { drName :: String
              , drProperties :: [(String, String)]
              } deriving (Eq, Show)

-- |The body of a node has different parts.  We can put tables here,
-- as well as Babel sections, later.
data NodeChild = ChildText String
               | ChildDrawer Drawer
               | ChildNode Node
                 deriving (Eq, Show)

data Node = Node
            { nDepth :: Int
            , nPrefixes :: [Prefix]
            , nTags :: [String]
            , nChildren :: [NodeChild]
            , nTopic :: String
            } deriving (Eq, Show)


data OrgFile = OrgFile { orgTitle :: String,
                         orgProps :: [(String, String)],
                         orgNodes :: [Node] } deriving (Eq, Show)

data OrgFileElement = OrgFileProperty String String
                    | OrgTopLevel Node
                    deriving (Eq, Show)


{-
Parsing orgNode
*** PREFIX String Taglist

So, let's (try prefix) <|> (try taglist <|> not newline) >> newline

Parts of this problem:
 - The sequencing, combind with optionality.
 - The result types
 -}


tagList = char ':' >> word `endBy1` char ':'
          where word = many1 (letter <|> char '-')

orgPrefix = do pfx <- string "TODO" <|> string "DONE"
               return $ [Prefix pfx]

orgSuffix = (do tags <- tagList
                char '\n'
                return tags) <|> (char '\n' >> return [])

orgNodeTail :: GenParser Char st ([String], String)
orgNodeTail = do topic <- manyTill anyChar (try $ lookAhead orgSuffix)
                 tags <- orgSuffix
                 return (tags, topic)

orgNode = do stars <- many1 $ char '*'
             let depth = length stars
             many1 space
             pfx <- optionMaybe orgPrefix
             let prefixes = maybe [] id pfx
             many space
             (tags, topic) <- orgNodeTail
             return $ Node depth prefixes tags [] topic


restOfLine = do text <- many $ satisfy (/= '\n')
                newline
                return text

propName = many1 letter

orgFileElement :: GenParser Char st OrgFileElement
orgFileElement = (do string "#+"
                     name <- propName
                     char ':'
                     value <- restOfLine
                     return $ OrgFileProperty name value)
                 <|>
                 (do node <- orgNode
                     return $ OrgTopLevel node)

orgFile :: GenParser Char st OrgFile
orgFile = do
  elements <- many orgFileElement
  return $ OrgFile "" [] []

orgHead :: String -> Either ParseError Node
orgHead s = parse orgNode "input" s

parseOrgFile :: FilePath -> IO (Either ParseError OrgFile)
parseOrgFile fname = do
  input <- readFile fname
  return $ parse orgFile fname input

