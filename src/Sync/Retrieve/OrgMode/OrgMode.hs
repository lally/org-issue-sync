module Sync.Retrieve.OrgMode.OrgMode where
import Sync.Issue.Issue

import Text.ParserCombinators.Parsec
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
    "PP<<:" ++ name ++ ":\n"
    ++ concatMap (\(k,v) -> ":" ++ k ++ ": " ++ v ++ "\n") props
    ++ ":END:>>PP\n"

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
    stars ++ " <" ++ pfx ++ "> " ++ topic ++ "<<" ++ tgs ++ ">>\n" ++ rest
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
                   return $ ChildDrawer $ Drawer drawerName props

-- Any line that isn't a node.
orgBodyLine :: GenParser Char st NodeChild
orgBodyLine = do firstChar <- satisfy (\a -> (a /= '*') && (a /= '#'))
                 if firstChar /= '\n'
                   then do rest <- manyTill anyChar newline
                           return $ ChildText $ firstChar : rest
                   else return $ ChildText ""

-- (Depth, prefixes, tags, topic)
orgNodeHead :: GenParser Char st (Int, [Prefix], [String], String)
orgNodeHead = do let tagList = char ':' >> word `endBy1` char ':'
                       where word = many1 (letter <|> char '-' <|> digit <|> char '_')

                     orgPrefix = do pfx <- string "TODO" <|> string "DONE" <|>
                                           string "OPEN" <|> string "CLOSED" <|>
                                           string "ACTIVE"
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
                 return (depth, prefixes, tags, strip topic)

orgNode = do (depth, prefixes, tags, topic) <- orgNodeHead
             body <- many ((try orgPropDrawer)
                           <|> orgBodyLine
                           <|> (do {newline; return $ ChildText ""})
                           <?> "Node Body")
             return $ Node depth prefixes tags body topic

orgProperty = do string "#+"
                 name <- many1 letter
                 char ':'
                 many space
                 value <- manyTill anyChar (try newline)
                 return $ OrgFileProperty name value

orgFileElement :: GenParser Char st OrgFileElement
orgFileElement = do orgProperty <|> (do node <- orgNode
                                        return $ OrgTopLevel node) <?> "file element"

orgFile :: GenParser Char st OrgFile
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

orgHead :: String -> Either ParseError Node
orgHead s = parse orgNode "input" s

parseOrgFile :: FilePath -> String -> IO (Either ParseError OrgFile)
parseOrgFile fname input = do
  return $ parse orgFile fname input

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
      mapStatus [] = Open
      mapStatus ((Prefix s):ps) = case s of
        "ACTIVE" -> Active
        "CLOSED" -> Closed
        "DONE" -> Closed
        "TODO" -> Open
        "OPEN" -> Open
        _ -> Open
  in if (hasPropDrawer n && hasOrigin && hasNum && hasUser)
     then Just $ Issue (valOf "ISSUEORIGIN" draw) (read $ valOf "ISSUENUM" draw) (
       valOf "ISSUEUSER" draw) (mapStatus $ nPrefixes n) (nTags n) (nTopic n)
     else Nothing

getOrgIssues :: FilePath -> String -> IO [Issue]
getOrgIssues fname text = do
  res <- parseOrgFile fname text
  case res of
    Left e -> do putStrLn $ show e
                 return []
    Right orgFile -> return $ mapMaybe getOrgIssue $ orgNodes orgFile 

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
