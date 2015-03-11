module Data.OrgMode.Doc where
import Data.List (intercalate)

--
-- * Data Decls
--

import Data.OrgMode.Text

data Prefix = Prefix String deriving (Eq)

data Drawer = Drawer
              { drName :: String
              , drProperties :: [(String, String)]
              , drLines :: [TextLine]
              } deriving (Eq)

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

data Node = Node
            { nDepth :: Int
            , nPrefix :: Maybe Prefix
            , nTags :: [String]
            , nChildren :: [NodeChild]
              -- ^ In reverse order during construction.
            , nTopic :: String
            , nLine :: TextLine
            } deriving (Eq)

data OrgFileProperty = OrgFileProperty { fpName :: String
                                       , fpValue :: String
                                       } deriving (Eq, Show)
data OrgFile = OrgFile { orgTitle :: String
                       , orgProps :: [(String, String)]
                       , orgNodes :: [Node]
                       } deriving (Eq, Show)

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

-- ^ Backwards!
data OrgElement = OrgElNode Node
                | OrgElPragma OrgFileProperty
                deriving (Eq, Show)

data OrgDoc = OrgDoc
              { odNodes :: [Node]
              , odProperties :: [OrgFileProperty]
              } deriving (Eq, Show)
--
-- * Instance Decls
--
instance Show Prefix where
  show (Prefix s) = s
instance Show Drawer where
  show (Drawer name props _) =
    ":" ++ name ++ ":\n"
    ++ concatMap (\(k,v) -> ":" ++ k ++ ": " ++ v ++ "\n") props
    ++ ":END:\n"

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

instance Show Node where
  show (Node depth prefix tags children topic _) =
    stars ++ " " ++ pfx ++ " " ++ topic ++ " " ++ tgs ++ "\n" ++ rest
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

instance TextLineSource OrgFileProperty where
  getTextLines prop =
    [TextLine 0 ("#+" ++ (fpName prop) ++ ": " ++ (fpValue prop)) NoLine]

instance TextLineSource OrgLine where
  getTextLines (OrgText t) = [t]
  getTextLines (OrgHeader t _) = [t]
  getTextLines (OrgDrawer t) = [t]
  getTextLines (OrgPragma t _) = [t]
  getTextLines (OrgBabel t) = [t]
  getTextLines (OrgTable t) = [t]

-- ** Utilities
trim xs =
  let rstrip xs = reverse $ lstrip $ reverse xs
      lstrip = dropWhile (== ' ')
  in lstrip $ rstrip xs

