{-# LANGUAGE BangPatterns #-}
module Data.ArbOrgNode where

import Control.Applicative ((<$>))
import Control.Exception (assert)
import Control.Monad (liftM)
import Data.OrgMode
import Data.Char (isSpace, isPrint, chr, toUpper)
import Data.Foldable (foldlM)
import Data.List (sort)
import Debug.Trace (trace)
import qualified Data.Map as M
import System.IO (putStrLn)
import System.Random
import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen (elements)
import Test.QuickCheck.Modifiers (getPositive)

allPrintableChars = filter (\c -> isPrint c && c /= '\n') $ map chr [0..0x7f]
lineWidth = 78

maxDepth = 5
maxChildren = 4

arbIndent :: Gen Int
arbIndent = choose (0, maxDepth)

arbIndentFrom :: Int -> Gen Int
arbIndentFrom depth
  | depth >= maxDepth = return (depth+1)
  | depth < maxDepth = do
    let mx = max (lineWidth - depth) depth
    d <- choose (depth, mx)
    return d

arbitraryTextCore mn depth = do
  let printable = allPrintableChars
      genSafeChar = elements printable
      mxdpth = max (maxDepth - (max depth 0)) 0
  numCharsInLine <- choose (1, mxdpth)
  k <- choose (mn,numCharsInLine)
  chars <- vectorOf k genSafeChar
  return chars

arbitraryTextMin depth = arbitraryTextCore 1 depth
arbitraryText depth = arbitraryTextCore 0 depth

nextLine :: LineNumber -> LineNumber
nextLine = flip mappend (Line 1)

packLines :: Int -> LineNumber -> [String] -> [TextLine]
packLines depth linenr lines =
  let nrs = map (\n -> mappend linenr $ Line n) [0..]
      ln_nrs = zip lines nrs
      mkline (s,ln) = TextLine depth s ln
  in map mkline ln_nrs

arbLine :: Int -> LineNumber -> Gen TextLine
arbLine depth lineno = do
  chars <- arbitraryText depth
  return (TextLine depth chars lineno)

-- |Makes at least 2 characters.
arbIdentifier :: Gen String
arbIdentifier = do
  let id_start_letters = ['A'..'Z'] ++ ['a'..'z'] ++ ['_']
      id_rest_letters = id_start_letters ++ ['0'..'9']
  first_letter <- elements id_start_letters
  restid <- listOf1 (elements id_rest_letters)
  return (first_letter:restid)

arbDrawer :: LineNumber -> Bool -> Gen Drawer
arbDrawer lineno is_prop = do
  -- generate a drawer name (unless is_prop, which indicates PROPERTIES)
  dname <- case is_prop of
    True -> return "PROPERTIES"
    False -> do
             id <- arbIdentifier
             return $ map toUpper id
  -- then generate some key-value pairs.
  let gen_kv :: Gen (String, String)
      gen_kv = do
        key <- arbIdentifier
        value <- listOf (elements allPrintableChars)
        return (key, value)
  raw_key_vals <- listOf gen_kv
  -- unique-ify the keys in our key-val pair list.
  indent <- elements [0..15]
  let props = M.toList $ M.fromList raw_key_vals
      lines = makeDrawerLines lineno indent dname props
  return $ Drawer dname props lines

arbBabel :: LineNumber -> Gen Babel
arbBabel linenr = do
  raw_body_lines <- listOf (arbitraryText 0)
  let is_command_pfx ('#':'+':xs) = True
      is_command_pfx _ = False
      body_lines = filter (\s -> not (is_command_pfx s)) raw_body_lines
      header = "#+begin_src"
      footer = "#+end_src"
      babel_text = header:((take 10 body_lines) ++ [footer])
      lines = packLines 0 linenr babel_text
  return (Babel lines)

arbTable :: LineNumber -> Gen Table
arbTable linenr = do
  indent <- arbIndent
  let arb_table_line = do
        body <- arbitraryText indent
        return ((take indent$repeat ' ' ) ++ "|" ++ body ++ "|")
  first_line <- arb_table_line
  rest <- listOf1 arb_table_line
  let lines = packLines indent linenr (first_line:(take 10 rest))
  return (Table lines)

arbTextLine :: LineNumber -> Gen TextLine
arbTextLine linenr = do
  indent <- arbIndent
  text <- arbitraryText indent
  return (TextLine indent text linenr)

-- lnfunc takes a line and returns something >= to it.  It may return
-- NoLine.
arbNode :: LineNumber -> Int -> (LineNumber -> Gen LineNumber) -> Gen Node
arbNode ln depth lnfunc = do
  -- generate the head, then generate some children.
  tags <- listOf arbIdentifier
  topic <- arbitraryTextMin 0
  prefix <- elements [Nothing, Just (Prefix "OPEN"), Just (Prefix "CLOSED"),
                      Just (Prefix "TODO"), Just (Prefix "ACTIVE"),
                      Just (Prefix "DONE")]
  raw_children_selected <- listOf (elements [1..5]) -- [1..5] for full set.
  -- max 10 chlidren, max depth 'maxDepth'
  let cld_dpth = depth + 1
      children_selected =
        if cld_dpth >= maxDepth
        then take maxChildren $ filter (/= 3) raw_children_selected
        else take maxChildren raw_children_selected
      make_child :: Int -> LineNumber -> Gen NodeChild
      make_child cld linenr = do
        case cld of
          1 -> do line <- arbLine cld_dpth linenr
                  return (ChildText line)
          2 -> do is_prop <- elements [True, False]
                  drawer <- arbDrawer linenr is_prop
                  return (ChildDrawer drawer)
          3 -> do node <- arbNode linenr cld_dpth lnfunc
                  return (ChildNode node)
          4 -> do babel <- arbBabel linenr
                  return (ChildBabel babel)
          5 -> do table <- arbTable linenr
                  return (ChildTable table)
      comp_larger a b = if isNumber a && isNumber b then a > b else True
      step_child (linenr, xs) cld = do
        child <- make_child cld linenr
        -- children can arbitrarily step forward their line numbers,
        -- so scan for the largest that they've done so.
        let mxline :: Int
            mxline = maximum $ map (\l -> toNumber 0 (tlLineNum l)) $ getTextLines child
            nxln = if mxline == 0 then NoLine else Line mxline
        nxtChildLn <- lnfunc nxln
        return (nxtChildLn, xs ++ [child])
  linenr <- lnfunc ln
  (_, children) <- foldlM step_child (linenr, []) children_selected
  let mkText nd = TextLine depth (makeNodeLine nd) ln
      nd = Node depth prefix tags children topic (mkText nd)
  return nd

instance Arbitrary Node where
  arbitrary = do
    let nl_thresh = 25
        zero_thresh = 50
        lnfunc (NoLine) = return NoLine
        lnfunc (Line x) = do
          offset <- choose (1, 100)
          if offset < nl_thresh
            then return NoLine
            else if offset < zero_thresh
                 then return $ Line (x+1)
                 else return $ Line (x + (offset - zero_thresh))
    arbNode (Line 1) 1 lnfunc

instance Arbitrary TextLine where
  arbitrary = do
    linenr <- arbitrary
    line <- elements [NoLine, Line linenr]
    text <- arbTextLine line
    return text
