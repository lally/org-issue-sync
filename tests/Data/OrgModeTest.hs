module Data.OrgModeTest where

import Data.OrgMode

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

propTextLineIndent :: TextLine -> Bool
propTextLineIndent line =
  length $ takeWhile isSpace $ tlText line == tlIndent line


