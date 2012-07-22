{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import System.IO.Unsafe
import qualified Data.Vector as V
import qualified Data.Text.Lazy as LT

import Data.Foldable as F
import Data.Monoid
import Data.AList

main = $(defaultMainGenerator)

case_fromToList = (F.toList . fromList) [1,2,3] @?= [1,2,3]
case_take4undefined = (take 4 . F.toList . fromList) [0,1,2,3,undefined] @?= [0,1,2,3]
case_take4undefined2 = (length . take 3 . F.toList) lst @?= 3
    where
        lst = (singleton 0) `mappend` (singleton 1) `mappend` (singleton 2) `mappend` undefined
