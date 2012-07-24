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
import qualified Data.AList as AL

main = $(defaultMainGenerator)

case_fromToList = (AL.toList . AL.fromList) [1,2,3] @?= [1,2,3]
case_take4undefined = (take 4 . F.toList . AL.fromList) [0,1,2,3,undefined] @?= [0,1,2,3]
case_take4undefined2 = (length . take 3 . F.toList) lst @?= 3
    where
        lst = (AL.singleton 0) `mappend` (AL.singleton 1) `mappend` (AL.singleton 2) `mappend` undefined

case_head = (AL.head . AL.fromList) [2,4] @?= 2
case_tail = (AL.tail . AL.fromList) [2,4] @?= 4
