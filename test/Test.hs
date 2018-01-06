module Main where

import Algebra
import Category
import Data.Ratio
import Prelude hiding (id, (.))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Category"
    [ testCase "Composition respects identity" (comp 1)
    ]
  , testGroup "Group"
    [ testCase "Ratio - left inverse"  $ inverse (5 :: Ratio Int) |*| 5 @?= unit
    , testCase "Ratio - right inverse" $ (5 :: Ratio Int) |*| inverse 5 @?= unit
    ]
  ]

comp :: Int -> Assertion
comp n = f n @?= g n
  where f = (+1) . id
        g = id . (+1)
