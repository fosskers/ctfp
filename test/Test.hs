module Main where

import Algebra
import Category
import Data.Ratio
import Prelude hiding (id, (.))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Category"
    [ testCase "Composition respects identity" (comp 1)
    ]
  , testGroup "Group"
    [ testProperty "Ratio - left inverse"  $ \a -> inverse a |*| a == (unit :: Ratio Int)
    , testProperty "Ratio - right inverse" $ \a -> a |*| inverse a == (unit :: Ratio Int)
    , testProperty "Bool - left inverse"  $ \a -> inverse a |*| a == (unit :: Bool)
    , testProperty "Bool - right inverse" $ \a -> a |*| inverse a == (unit :: Bool)
    ]
  ]

comp :: Int -> Assertion
comp n = f n @?= g n
  where f = (+1) . id
        g = id . (+1)
