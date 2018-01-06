module Main where

import Algebra
import Category
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
    [ testProperty "Ratio - left inverse"  $ \a -> fmap (\a' -> inverse a' |*| a') (positive $ bump a) == Just unit
    , testProperty "Ratio - right inverse" $ \a -> fmap (\a' -> a' |*| inverse a') (positive $ bump a) == Just unit
    , testProperty "Bool - left inverse"   $ \a -> inverse a |*| a == (unit :: Bool)
    , testProperty "Bool - right inverse"  $ \a -> a |*| inverse a == (unit :: Bool)
    , testProperty "() - left inverse"     $ \a -> inverse a |*| a == (unit :: ())
    , testProperty "() - right inverse"    $ \a -> a |*| inverse a == (unit :: ())
    ]
  ]
  where bump 0 = 1
        bump n = n

comp :: Int -> Assertion
comp n = f n @?= g n
  where f = (+1) . id
        g = id . (+1)
