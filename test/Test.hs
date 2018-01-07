module Main where

import Algebra
import Category
import Prelude hiding (id, (.))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Category"
    [ testCase "Composition respects identity" (comp 1)
    ]
  , testGroup "Group"
    [ testProperty "Ratio - left inverse"  (leftinv  :: Fraction -> Bool)
    , testProperty "Ratio - right inverse" (rightinv :: Fraction -> Bool)
    , testProperty "Bool - left inverse"   (leftinv  :: Bool -> Bool)
    , testProperty "Bool - right inverse"  (rightinv :: Bool -> Bool)
    , testProperty "() - left inverse"     (leftinv  :: () -> Bool)
    , testProperty "() - right inverse"    (rightinv :: () -> Bool)
    ]
  ]

leftinv :: (Arbitrary a, Group a, Eq a) => a -> Bool
leftinv a = inverse a |*| a == unit

rightinv :: (Arbitrary a, Group a, Eq a) => a -> Bool
rightinv a = a |*| inverse a == unit

comp :: Int -> Assertion
comp n = f n @?= g n
  where f = (+1) . id
        g = id . (+1)
