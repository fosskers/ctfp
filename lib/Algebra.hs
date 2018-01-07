{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Algebra where

import Data.Ratio
import Numeric.Natural
import Refined
import Test.QuickCheck
import Unsafe.Coerce

---

class Group a where
  -- | An identity element.
  unit :: a

  -- | Each element of /a/ must possess an inverse.
  inverse :: a -> a

  -- | An associative combination of elements of /a/.
  --
  -- LAW:
  --
  -- @
  -- a |*| inverse a == unit
  -- inverse a |*| a == unit
  -- @
  (|*|) :: a -> a -> a

-- | Non-zero, positive fractional values.
--
-- >>> :set -XTemplateHaskell
-- >>> inverse ($$(refineTH (5 % 17)) :: Fraction)
-- Refined (17 % 5)
newtype Fraction = Fraction { unfraction :: Refined (GreaterThan 0) (Ratio Natural) } deriving (Eq, Show)

instance Arbitrary Fraction where
  arbitrary = (\n d -> Fraction . unsafeCoerce $ f n % f d) <$> (arbitrary :: Gen Natural) <*> (arbitrary :: Gen Natural)
    where f 0 = 1
          f n = n

instance Arbitrary Natural where
  arbitrary = fromIntegral . abs <$> (arbitrary :: Gen Integer)

-- TODO This could be improved. It's hard to use.
instance Group Fraction where
  unit = Fraction $$(refineTH 1)

  -- By virtue of being passed to this function in a way that compiles,
  -- the argument already satisfies the Refined predicate.
  -- By definition `recip` cannot fail on values in that range,
  -- so we don't need to "rerefine" it, taking a runtime penalty.
  -- So, it's safe to use `unsafeCoerce` here.
  inverse = Fraction . unsafeCoerce . recip . unrefine . unfraction

  Fraction l |*| Fraction r = Fraction . unsafeCoerce $ unrefine l * unrefine r

instance Group () where
  unit      = ()
  inverse _ = ()
  _ |*| _   = ()

instance Group Bool where
  unit    = False
  inverse = not
  (|*|)   = (&&)
