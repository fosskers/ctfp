{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Algebra where

import Data.Ratio
import GHC.TypeNats
import Numeric.Natural
import Prelude hiding (Monoid)
import Refined
import Test.QuickCheck
import Unsafe.Coerce

---

-- | Types with a binary, associative operator.
class Semigroup a where
  (<>) :: a -> a -> a

-- | Types with a binary associative operator that also have an "identity" element.
class Semigroup a => Monoid a where
  identity :: a

-- | Types for which each element has an inverse, such that:
-- TODO the law here and elsewhere.
class Monoid a => Group a where
  -- | Each element of /a/ must possess an inverse.
  inverse :: a -> a

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

instance Semigroup Fraction where
  Fraction l <> Fraction r = Fraction . unsafeCoerce $ unrefine l * unrefine r

instance Monoid Fraction where
  identity = Fraction $$(refineTH 1)

-- | Positive `Fraction`s under multiplication form a `Group`.
instance Group Fraction where

  -- By virtue of being passed to this function in a way that compiles,
  -- the argument already satisfies the Refined predicate.
  -- By definition `recip` cannot fail on values in that range,
  -- so we don't need to "rerefine" it, taking a runtime penalty.
  -- So, it's safe to use `unsafeCoerce` here.
  inverse = Fraction . unsafeCoerce . recip . unrefine . unfraction

instance Semigroup () where
  _ <> _ = ()

instance Monoid () where
  identity = ()

instance Group () where
  inverse _ = ()

instance Semigroup Bool where
  (<>) = (&&)

instance Monoid Bool where
  identity = False

instance Group Bool where
  inverse = not

instance Semigroup Integer where
  (<>) = (+)

instance Monoid Integer where
  identity = 0

instance Group Integer where
  inverse = negate

-- | Some bounded, circular number type.
newtype Mod m = Mod { unmod :: Refined (LessThan m) Word } deriving (Eq, Show)

-- instance KnownNat m => Group (Mod m) where
--   unit = Mod $$(refineTH (0 :: Word))
