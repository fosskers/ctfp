{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Algebra where

import qualified Control.Monad.Trans.State.Lazy as LS
import qualified Control.Monad.Trans.State.Strict as SS
import           Data.Functor.Identity (Identity)
import           Data.Ratio
import           Numeric.Natural
import           Prelude hiding (Monoid, id, (.))
import           Refined
import           Test.QuickCheck
import           Unsafe.Coerce

---

class Category t where
  -- | An identity morphism.
  id :: t a a

  -- | Composition of morphisms.
  (.) :: t b c -> t a b -> t a c

instance Category (->) where
  id a = a

  (.) f g a = f (g a)

class Category t => Kleisli t where
  (>=>) :: Monad m => t a (m b) -> t b (m c) -> t a (m c)

-- Is using Monad like this cheating? Or is Kleisli so fundamental that there
-- couldn't possibly be a complicated implementation.
--
-- Should Monad be defined in terms of /this/ instead? Still unclear.
instance Kleisli (->) where
  (f >=> g) a = f a >>= g

class Product f where
  first  :: f a b -> a
  second :: f a b -> b

-- | The tuple is the "best" product for the category of types and functions,
-- since there is always a way (`(,)`) to map from some other candidate
-- construction to tuple.
instance Product (,) where
  first  (a, _) = a
  second (_, b) = b

-- instance Product (LS.State s Identity a) where
--   first  = LS.execState
--   second = LS.evalState

class Coproduct f where
  left  :: a -> f a b
  right :: b -> f a b

-- | The disjoint union (`Either`) is the "best" coproduct for the category
-- of types and functions, since there is always a way (`either`) to map from
-- `Either` to the other candidate construction.
instance Coproduct Either where
  left  = Left
  right = Right

-- | Types with a binary, associative operator.
class Semigroup a where
  (<>) :: a -> a -> a

-- Boring?
instance Semigroup (a -> a) where
  (<>) = (.)

instance Semigroup Fraction where
  Fraction l <> Fraction r = Fraction . unsafeCoerce $ unrefine l * unrefine r

instance Semigroup () where
  _ <> _ = ()

instance Semigroup Bool where
  (<>) = (&&)

instance Semigroup Integer where
  (<>) = (+)

-- | Types with a binary associative operator that also have an "identity" element.
class Semigroup a => Monoid a where
  identity :: a

-- Boring?
instance Monoid (a -> a) where
  identity = id

instance Monoid Fraction where
  identity = Fraction $$(refineTH 1)

instance Monoid Integer where
  identity = 0

instance Monoid () where
  identity = ()

instance Monoid Bool where
  identity = False

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

-- | Types for which each element has an inverse, such that:
-- TODO the law here and elsewhere.
class Monoid a => Group a where
  -- | Each element of /a/ must possess an inverse.
  inverse :: a -> a

-- | Positive `Fraction`s under multiplication form a `Group`.
instance Group Fraction where

  -- By virtue of being passed to this function in a way that compiles,
  -- the argument already satisfies the Refined predicate.
  -- By definition `recip` cannot fail on values in that range,
  -- so we don't need to "rerefine" it, taking a runtime penalty.
  -- So, it's safe to use `unsafeCoerce` here.
  inverse = Fraction . unsafeCoerce . recip . unrefine . unfraction

instance Group () where
  inverse _ = ()

instance Group Bool where
  inverse = not

instance Group Integer where
  inverse = negate

-- | Some bounded, circular number type.
newtype Mod m = Mod { unmod :: Refined (LessThan m) Word } deriving (Eq, Show)

-- instance KnownNat m => Group (Mod m) where
--   unit = Mod $$(refineTH (0 :: Word))
