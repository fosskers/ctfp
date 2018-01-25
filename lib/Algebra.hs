{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Algebra where

import Data.Ratio
import Numeric.Natural
import Prelude hiding (Monoid, id, (.))
import Refined
import Test.QuickCheck
import Unsafe.Coerce

---

-- | Objects and morphisms. Since we're in Haskell, it's assumed that the
-- objects are always Types.
class Category t where
  -- | An identity morphism.
  id :: t a a

  -- | Composition of morphisms.
  (.) :: t b c -> t a b -> t a c

instance Category (->) where
  id a = a

  (.) f g a = f (g a)

-- class Category t => Kleisli t where
--   (>=>) :: Monad m => t a (m b) -> t b (m c) -> t a (m c)

-- Is using Monad like this cheating? Or is Kleisli so fundamental that there
-- couldn't possibly be a complicated implementation.
--
-- Should Monad be defined in terms of /this/ instead? Still unclear.
-- instance Kleisli (->) where
--   (f >=> g) a = f a >>= g

newtype Kleisli a b = Kleisli { kleisli :: forall m. Monad m => a -> m b }

instance Category Kleisli where
  id = Kleisli return

  (.) (Kleisli f) (Kleisli g) = Kleisli (\a -> g a >>= f)

class Product f where
  first  :: f a b -> a
  second :: f a b -> b

-- | The tuple is the "best" product for the category of types and functions,
-- since there is always a way (`(,)`) to map from some other candidate
-- construction to tuple.
instance Product (,) where
  first  (a, _) = a
  second (_, b) = b

class Coproduct f where
  left  :: a -> f a b
  right :: b -> f a b

-- | The disjoint union (`Either`) is the "best" coproduct for the category
-- of types and functions, since there is always a way (`either`) to map from
-- `Either` to the other candidate construction.
instance Coproduct Either where
  left  = Left
  right = Right

-- newtype IdState s a = IdState { idstate :: LS.StateT s Identity a } deriving (Functor, Applicative)

-- | Not possible, since we can't specify any constraints (esp. Monoid) on `b` here.
-- instance Coproduct IdState where
--   left s = IdState $ LS.state (\_ -> (mempty, s))
--   right  = pure

-- | What is this actually in Category Theory?
-- This /kinda/ follows from Product (using `first` and `second`), but it's
-- also possible for `Either` using /its/ factorizer (`either`).
class Swappable f where
  swap :: f a b -> f b a

instance Swappable (,) where
  swap (a, b) = (b, a)

instance Swappable Either where
  swap = either Right Left

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

-- type Op r a = a -> r

-- | Impossible!
-- instance Functor (Op r) where
--   fmap f (Op g) = Op (\b -> undefined)

-- instance Contravariant ((->) r) where
--   contramap f g = g . f

class Bifunctor f where
  bimap  :: (a -> b) -> (c -> d) -> f a c -> f b d
  bimap f g = sndmap g . fstmap f

  fstmap :: (a -> b) -> f a c -> f b c
  fstmap f x = bimap f id x

  sndmap :: (c -> d) -> f a c -> f a d
  sndmap g x = bimap id g x

class Profunctor f where
  dimap :: (a' -> a) -> (b -> b') -> f a b -> f a' b'

-- | Uhh... what's the point?
instance Profunctor (->) where
  dimap f g h = g . h . f

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
