{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Algebra where

import Data.Ratio
import Refined hiding (Positive)
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
-- >>> $$(refineTH (1 % maxBound)) :: Positive
-- Refined (1 % 18446744073709551615)
-- >>> $$(refineTH (maxBound % 1)) :: Positive
-- Refined (18446744073709551615 % 1)
--
-- >>> inverse ($$(refineTH (5 % 17)) :: Positive)
-- Refined (17 % 5)
type Positive = Refined (GreaterThan 0) (Ratio Word)

positive :: Ratio Word -> Maybe Positive
positive = either (const Nothing) Just . refine

-- TODO This could be improved. It's hard to use.
instance Group Positive where
  unit    = $$(refineTH 1)
  inverse = unsafeCoerce . recip . unrefine
  l |*| r = unsafeCoerce $ unrefine l * unrefine r

instance Group () where
  unit      = ()
  inverse _ = ()
  _ |*| _   = ()

instance Group Bool where
  unit    = False
  inverse = not
  (|*|)   = (&&)
