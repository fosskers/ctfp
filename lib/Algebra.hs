module Algebra where

import Data.Ratio

---

class Group a where
  -- | An identity element.
  unit :: a

  -- | Each element of `a` must possess an inverse.
  inverse :: a -> a

  -- | An associative combination of elements of `a`.
  --
  -- LAW: a |*| inverse a == unit
  --      inverse a |*| a == unit
  (|*|) :: a -> a -> a

-- TODO Bad and unlawful!
-- >>> inverse 0 :: Ratio Int
-- *** Exception: Ratio has zero denominator
instance Integral a => Group (Ratio a) where
  unit    = 1
  inverse = recip
  (|*|)   = (*)

instance Group Bool where
  unit    = False
  inverse = not
  (|*|)   = (&&)
