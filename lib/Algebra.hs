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

-- >>> unit :: Ratio Int
-- 1 % 1
-- >>> inverse 5 :: Ratio Int
-- 1 % 5
-- >>> (5 |*| inverse 5) :: Ratio Int
-- 1 % 1
instance Integral a => Group (Ratio a) where
  unit = 1
  inverse = recip
  (|*|) = (*)
