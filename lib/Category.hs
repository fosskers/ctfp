module Category where

import           Algebra
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import           Prelude hiding ((.))

---

-- id :: a -> a
-- id a = a

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) f g a = f (g a)

memoize :: Ord a => (a -> b) -> a -> State (M.Map a b) b
memoize f a = get >>= maybe g pure . M.lookup a
  where g = modify (M.insert a b) >> pure b
        b = f a  -- It's lazy :)

-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: [Word]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- >>> map badFibs [0..9]
-- [0,1,1,2,3,5,8,13,21,34]
badFibs :: Word -> Word
badFibs 0 = 0
badFibs 1 = 1
badFibs n = badFibs (n-1) + badFibs (n-2)

go :: IO ()
go = do
  let f = memoize badFibs
      (b, m) = runState (f 40) mempty
  print b
  print $ evalState (f 40) m

------------
-- CHAPTER 4
------------

safeRoot :: Double -> Maybe Double
safeRoot n | n >= 0 = Just $ sqrt n
           | otherwise = Nothing

safeRecip :: Double -> Maybe Double
safeRecip 0 = Nothing
safeRecip n = Just $ 1 / n

-- >>> import Algebra
-- >>> let f = safeRecip >=> safeRoot
-- >>> f 4
-- Just 0.5
-- >>> f 0
-- Nothing

------------
-- CHAPTER 8
------------

data Pair a b = Pair a b

instance Bifunctor Pair where
  bimap f g (Pair a b) = Pair (f a) (g b)
  fstmap f  (Pair a b) = Pair (f a) b
  sndmap g  (Pair a b) = Pair a (g b)

data PreList a b = Nil | Cons a b

instance Bifunctor PreList where
  bimap _ _ Nil = Nil
  bimap f g (Cons a b) = Cons (f a) (g b)

data K2 c a b = K2 c

instance Bifunctor (K2 c) where
  bimap _ _ (K2 c) = K2 c

data Fst a b = Fst a

instance Bifunctor Fst where
  bimap f _ (Fst a) = Fst $ f a

data Snd a b = Snd b

instance Bifunctor Snd where
  bimap _ g (Snd b) = Snd $ g b
