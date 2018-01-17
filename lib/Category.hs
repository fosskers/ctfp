module Category where

import           Algebra
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import           Prelude hiding (id, (.))

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
