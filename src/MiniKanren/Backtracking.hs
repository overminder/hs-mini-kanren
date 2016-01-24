module MiniKanren.Backtracking (
  module MiniKanren.Backtracking,
  module Control.Monad,
  module Control.Applicative,
) where

import Control.Monad
import Control.Applicative
import Control.Arrow

-- StateT s (Logic a)
-- s -> (s, [a])
--
-- Logic a
-- [a]

-- LogicT (State s) a
newtype BT s a = BT { runBT :: s -> [(a, s)] }
  deriving (Functor)

solutions :: BT s s
solutions = BT $ \ s -> [(s, s)]

execBT :: BT s a -> s -> [s]
execBT (BT f) = map snd . f

mapBT :: (s -> s1) -> (s1 -> s) -> BT s a -> BT s1 a
mapBT f g (BT h) = BT $ \ s1 -> map (second f) $ h (g s1)

withBT :: s -> BT s a -> BT s a
withBT s (BT f) = BT $ \ _ -> f s

instance Applicative (BT s) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return $ f a

instance Monad (BT s) where
  return a = BT $ \ s -> [(a, s)]
  BT ma >>= f = BT $ \ s -> do
    (a, s') <- ma s
    runBT (f a) s'

instance Alternative (BT s) where
  (<|>) = mplus
  empty = mzero

instance MonadPlus (BT s) where
  mzero = BT $ const []
  BT m1 `mplus` BT m2 = BT $ \ s -> m1 s ++ m2 s

