module MiniKanren.Backtracking where

import Control.Monad
import Control.Applicative

newtype BT s a = BT { runBT :: [s] -> [(a, [s])] }
  deriving (Functor)

instance Applicative (BT s) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return $ f a

instance Monad (BT s) where
  return a = BT $ \ ss -> [(a, ss)]
  BT ma >>= f = BT $ \ ss -> do
    (a, ss') <- ma ss
    runBT (f a) ss'

instance Alternative (BT s) where
  (<|>) = mplus
  empty = mzero

instance MonadPlus (BT s) where
  mzero = BT $ const []
  BT m1 `mplus` BT m2 = BT $ \ ss -> m1 ss ++ m2 ss

