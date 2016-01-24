module MiniKanren.Fresh (
  module MiniKanren.Fresh,
  module Control.Monad.State.Strict
) where

import MiniKanren.Term

import Control.Monad.State.Strict

type GenT = StateT Int
type Gen = State Int

fresh :: (Monad m, MonadState Int m) => m Term
fresh = do
  t <- TVar . VGen <$> get
  modify (+1)
  return t

namedFresh :: (Monad m, MonadState Int m) => m Term
namedFresh name = do
  t <- TVar . Var name <$> get
  modify (+1)
  return t
