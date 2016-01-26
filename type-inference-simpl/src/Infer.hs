module Infer where

import Expr

import Control.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.Functor.Foldable
import Control.Monad.Trans
import Control.Monad.State.Strict

type Gen = (TyId, TyEnv)
type GenT = StateT Gen

emptyGen = (0, M.empty)

freshTyVar :: (Monad m, MonadState Gen m) => m Type
freshTyVar = uses _1 TyVar <* (_1 += 1)

data TypeF a
  = TyConF Name
  | TyAppF a a
  | TyVarF TyId  -- A monomorphic type var
  | TyGenF TyId  -- A polymorphic type var
  deriving (Functor, Eq, Show)

pattern TyCon name = Fix (TyConF name)  
pattern TyApp f a = Fix (TyAppF f a)
pattern TyVar v = Fix (TyVarF v)
pattern TyGen v = Fix (TyGenF v)

newtype ShowType = ShowType { runShowType :: Type }

instance Show ShowType where
  show = go . runShowType
   where
    go (TyCon c) = c
    go (TyGen v) = "g" ++ show v
    go (TyVar v) = "m" ++ show v
    go (TyApp (TyApp (TyCon "->") a) b) = "(" ++ go a ++ " -> " ++ go b ++ ")"
    go (TyApp f a) = go f ++ " " ++ go a

showType = show . ShowType

type Type = Fix TypeF
type TyId = Int
type TyEnv = M.Map TyId Type

data UnifyError
  = TyConMismatch Name Name
  | CannotUnify Type Type
  | OccurCheck TyId Type
  | Unbound Name

instance Show UnifyError where
  show (TyConMismatch c1 c2) = "TyConMismatch " ++ c1 ++ " " ++ c2
  show (CannotUnify t1 t2) = "CannotUnify " ++ show (ShowType t1) ++ " " ++ show (ShowType t2)
  show (OccurCheck v t) = "OccurCheck " ++ show (ShowType (TyVar v)) ++ " " ++ show (ShowType t)
  show (Unbound v) = "Unbound " ++ v

funTyCon = TyCon "->"
funTy a b = funTyCon `TyApp` a `TyApp` b

type UnifyM = Either UnifyError

unify :: Type -> Type -> TyEnv -> UnifyM TyEnv
unify t1 t2 tenv = case (t1, t2) of
  (TyCon c1, TyCon c2) | t1 == t2 -> pure tenv
                       | otherwise -> Left $ TyConMismatch c1 c2
  (TyApp f1 a1, TyApp f2 a2) -> unify f1 f2 tenv >>= unify a1 a2
  (TyVar v1, TyVar v2) | v1 == v2 -> pure tenv
  (TyVar v, t) -> v -+> t
  (t, TyVar v) -> v -+> t
  _ -> Left $ CannotUnify t1 t2
 where
  v -+> t = if v `IS.notMember` tyVars t
    then tryExtend v t tenv
    else Left $ OccurCheck v t

unifyC t1 t2 tenv = unify (c t1) (c t2) tenv
 where
  c = canonize tenv

canonize :: TyEnv -> Type -> Type
canonize env = cata go
 where
  go t@(TyConF _) = Fix t
  go t@(TyVarF v) = maybe (Fix t) (canonize $ M.delete v env) $ M.lookup v env
  go (TyAppF f a) = Fix (TyAppF f a)

canonizeI :: Type -> InferM Type
canonizeI t = uses _2 (`canonize` t)

tyVars :: Type -> IS.IntSet
tyVars = cata go
 where
  go (TyConF _) = IS.empty
  go (TyVarF v) = IS.singleton v
  go (TyAppF f a) = f `IS.union` a

tryExtend :: TyId -> Type -> TyEnv -> UnifyM TyEnv
tryExtend v t tenv = case M.lookup v tenv of
  Nothing -> pure $ extend v t tenv
  Just v' -> unify v' t tenv

extend :: TyId -> Type -> TyEnv -> TyEnv
extend = M.insert

type ExprEnv = M.Map Name Type
type InferM = GenT UnifyM

emptyExprEnv = M.empty

runUnify :: Type -> Type -> InferM ()
runUnify t1 t2 = do
  tenv <- use _2
  tenv' <- lift $ unifyC t1 t2 tenv
  _2 .= tenv'

runInfer :: Expr -> ExprEnv -> UnifyM Type
runInfer term env = (`evalStateT` emptyGen) $ canonizeI =<< infer term env

infer :: Expr -> ExprEnv -> InferM Type
infer term env = case term of
  Var v -> lift $ fromMaybe (Left $ Unbound v) (Right <$> M.lookup v env)
  Lam v e -> do
    vt <- freshTyVar
    ft <- freshTyVar
    let env' = M.insert v vt env
    et <- infer e env'
    runUnify (funTy vt et) ft
    return ft
  App f a -> do
    ft <- infer f env
    at <- infer a env
    et <- freshTyVar
    runUnify (funTy at et) ft
    return et
