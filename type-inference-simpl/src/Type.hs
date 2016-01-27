module Type where

import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.Functor.Foldable hiding (Foldable)

type TyName = String

data TypeF a
  = TyConF !TyName
  | TyAppF a a
  | TyVarF !Bool !TyId  -- Bool: is polymorphic
  deriving (Functor, Eq, Show, Traversable, Foldable)

pattern TyCon name = Fix (TyConF name)  
pattern TyApp f a = Fix (TyAppF f a)
pattern TyVar b v = Fix (TyVarF b v)
pattern PolyTyVar v = TyVar True v
pattern MonoTyVar v = TyVar False v

unTyVar (TyVar _ v) = v

newtype ShowType = ShowType { runShowType :: Type }

instance Show ShowType where
  show = go . runShowType
   where
    go (TyCon c) = c
    go (TyVar poly v) = (if poly then "p" else "m") ++ show v
    go (TyApp (TyApp (TyCon "->") a) b) = "(" ++ go a ++ " -> " ++ go b ++ ")"
    go (TyApp f a) = go f ++ " " ++ go a

showType = show . ShowType

type Type = Fix TypeF
type TyId = Int
type Subst = M.Map TyId Type

data UnifyError
  = TyConMismatch TyName TyName
  | CannotUnify Type Type
  | OccurCheck Type Type

instance Show UnifyError where
  show (TyConMismatch c1 c2) = "TyConMismatch " ++ c1 ++ " " ++ c2
  show (CannotUnify t1 t2) = "CannotUnify " ++ show (ShowType t1) ++ " " ++ show (ShowType t2)
  show (OccurCheck v t) = "OccurCheck " ++ show (ShowType v) ++ " " ++ show (ShowType t)

funTyCon = TyCon "->"
funTy a b = funTyCon `TyApp` a `TyApp` b
funArrows args res = foldr funTy res args

type UnifyM = Either UnifyError

unify :: Type -> Type -> Subst -> UnifyM Subst
unify t1 t2 tenv = case (t1, t2) of
  (TyCon c1, TyCon c2) | t1 == t2 -> pure tenv
                       | otherwise -> Left $ TyConMismatch c1 c2
  (TyApp f1 a1, TyApp f2 a2) -> unify f1 f2 tenv >>= unify a1 a2

  (TyVar True v1, TyVar True v2)
    | v1 == v2 -> pure tenv
    | otherwise -> Left $ CannotUnify t1 t2
  (TyVar _ v1, TyVar _ v2) | v1 == v2 -> pure tenv
  (v@(MonoTyVar {}), t) -> v -+> t
  (t, v@(MonoTyVar {})) -> v -+> t
  _ -> Left $ CannotUnify t1 t2
 where
  v@(TyVar _ i) -+> t = if i `IS.notMember` tyVars (const True) t
    then tryExtend i t tenv
    else Left $ OccurCheck v t

polyTyVars = tyVars id
monoTyVars = tyVars not

tyVars :: (Bool -> Bool) -> Type -> IS.IntSet
tyVars ck = cata go
 where
  go (TyConF _) = IS.empty
  go (TyVarF poly v) = if ck poly then IS.singleton v else IS.empty
  go (TyAppF f a) = f `IS.union` a

unifyC t1 t2 tenv = unify (c t1) (c t2) tenv
 where
  c = canonize tenv

canonize :: Subst -> Type -> Type
canonize env = cata go
 where
  go t@(TyConF _) = Fix t
  go t@(TyVarF b v) = maybe (Fix t) (canonize $ M.delete v env) $ M.lookup v env
  go (TyAppF f a) = TyApp f a

tryExtend :: TyId -> Type -> Subst -> UnifyM Subst
tryExtend v t tenv = case M.lookup v tenv of
  Nothing -> pure $ M.insert v t tenv
  Just v' -> unify v' t tenv
