module Infer where

import Expr
import Type

import Debug.Trace
import Control.Lens
import qualified Data.List as L
import Data.Either.Combinators (mapLeft)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Data.Functor.Foldable hiding (Foldable)
import Control.Monad.Trans
import Control.Monad.State.Strict

type Gen = (TyId, Subst)
type GenT = StateT Gen

emptyGen = (0, M.empty)

freshTyId :: (Monad m, MonadState Gen m) => m TyId
freshTyId = use _1 <* (_1 += 1)

freshTyVar :: (Monad m, MonadState Gen m) => Bool -> m Type
freshTyVar poly = TyVar poly <$> freshTyId

canonizeI :: Type -> InferM Type
canonizeI t = uses _2 (`canonize` t)

type InferM = GenT (Either InferError)

emptyGamma = M.empty

runUnify :: Type -> Type -> InferM ()
runUnify t1 t2 = do
  tenv <- use _2
  tenv' <- lift . mapLeft UnifyError $ unifyC t1 t2 tenv
  _2 .= tenv'

runInfer :: Expr -> Gamma -> Either InferError QType
runInfer term env = (`evalStateT` emptyGen) $ infer term env

-- From unification-fd
cataM :: (Traversable f, Monad m) => (f a -> m a) -> (Fix f -> m a)
cataM phiM = self
  where
    self = phiM <=< (mapM self . unFix)
    unFix (Fix x) = x

instantiate :: QType -> InferM QType
instantiate qt@(Forall ps t) = do
  ms <- mapM (const $ freshTyVar False) ps
  let
    p2m = IM.fromList (zip ps ms)
    t' = replaceType p2m t
  trace ("instantiating " ++ showType t ++ " as " ++ showType t') $ return (simpleQType t')

replaceType :: IM.IntMap Type -> Type -> Type
replaceType v2t t
  -- | trace ("replaceType: " ++ show v2t ++ ", " ++ showType t) False = undefined
  | otherwise = cata go t
 where
  go t = case t of
    TyConF c -> TyCon c
    TyAppF f a -> TyApp f a
    TyVarF _ v -> fromMaybe (Fix t) (IM.lookup v v2t)

-- Universally quantified type.
data QType = Forall { quals :: ![TyId], typeOf :: !Type }
simpleQType = Forall []
withQuals (Forall qs e) qs' = Forall (qs ++ qs') e

instance Show QType where
  show (Forall p e) = "forall " ++ showP p ++ ". " ++ showType e
   where
    showP = L.intercalate " " . map (showType . PolyTyVar)

type Gamma = M.Map Name QType

skolemize :: Type -> InferM QType
skolemize t = do
  let ms = IS.toList $ monoTyVars t
  m2p <- forM ms $ \ m -> do
    p <- freshTyVar True
    runUnify (MonoTyVar m) p
    return (m, p)
  let
    t' = Forall (map (unTyVar . snd) m2p) $ replaceType (IM.fromList m2p) t
  trace ("skolemizing " ++ showType t ++ " as " ++ show t') $ return t'

data InferError
  = Unbound Name
  | UnifyError UnifyError
  deriving (Show)

infer :: Expr -> Gamma -> InferM QType
infer term env = case term of
  Var v -> do
    t <- lift $ fromMaybe (Left $ Unbound v) (Right <$> M.lookup v env)
    instantiate t
  Lam vs e -> do
    vts <- mapM (const $ freshTyVar False) vs
    let
      env' = M.fromList (zip vs (map simpleQType vts)) `M.union` env
    et <- infer e env' -- Should be skolemized?
    let ft = vts `funArrows` (typeOf et)
    ft' <- skolemize =<< canonizeI ft
    return $ ft' `withQuals` (quals et)
  App f a -> do
    at <- infer a env
    ft <- infer f env
    et <- freshTyVar False
    runUnify (funTy (typeOf at) et) (typeOf ft)
    et' <- canonizeI et
    return $ Forall (quals at ++ quals ft) et'
  Let bs e -> do
    bs' <- forM bs $ \ (v, e) -> do
      e' <- infer e env
      return (v, e')
    infer e (M.fromList bs' `M.union` env)
