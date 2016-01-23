module MiniKanren.Term where

import Data.String (IsString)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map as M

newtype Var = Var { unVar :: T.Text }
  deriving (Eq, Ord)

instance Show Var where
  show = T.unpack . unVar

newtype Atom = Atom { unAtom :: T.Text }
  deriving (Eq, Ord, IsString)

instance Show Atom where
  show (Atom a) = '\'' : T.unpack a

data Term
  = TAtom !Atom
  | TPair !Term !Term
  | TVar !Var
  deriving (Eq)

instance Show Term where
  show (TAtom a) = show a
  show t@(TPair {}) = showList t
   where
    showList t = "(" ++ L.intercalate " " (map show ts) ++ lastShown ++ ")"
     where
      lastShown = if last == nil then "" else " . " ++ show last
      (ts, last) = unfoldPair t
  show (TVar v) = show v

foldPair :: [Term] -> Term
foldPair = foldr TPair nil

unfoldPair :: Term -> ([Term], Term)
unfoldPair (TPair t1 t2) = let (ts, last) = unfoldPair t2 in (t1:ts, last)
unfoldPair t = ([], t)

type Env = M.Map Var Term

data UnifyError
  = OccursCheck Var Term
  | AtomsDiffer Atom Atom
  | TypeMismatch Term Term
  deriving (Show)

unify :: Term -> Term -> Env -> Either UnifyError Env
unify t1 t2 env = case (t1, t2) of
  (TAtom a1, TAtom a2) -> if a1 == a2
    then pure env
    else Left $ AtomsDiffer a1 a2
  (TPair f1 s1, TPair f2 s2) -> unify f1 f2 env >>= unify s1 s2
  (TVar v1, TVar v2) | v1 == v2 -> pure env
  (_, TVar v2) -> t1 `unifyVar` v2
  (TVar v1, _) -> t2 `unifyVar` v1
  _ -> Left $ TypeMismatch t1 t2
 where
  t `unifyVar` v = if v `notIn` t
    then tryExtend v t env
    else Left $ OccursCheck v t

notIn :: Var -> Term -> Bool
notIn v1 t2 = case t2 of
  TVar v2 -> v1 /= v2
  TAtom _ -> True
  TPair f s -> v1 `notIn` f && v1 `notIn` s

tryExtend v t env = case M.lookup v env of
  Just t0 -> unify t t0 env
  Nothing -> pure $ M.insert v t env

emptyEnv :: Env
emptyEnv = M.empty

nil :: Term
nil = TAtom "nil"

