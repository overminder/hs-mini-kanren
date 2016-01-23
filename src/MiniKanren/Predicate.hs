module MiniKanren.Predicate (
  module MiniKanren.Predicate,
  module MiniKanren.Backtracking
) where

import MiniKanren.Term
import MiniKanren.Backtracking

import Data.Function (on)

data Predicate
  = PEq !Term !Term
  | PNeq !Term !Term
  | PConde !Predicate !Predicate
  | PAnd !Predicate !Predicate
  deriving (Show)

data CompileError
  = NotAProperList Term
  | UnknownClause Atom [Term]
  | NotAnAtom Term
  deriving (Show)

compileP :: Term -> Either CompileError Predicate
compileP t = do
  ttag:ts <- unfoldPairAsList t
  tag <- asAtom ttag
  case (tag, ts) of
    ("eq", [t1, t2]) -> pure $ PEq t1 t2
    ("neq", [t1, t2]) -> pure $ PNeq t1 t2
    ("and", [t1, t2]) -> PAnd <$> compileP t1 <*> compileP t2
    ("conde", [t1, t2]) -> PConde <$> compileP t1 <*> compileP t2
    _ -> Left $ UnknownClause tag ts
 where
  unfoldPairAsList t = case unfoldPair t of
    (ts, rest) | rest == nil -> pure ts
               | otherwise -> Left $ NotAProperList t
  asAtom (TAtom a) = pure a
  asAtom t = Left $ NotAnAtom t

data BtState
  = BtState
  { btEnv :: Env
  , btNeq :: [(Term, Term)]
  }

emptyState :: BtState
emptyState = BtState emptyEnv []

checkNeqs :: BT BtState ()
checkNeqs = do
  s@BtState {..} <- solutions
  foldr (go btEnv) (return ()) btNeq
 where
  go env (l, r) m = case unifyC l r env of
    Left _ -> m
    Right oops -> if oops `domainEq` env then mzero else m
  domainEq = (==) `on` domain

solveP :: Predicate -> BT BtState ()
solveP p = do
  s@BtState {..} <- solutions
  case p of
    PEq t1 t2 -> do
      case unifyC t1 t2 btEnv of
        Left _ -> mzero
        Right env -> withBT (s{btEnv = env}) checkNeqs
    PNeq t1 t2 -> withBT (s{btNeq = (t1, t2):btNeq}) checkNeqs 
    PAnd p1 p2 -> solveP p1 >> solveP p2
    PConde p1 p2 -> solveP p1 <|> solveP p2
