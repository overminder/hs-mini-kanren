module MiniKanren.Predicate where

import MiniKanren.Term

data Predicate
  = PEq !Term !Term
  | PNeq !Term !Term
  | PConde !Predicate !Predicate
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
    ("conde", [t1, t2]) -> PConde <$> compileP t1 <*> compileP t2
    _ -> Left $ UnknownClause tag ts
 where
  unfoldPairAsList t = case unfoldPair t of
    (ts, rest) | rest == nil -> pure ts
               | otherwise -> Left $ NotAProperList t
  asAtom (TAtom a) = pure a
  asAtom t = Left $ NotAnAtom t
