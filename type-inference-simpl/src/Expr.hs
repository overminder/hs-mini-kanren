module Expr where

import qualified Data.List as L
import Data.String (IsString(..))
import Data.Functor.Foldable

type Name = String

data ExprF a
  = VarF Name
  | LamF [Name] a
  | AppF a a
  | LetF [(Name, a)] a
  deriving (Functor)

type Expr = Fix ExprF

instance IsString Expr where
  fromString = Var . fromString

pattern Var v = Fix (VarF v)
pattern Lam v e = Fix (LamF v e)
pattern App f a = Fix (AppF f a)
pattern Let bs e = Fix (LetF bs e)

newtype ShowExpr = ShowExpr Expr

instance Show ShowExpr where
  show (ShowExpr t) = showExpr t

showExpr = cata go
 where
  go = \case
    VarF v -> v
    AppF f a -> f ++ " " ++ a
    LamF vs e -> "\\" ++ L.intercalate " " vs ++ " -> " ++ e
    LetF bs e -> "let " ++ L.intercalate "; " (map showB bs) ++ e

  showB (name, e) = name ++ " = " ++ e
