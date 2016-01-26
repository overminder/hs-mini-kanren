module Term where

import Data.String (IsString(..))
import Data.Functor.Foldable

type Name = String

data TermF a
  = VarF Name
  | LamF Name a
  | AppF a a
  deriving (Show, Functor)

type Term = Fix TermF

instance IsString Term where
  fromString = Var . fromString

pattern Var v = Fix (VarF v)
pattern Lam v e = Fix (LamF v e)
pattern App f a = Fix (AppF f a)

