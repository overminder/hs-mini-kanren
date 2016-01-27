module Example where

import Expr

import Prelude hiding (id)

id = Lam ["x"] $ Var "x"
s = Lam ["f", "g", "x"] $ ("f" `App` "x") `App` ("g" `App` "x")
k = Lam ["x", "y"] $ "x"
skk = (Lam ["s", "k"] $ "s" `App` "k" `App` "k") `App` s `App` k
skk2 = Let [("s", s), ("k", k)] $ "s" `App` "k" `App` "k"

example = skk2
