module Example where

import Expr

import Prelude hiding (id)

id = Lam "x" $ Var "x"
s = Lam "f" $ Lam "g" $ Lam "x" $ "f" `App` "x" `App` ("g" `App` "x")
k = Lam "x" $ Lam "y" $ "x"
skk = (Lam "s" $ Lam "k" $ "s" `App` "k" `App` "k") `App` s `App` k

example = skk
