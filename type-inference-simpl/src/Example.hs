module Example where

import Term

import Prelude hiding (id)

id = Lam "x" $ Var "x"
s = Lam "f" $ Lam "g" $ Lam "x" $ "f" `App` "x" `App` ("g" `App` "x")
k = Lam "x" $ Lam "y" $ "x"

example = s `App` k `App` k
