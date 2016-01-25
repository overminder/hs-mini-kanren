module Main where

import MiniKanren

prog1 q = entry
 where
  entry = freshN ["q1", "q2", "q3"] $ \ [q1, q2, q3] ->
    typeOfTerm s nil q
    --typeOfTerm (tApp (tApp s k) k) nil q

  id = tLam "x" $ tVar "x"
  ap = tLam "f" $ tLam "x" $ tApp (tVar "f") (tVar "x")

  s = tLam "f" $ tLam "g" $ tLam "x" $
    tApp (tApp (tVar "f") (tVar "x"))
         (tApp (tVar "g") (tVar "x"))

  k = tLam "x" $ tLam "y" $ tVar "x"

appendo = \ l r o ->
  conde [ program [ l === nil, o === r]
        , freshN ["h", "t", "o'"] $ \ [h, t, o'] ->
          program [ TPair h t === l
                  , appendo t r o'
                  , TPair h o' === o
                  ]
        ]
notMember = \ x xs ->
  conde [ program [ xs === nil, PSucceed ]
        , freshN ["y", "ys"] $ \ [y, ys] ->
          program [ TPair y ys === xs
                  , y =/= x
                  , notMember x ys
                  ]
        ]
member = \ x xs ->
  conde [ program [ xs === nil, PFail ]
        , freshN ["y", "ys"] $ \ [y, ys] ->
          program [ TPair y ys === xs
                  , conde [ program [ y === x ], member x ys ]
                  ]
        ]

assocExt = \ k v xs o -> TPair (TPair k v) xs === o

assocEmpty = nil

assocRead = \ x xs o ->
  conde [ program [ xs === nil, PFail ]
        , freshN ["y", "ys", "v"] $ \ [y, ys, v] ->
          program [ TPair y ys === xs
                  , conde [ program [ TPair x v === y, o === v ]
                          , assocRead x ys o
                          ]
                  ]
        ]
kvs = list [TPair "x" "5", TPair "y" "8"]

addo = \ a b o ->
  conde [ program [ a === "O", o === b ]
        , freshN ["a'", "o'"] $ \ [a', o'] ->
          program [ TPair "S" a' === a
                  , addo a' b o'
                  , TPair "S" o' === o
                  ]
        ]
subo = \ l r o -> addo r o l
lessThano = \ l r ->
  conde [ program [ l === zero_, isPair r ]
        , freshN ["l'", "r'"] $ \ [l', r'] ->
          program [ TPair "S" l' === l
                  , TPair "S" r' === r
                  , lessThano l' r'
                  ]
        ]

fiboo = \ n o ->
  conde [ program [ lessThano n two, o === n ]
        , freshN ["o1", "o2", "ns1", "ns2"] $ \ [o1, o2, ns1, ns2] ->
          program [ subo n one ns1
                  , subo n two ns2
                  , fiboo ns1 o1
                  , fiboo ns2 o2
                  , addo o1 o2 o
                  ]
        ]

isPair = \ r -> freshN ["h", "t"] $ \ [h, t] -> TPair h t === r

languages = list [ "c", "c++", "haskell", "prolog", "scheme" ]
functional = list [ "haskell", "scheme" ]
imperative = list [ "c", "c++" ]
prolog = \ q ->
  program [ member q languages
          , notMember q imperative
          , notMember q functional
          ]

someAtoms = list ["1", "2", "3"]

zero_ = "O"
succ_ = TPair "S"
one = succ_ zero_
two = succ_ one
mkNat 0 = zero_
mkNat n = succ_ $ mkNat (n - 1)

fromNat (TAtom "O") = 0
fromNat (TPair "S" x) = 1 + fromNat x

--tyCon a = list ["tyCon", a]
--tyApp a b = list ["tyApp", a, b]
funTy a b = list [a, "->", b]

tVar x = list ["Var", x]
tLam x y = list ["Lam", x, y]
tApp x y = list ["App", x, y]
typeOfTerm term env o =
  conde [ freshN ["v"] $ \ [v] ->
          program [ term === tVar v
                  , assocRead v env o
                  ]
        , freshN ["v", "tv", "e", "env'", "o'"] $ \ [v, tv, e, env', o'] ->
          program [ term === tLam v e
                  , assocExt v tv env env'  -- tv: a generalized type
                  , typeOfTerm e env' o'
                  , funTy tv o' === o
                  ]
        , freshN ["f", "fType", "a", "aType"] $ \
                  [f, fType, a, aType] ->
          program [ term === tApp f a
                  , typeOfTerm a env aType
                  , typeOfTerm f env (funTy aType o)
                  ]
        ]

main :: IO ()
main = do
  print $ runN 5 prog1
