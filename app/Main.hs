module Main where

import MiniKanren

prog1 q = entry
 where
  entry = freshN ["q1", "q2", "q3"] $ \ [q1, q2, q3] ->
    program [ fiboo (mkNat 10) q ]
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

  xs = list ["1", "2", "3"]

zero_ = "O"
succ_ = TPair "S"
one = succ_ zero_
two = succ_ one
mkNat 0 = zero_
mkNat n = succ_ $ mkNat (n - 1)

fromNat (TAtom "O") = 0
fromNat (TPair "S" x) = 1 + fromNat x

main :: IO ()
main = do
  print $ map fromNat $ runN 10 prog1
