module Main where

import MiniKanren

prog1 q = appendo xs xs q
 where
  appendo = \ l r o ->
    conde [ program [ l === nil, o === r]
          , freshN ["h", "t", "o'"] $ \ [h, t, o'] ->
            program [ TPair h t === l
                      , appendo t r o'
                      , TPair h o' === o
                      ]
          ]
  xs = list ["1", "2", "3"]

main :: IO ()
main = do
  print $ runN 1 prog1
