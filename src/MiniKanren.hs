module MiniKanren (
  module MiniKanren.Term,
  module MiniKanren.Predicate,
  runN,
) where

import MiniKanren.Term
import MiniKanren.Predicate

import System.IO
import Text.Parsec (ParseError)
import Control.Monad
import Data.Either.Combinators

accumEither :: [Either e a] -> ([e], [a])
accumEither = foldr (\ ei (es, as) -> either (\e -> (e:es, as)) (\ a -> (es, a:as)) ei) ([], [])

runN :: Int -> (Term -> Predicate) -> [Env]
runN n = map btEnv . observeMany n . (`solveP` emptyState) . ($ (TVar (VNamed "q" 0)))

