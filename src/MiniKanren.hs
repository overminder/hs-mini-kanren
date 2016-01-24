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

goal = TVar (VNamed "q" 0)

runN :: Int -> (Term -> Predicate) -> [Term]
runN n = map (\ s -> canonize (btEnv s) goal) . observeMany n . (`solveP` emptyState) . ($ goal)

