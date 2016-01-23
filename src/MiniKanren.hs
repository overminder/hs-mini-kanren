module MiniKanren (
  run,
  repl
) where

import MiniKanren.Term
import MiniKanren.Parser
import MiniKanren.Predicate

import System.IO
import Text.Parsec (ParseError)
import Control.Monad
import Data.Either.Combinators

data ReplError
  = ParseTerm ParseError
  | CompilePredicate CompileError
  | Run [UnifyError]
  deriving (Show)

repl = forever $ do
  hPutStr stdout "mini-kanren-repl> "
  hFlush stdout
  line <- getLine
  let
    res = do
      term <- mapLeft ParseTerm $ parseTerm line
      pred <- mapLeft CompilePredicate $ compileP term
      mapLeft Run $ run pred
  case res of
    Left e -> print e
    Right r -> print r

accumEither :: [Either e a] -> ([e], [a])
accumEither = foldr (\ ei (es, as) -> either (\e -> (e:es, as)) (\ a -> (es, a:as)) ei) ([], [])

run :: Predicate -> Either [UnifyError] [Env]
run = anyOrLeft . accumEither . runs
 where
  anyOrLeft (es, []) = Left es
  anyOrLeft (_, rs) = Right rs
  runs (PEq t1 t2) = [unify t1 t2 emptyEnv]
  runs (PConde p1 p2) = runs p1 ++ runs p2

--run :: Predicate -> Env
--run (PEq t1 t2) = unify t1 t2 emptyEnv
