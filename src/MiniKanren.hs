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
  deriving (Show)

repl = forever $ do
  hPutStr stdout "mini-kanren-repl> "
  hFlush stdout
  line <- getLine
  let
    res = do
      term <- mapLeft ParseTerm $ parseTerm line
      pred <- mapLeft CompilePredicate $ (`evalStateT` 0) $ compileP term
      return $ run pred
  case res of
    Left e -> print e
    Right r -> print r

accumEither :: [Either e a] -> ([e], [a])
accumEither = foldr (\ ei (es, as) -> either (\e -> (e:es, as)) (\ a -> (es, a:as)) ei) ([], [])

run :: Predicate -> [Env]
run = map btEnv . (`execBT` emptyState) . solveP

