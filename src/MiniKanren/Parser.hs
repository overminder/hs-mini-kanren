module MiniKanren.Parser where

import MiniKanren.Term

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String

parseTerm :: String -> Either ParseError Term
parseTerm = parse pTerm "Term"

pTerm =
  TAtom <$> pAtom <|>
  foldPair <$> pList <|>
  TVar <$> pVar

pAtom :: Parser Atom
pAtom = char '\'' *> (Atom . T.pack <$> many1 letter)

pList :: Parser [Term]
pList = char '(' *> pTerm `sepBy` space <* char ')'

pVar :: Parser Var
pVar = Var . T.pack <$> many1 letter
