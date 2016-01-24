module MiniKanren.Predicate (
  module MiniKanren.Predicate,
  module MiniKanren.Backtracking,
  module Control.Monad.Logic
) where

import MiniKanren.Term
import MiniKanren.Backtracking
import Debug.Trace

import qualified Data.Map as M
import Control.Monad.Logic
import qualified Data.Text as T
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Function (on)

data Predicate
  = PEq Term Term
  | PNeq Term Term
  | PConde Predicate Predicate
  | PAnd Predicate Predicate
  | PFreshN [T.Text] ([Term] -> Predicate)
  | PSucceed
  | PFail

data BtState
  = BtState
  { btEnv :: Env
  , btNeq :: [(Term, Term)]
  , btGen :: Int
  }

emptyState :: BtState
emptyState = BtState emptyEnv [] 0

checkNeqs :: BtState -> Logic BtState
checkNeqs s@BtState {..} = foldr (go btEnv) (return s) btNeq
 where
  go env (l, r) m = case unifyC l r env of
    Left _ -> m
    Right oops -> if oops `domainEq` env then mzero else m
  domainEq = (==) `on` domain

(===) :: Term -> Term -> Predicate
(===) = PEq

(=/=) :: Term -> Term -> Predicate
(=/=) = PNeq

conde :: [Predicate] -> Predicate
conde = foldr PConde PFail

program :: [Predicate] -> Predicate
program = foldr PAnd PSucceed

freshN :: [T.Text] -> ([Term] -> Predicate) -> Predicate
freshN = PFreshN

solveP :: Predicate -> BtState -> Logic BtState
solveP p s@BtState {..} = do
  case p of
    PEq t1 t2 -> do
      case unifyC t1 t2 btEnv of
        Left _ -> mzero
        Right env -> checkNeqs (s{btEnv = env})
    PNeq t1 t2 -> checkNeqs (s{btNeq = (t1, t2):btNeq})
    PAnd p1 p2 -> solveP p1 s >>- solveP p2
    PConde p1 p2 -> solveP p1 s `interleave` solveP p2 s
    PFail -> mzero
    PSucceed -> return s
    PFreshN names mkP -> 
      let s' = s{btGen = btGen + length names}
          newVars = zipWith VNamed names [btGen..]
          p' = mkP (map TVar newVars)
          rmFreshes = \ s@BtState {..} -> s {
            btGen = btGen - length names,
            btEnv = foldr M.delete btEnv newVars
          }
       in rmFreshes <$> solveP p' s'

