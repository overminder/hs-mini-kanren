module Lib where

import Expr
import Infer
import qualified Example

runExample :: IO ()
runExample = print . fmap ShowType . (`runInfer` emptyExprEnv) $ Example.example
