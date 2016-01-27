module Lib where

import Expr
import Infer
import Type
import qualified Example

runExample :: IO ()
runExample = print . (`runInfer` emptyGamma) $ Example.example
