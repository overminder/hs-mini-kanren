module Lib where

import Term
import Infer
import qualified Example

runExample :: IO ()
runExample = print (fmap ShowType $ runInfer Example.example emptyTermEnv)
