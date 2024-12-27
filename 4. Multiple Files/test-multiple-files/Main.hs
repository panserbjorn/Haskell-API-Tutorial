module Main where

import Ambiguous1 hiding (ambiguousFunction)
import Ambiguous1 as A1
import Ambiguous2 as A2
import Exposing
import Inner.ConstructPath
import MyModule

-- Uncomment the following line when you use the -i argument
-- import MyModule2

main :: IO ()
main = do
  --   print 5
  print $ myFunction 5
  print constructPathFunction
  print $ myFunction1 5
  print $ A1.ambiguousFunction 3
  print $ A2.ambiguousFunction 3

-- Not visible function from Exposing.hs because it is not exported
--   print $ myFunction3 5

-- Uncomment the following line when you use the -i argument
--   print $ module2Function
