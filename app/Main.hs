module Main where

import Examples 

main :: IO ()

main = do 
  d <- unRDist (rSampleTrans 20 (8 .* oneYear) (Alive 0))
  print d


-- main = print $ (8 .* oneYear) (Alive 0)

