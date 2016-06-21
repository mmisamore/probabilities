module Main where

import Examples 

main :: IO ()

main = do 
  d <- unRDist (rSampleTrans 1000 (8 .* oneYear) (Alive 0))
  print d

-- main = print $ (8 .* oneYear) (Alive 0)

