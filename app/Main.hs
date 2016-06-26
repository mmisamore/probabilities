module Main where

import Examples 

main :: IO ()

main = do 
  let dist = 3 .* (rSampleTrans 100 oneYear)
  d <- unRand (unRDist (dist (Alive 0)))
  print d

-- main = print $ (8 .* oneYear) (Alive 0)
-- main = print $ (10 .* oneYear) (Alive 0)

