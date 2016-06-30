module Main where

import Examples 

main :: IO ()

-- main = do 
--   let dist = 9 .* (rSampleTrans 100 oneYear)
--   d <- runRDist (dist (Alive 0))
--   print d

main = print $ (9 .* oneYear) (Alive 0)

