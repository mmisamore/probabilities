import Test.QuickCheck
import Examples
import Data.Ratio

import Debug.Trace

-- Random probabilties
instance Arbitrary Probability where
  arbitrary = do
    n <- arbitrary `suchThat` (>= 0)
    d <- arbitrary `suchThat` (\d -> d >= n && d > 0) 
    return (prob (n % d))

-- Random finite distributions
instance (Ord a, Arbitrary a) => Arbitrary (Dist a) where
  arbitrary = do
    n  <- arbitrary `suchThat` (> 0)
    as <- vectorOf n arbitrary
    ps <- vectorOf n (arbitrary :: Gen Probability)
    let ms  = zip as ps
    let ms' = normalize ms
    return $! enumDist (fmap (fmap unProb) ms')

-- TODO: Random "randomized" distributions

main :: IO ()
main = print "Nothing to see here" 

