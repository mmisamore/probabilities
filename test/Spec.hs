import Test.QuickCheck
import Examples
import Data.Ratio

-- Random numerics in [0,1]
instance Arbitrary Numeric where
  arbitrary = do
    n <- arbitrary `suchThat` (>= 0)
    d <- arbitrary `suchThat` (\d -> d >= n && d > 0) 
    return (toNumeric (n % d))

-- Random probabilties
instance Arbitrary Probability where
  arbitrary = do
    n <- arbitrary :: Gen Numeric
    return (P n)

-- Random finite distributions
instance (Monoid a, Ord a, Arbitrary a) => Arbitrary (Dist a) where
  arbitrary = do
    n  <- arbitrary `suchThat` (> 0)
    as <- vectorOf n arbitrary
    return (simplify (uniform as))

-- Probabilities are always in [0,1]
probabilityInUnitInterval :: Probability -> Bool
probabilityInUnitInterval p = (p >= prob 0) && (p <= prob 1)

-- Distributions have total mass 1 
probabilityTotalMassOne :: Dist Numeric -> Bool
probabilityTotalMassOne da = totalProb (masses da) == 1

-- Sum of probabilities has mass 1
probabilityProbsMassOne :: Dist Numeric -> Bool 
probabilityProbsMassOne da = sum (probs da) == 1

-- Distributions are distributions
distIsDist :: Dist Numeric -> Bool
distIsDist da = isDist (masses da) 

-- Simplifying a distribution gives another distribution
simplifyIsDist :: Dist Numeric -> Bool
simplifyIsDist da = isDist (masses (simplify da))

-- Simplifying twice is the same as simplifying once
simplifyIdempotent :: Dist Numeric -> Bool
simplifyIdempotent da = simplify (simplify da) == simplify da

-- Values of any uniform distribution gives back values we started with
valuesOfUniformId :: [Numeric] -> Bool
valuesOfUniformId ns = if ns /= [] 
                       then values (uniform ns) == ns 
                       else values (uniform ns) == [mempty]

-- Uniform distributions are distributions
uniformIsDist :: [Numeric] -> Bool
uniformIsDist ns = isDist (masses (uniform ns))

-- enumDistIsDist :: Dist Numeric -> Bool
-- enumDistIsDist da = isDist (masses (enumDist (masses da :: [(Numeric,Rational)])))

-- TODO: Random "randomized" distributions

main :: IO ()
main = do
  quickCheck probabilityInUnitInterval 
  quickCheck probabilityTotalMassOne 
  quickCheck probabilityProbsMassOne 
  quickCheck distIsDist 
  quickCheck simplifyIsDist 
  quickCheck simplifyIdempotent 
  quickCheck valuesOfUniformId 
  quickCheck uniformIsDist 
  -- quickCheck enumDistIsDist 

