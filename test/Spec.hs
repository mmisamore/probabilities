import Test.QuickCheck
import Examples
import Data.Ratio
import Data.List 
import Control.Monad

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

-- Enumerating masses of a distribution gives back the distribution
enumDistMassesId :: Dist Numeric -> Bool
enumDistMassesId da = enumDist (masses da) == da

-- A die is a distribution
dieIsDist :: Bool
dieIsDist = isDist (masses die) 

-- A coin is a distribution
coinIsDist :: Bool
coinIsDist = isDist (masses coin)

-- Certainly anything should be a distribution
certainlyIsDist :: Numeric -> Bool
certainlyIsDist n = isDist (masses (certainly n))

-- Total probability over all events is always 1 
sumPdfIsOne :: Dist Numeric -> Bool
sumPdfIsOne da = (const True) ?? da == 1  

-- Total probability for impossible event is 0
sumImpossibleIsZero :: Dist Numeric -> Bool
sumImpossibleIsZero da = (const False) ?? da == 0 

-- Distributions are functors: they respect function composition 
distRespectsDot :: Dist Numeric -> Gen Bool
distRespectsDot da = do
  n <- arbitrary
  m <- arbitrary
  return $ (fmap (+n) . fmap (*m)) da == fmap ((+n) . (*m)) da

-- Distributions are functors: they respect identity
distRespectsId :: Dist Numeric -> Bool
distRespectsId da = (fmap id da == da)

-- Distributions are also monoidal for the applicative structure
distMonoidalProd :: Dist Numeric -> Dist Numeric -> Dist Numeric -> Bool
distMonoidalProd da db dc = 
  fmap right ((da <&> db) <&> dc) == da <&> (db <&> dc) 
  where right ((a,b),c) = (a,(b,c))

-- Distributions respect the left monoidal unit, which is "pure"
distLeftMonoidalUnit :: Numeric -> Dist Numeric -> Bool
distLeftMonoidalUnit a db = (pure a <&> db) == fmap (\b -> (a,b)) db

-- Distributions respect the right monoidal unit, which is also "pure"
distRightMonoidalUnit :: Numeric -> Dist Numeric -> Bool
distRightMonoidalUnit a db = (db <&> pure a) == fmap (\b -> (b,a)) db 

-- n dice is a distribution
diceIsDist :: Int -> Bool
diceIsDist n = isDist (masses (dice n))

-- Binomial distributions are distributions
binomialIsDist :: Int -> Probability -> Bool
binomialIsDist n p = isDist (masses (binomial n p))

-- Selecting one and recombining always gives back the original list 
selectOneConcat :: [Numeric] -> Bool
selectOneConcat [] = True 
selectOneConcat ns = all (== ns') (values recombined) where 
  recombined = fmap (sort . uncurry (:)) (selectOne ns) :: Dist [Numeric] 
  ns'        = sort ns

-- Distributions are monads: they are associative
distIsMonadAssoc :: [Numeric] -> Bool
distIsMonadAssoc ns = ((f >=> g) >=> h) ns == (f >=> (g >=> h)) ns
  where f = selectOne  
        g = selectOne . snd
        h = g

-- Distributions are monads: they are left unital
distIsMonadLeftUnit :: [Numeric] -> Bool
distIsMonadLeftUnit ns = (return ns >>= selectOne) == (selectOne ns)



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
  quickCheck enumDistMassesId
  quickCheck dieIsDist
  quickCheck coinIsDist 
  quickCheck certainlyIsDist 
  quickCheck sumPdfIsOne 
  quickCheck sumImpossibleIsZero 
  quickCheck distRespectsDot 
  quickCheck distRespectsId 
  quickCheckWith (stdArgs {maxSuccess = 10}) distMonoidalProd 
  quickCheck distLeftMonoidalUnit
  quickCheck distRightMonoidalUnit 
  quickCheckWith (stdArgs {maxSize = 5}) diceIsDist 
  quickCheckWith (stdArgs {maxSize = 10}) binomialIsDist 
  quickCheck selectOneConcat 
  quickCheckWith (stdArgs {maxSize = 10}) distIsMonadAssoc 
  quickCheck distIsMonadLeftUnit 

