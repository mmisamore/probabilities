{-# language GeneralizedNewtypeDeriving #-} 
{-# language StandaloneDeriving #-}
module Examples 
where

import Data.List
import Data.Maybe
import Data.Function
import Data.Ratio
import Control.Applicative

-- So we don't have to worry about testing equalities of floats
newtype Probability = P { unProb :: Rational }
  deriving (Show,Eq,Ord,Num,Fractional,Real)

-- Treat a rational as a probability
prob :: Rational -> Probability
prob p = if p >= 0 && p <= 1 then P p else P 0
                             --
-- Simple representation of a distribution
newtype Dist a = Dist { unDist :: [(a,Probability)] }

-- Get list of masses underlying distribution
masses :: Dist a -> [(a,Probability)]
masses = unDist

-- Get just the list of values from a distribution
values :: Dist a -> [a]
values = map fst . masses

-- Get just the list of probabilities from a distribution
probs :: Dist a -> [Probability]
probs = map snd . masses

-- We can show distributions as long as we can normalize them
instance (Eq a, Show a) => Show (Dist a) where
  show (Dist ds) = show (normalize ds)

-- Total putative probability represented by list of masses
totalProb :: [(a,Probability)] -> Rational
totalProb = toRational . sum . map snd

-- Determine if a list of masses represents a distribution
isDist :: [(a,Probability)] -> Bool
isDist d = totalProb d == 1

-- Normalize a list of masses to total mass 1, removing duplicates.
-- Note the equality constraint on "a": this is needed to identify
-- duplicates. We can have distributions whose elements are functions
-- so this is relevant!
normalize :: Eq a => [(a,Probability)] -> [(a,Probability)] 
normalize aps = [(a, p a) | a <- distinctAs]
  where distinctAs    = nub (map fst aps)
        numerator a   = totalProb . filter ((== a) . fst) $ aps 
        denominator   = totalProb aps
        p a           = P (numerator a / denominator) 

-- Construct a uniform distribution from a finite list. Note that we
-- have no notion of equality in general so there cannot be "duplicates"
uniform :: [a] -> Dist a
uniform as = Dist [(a,p) | a <- as]
  where p = P (1 % (toInteger (length as)))

-- An unweighted die is a uniform distribution
die :: Dist Int
die = uniform [1..6]

-- An unbiased coin is a uniform distribution
coin :: Dist Int
coin = uniform [0..1]

-- An unbiased set of numeric functions 
funcs :: Dist (Int -> Int)
funcs = uniform [(+1),(+2),(+3)]

-- Construct a distribution representing a certain occurrence
certainly :: a -> Dist a
certainly a = Dist [(a,1)]

-- An event is just a subset of the powerset of "a"
type Event a = a -> Bool

-- Given an Event and a Dist, determine the probability mass of the Event
-- with respect to the Dist
(??) :: Event a -> Dist a -> Probability
(??) e = P . totalProb . filter (e . fst) . masses 

-- Examples: (==1) ?? die
--           (`elem` [1,2]) ?? die
--           (`elem` [1,2,3]) ?? die

-- A distribution is a functor: we keep the probabilities the same but
-- change the type of the points
instance Functor Dist 
  where fmap f (Dist aps) = Dist [(f a, p) | (a,p) <- aps]
 
-- Distributions are also applicative...
instance Applicative Dist
  where pure = certainly
        Dist fps <*> Dist aqs = 
          Dist [(f a, p*q) | (f,p) <- fps, (a,q) <- aqs]

-- ... so we can join them together with functions
joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith = liftA2

-- ... and they are automatically Numeric if the point types are Numeric
instance Num a => Num (Dist a)
  where (+) = liftA2 (+)
        (-) = liftA2 (-)
        (*) = liftA2 (*)
        negate = fmap negate
        abs    = fmap abs
        signum = fmap signum
        fromInteger = certainly . fromInteger 

-- Distribution for n dice rolls 
dice :: Int -> Dist [Int]
dice 0 = certainly []
dice n | n > 0 = (:) <$> die <*> dice (n-1)
dice _ = certainly []

-- Snake eyes: (==[1,1]) ?? dice 2 

-- Number of ways of choosing k elements from a set of n distinct elements
choose :: Int -> Int -> Integer
choose n 0 = 1
choose n k | k == n = 1
choose n k | 0 < k && k < n = choose (n-1) (k-1) + choose (n-1) k
choose _ _ = 1

-- Binomial distribution
binomial :: Int -> Probability -> Dist Int
binomial n (P p) = Dist [(k, P (nChoose k * p^k * (1 - p)^(n-k))) | k <- [0..n]]
  where nChoose k = toRational (n `choose` k)

-- Choose an element from a list, removing it 
selectOne :: (Eq a) => [a] -> Dist (a,[a])
selectOne as = uniform [(a, delete a as) | a <- as]

-- Example: selectOne [1,2,3]
-- Note that there is nothing random about these functions: they are
-- totally deterministic and represent the full distributions.

-- The monad instance allows us to sequence choices that depend 
-- on previous choices:
instance Monad Dist where
    return = certainly
    -- (>>=) :: Dist a -> (a -> Dist b) -> Dist b
    Dist aps >>= f = Dist [(b,p*q) | (a,p) <- aps, (b,q) <- masses (f a)]

-- ... and now we can select more than once without replacement:
selectMany :: (Eq a) => Int -> [a] -> Dist ([a],[a])
selectMany 0 as = certainly ([],as) 
selectMany n as | n > 0 = do
    (a,rs)  <- selectOne as
    (bs,ss) <- selectMany (n-1) rs
    return (a:bs,ss)
selectMany _ _ = certainly ([],[])


-- The Monty Hall problem
data Outcome = Win | Lose deriving (Show,Eq)

-- Contestant's initial choice of door
firstChoice :: Dist Outcome
firstChoice = uniform [Win,Lose,Lose]

-- Act of switching to the "other" door
switch :: Outcome -> Dist Outcome
switch Win  = certainly Lose
switch Lose = certainly Win

-- Solution:
-- firstChoice vs. firstChoice >>= switch

-- Cumulative distribution function for a distribution
cdf :: Dist a -> [(a,Rational)]
cdf d = [(a,unProb p) | (a,p) <- zip (values d) totalProbs]
  where totalProbs = drop 1 (scanl (+) 0 (probs d))

-- Sample by inverting the cumulative distribution
sample :: Rational -> Dist a -> a
sample r d = if r == 1 then last (values d)
                       else head (drop (sampleLength d) (values d))
  where sampleLength = length . takeWhile (r >=) . map snd . cdf  


-- Monty Hall, with more detail
data Door = A | B | C deriving Eq

-- Doors contestant can choose from
doors :: [Door]
doors = [A,B,C]

-- The game state
data State = State { 
  prize  :: Maybe Door, 
  chosen :: Maybe Door,
  opened :: Maybe Door 
}

-- Initial game state
gameStart = State { prize = Nothing, chosen = Nothing, opened = Nothing } 

-- Hiding the prize behind a door
hidePrize :: State -> Dist State
hidePrize s = uniform [s { prize = Just d } | d <- doors ]

-- Contestant chooses a door
chooseDoor :: State -> Dist State
chooseDoor s = uniform [s { chosen = Just d } | d <- doors ]

-- Host opens a door that is neither the chosen one nor the prize one
openDoor :: State -> Dist State
openDoor s = uniform [
    s { opened = Just d } | d <- doors \\ catMaybes [chosen s, prize s]
  ]

-- Contestant strategy of switching to the unchosen, unopened door 
switchDoor :: State -> Dist State
switchDoor s = uniform [
    s { chosen = Just d } | d <- doors \\ catMaybes [chosen s, opened s]
  ]

-- Contestant strategy of staying with the chosen door
stay :: State -> Dist State
stay = certainly 

-- Some type synonyms
type Strategy = State -> Dist State 
type StateTransition = State -> Dist State 

-- Starting with a strategy, we can describe the whole game
game :: Strategy -> [StateTransition]
game strat = [hidePrize,chooseDoor,openDoor,strat]


