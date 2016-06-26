{-# language GeneralizedNewtypeDeriving #-} 
{-# language StandaloneDeriving #-}
{-# language BangPatterns #-}
module Examples 
where

import Data.Coerce
import Data.Map.Strict as M (fromListWith,toList)
import Data.List
import Data.Maybe
import Data.Function
import Data.Ratio
import Control.Applicative
import Control.Monad
import System.Random 

-- So we don't have to worry about testing equalities of floats
newtype Probability = P { unProb :: Rational }
  deriving (Show,Eq,Ord,Num,Fractional,Real)

-- Treat a rational as a probability
prob :: Rational -> Probability
prob p = if p >= 0 && p <= 1 then P p else P 0

-- Simple representation of a distribution
newtype Dist a = Dist { unDist :: [(a,Probability)] }

-- Get list of masses underlying distribution
masses :: Dist a -> [(a,Probability)]
masses = coerce 

-- Get just the list of values from a distribution
values :: Dist a -> [a]
values = map fst . masses

-- Get just the list of probabilities from a distribution
probs :: Dist a -> [Probability]
probs = map snd . masses

-- We can show distributions as long as we can normalize them
instance (Ord a, Show a) => Show (Dist a) where
  show (Dist ds) = show (normalize ds) 

-- Total putative probability represented by list of masses
totalProb :: [(a,Probability)] -> Rational
totalProb = toRational . sum . map snd

-- Determine if a list of masses represents a distribution
isDist :: [(a,Probability)] -> Bool
isDist d = totalProb d == 1

-- Normalize a list of masses to total mass 1, removing duplicates.
normalize :: Ord a => [(a,Probability)] -> [(a,Probability)] 
normalize = toList . M.fromListWith (+)

-- Mathematically this is a no-op, but operationally it can enormously
-- reduce the size of the representation, making sampling tractable
norm :: Ord a => Dist a -> Dist a
norm = coerce . normalize . masses

-- Construct a uniform distribution from a finite list. Note that we
-- have no notion of equality in general so there cannot be "duplicates"
uniform :: [a] -> Dist a
uniform [] = fail "unsafe: uniform called on empty list"
uniform as = coerce [(a,p) | a <- as]
  where p = P (1 % (toInteger (length as)))

-- Given explicitly enumerated list of elements and probabilities, produce
-- a distribution. Like "uniform", this function isn't total.
enumDist :: [(a,Rational)] -> Dist a
enumDist ars = if isDist (masses putative) 
                 then putative 
                 else fail "unsafe: enumDist called on non-distribution"
  where putative = coerce [(a,prob r) | (a,r) <- ars]

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
certainly a = coerce [(a, 1 :: Rational)] 

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
  where fmap f (Dist aps) = coerce [(f a, p) | (a,p) <- aps]
 
-- Distributions are also applicative...
instance Applicative Dist
  where pure = certainly
        Dist fps <*> Dist aqs = 
          coerce [(f a, p*q) | (f,p) <- fps, (a,q) <- aqs]

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
binomial n (P p) = coerce [(k, P (nChoose k * p^k * (1 - p)^(n-k))) | k <- [0..n]]
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
    Dist aps >>= f = coerce [(b,p*q) | (a,p) <- aps, (b,q) <- masses (f a)]

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
-- This is slow for lookup because we just use a list. We should use a Map
-- instead for O(log n) lookup.
cdf :: Dist a -> [(a,Rational)]
cdf d = [(a,unProb p) | (a,p) <- zip (values d) totalProbs]
  where totalProbs = drop 1 (scanl (+) 0 (probs d))

-- Sample by inverting the cumulative distribution
-- Sampling. Slow for now.
sample :: Rational -> Dist a -> a
sample r d = if r == 1 then last (values d)
                       else head (drop (sampleLength d) (values d))
  where sampleLength = length . takeWhile (r >=) . map snd . cdf  


-- Monty Hall, with more detail
data Door = A | B | C deriving (Eq,Ord,Show)

-- Doors contestant can choose from
doors :: [Door]
doors = [A,B,C]

-- The game state
data State = State { 
  prize  :: Maybe Door, 
  chosen :: Maybe Door,
  opened :: Maybe Door 
} deriving (Eq,Ord,Show)

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

-- Class of distributions supporting simulation 
class Sim f where
  sequ :: (Ord a) => [a -> f a] -> a -> f a

-- Pure distributions can be simulated as long as the point type is
-- totally ordered
instance Sim Dist where
  sequ = let op f g = norm . (f >=> g) 
         in foldr op pure 

-- Randomized distributions can also be simulated
instance Sim RDist where
  sequ = let op f g = RDist . fmap norm . unRDist . (f >=> g)
         in foldr op pure

-- Run any simulation for a fixed number of transitions
(.*) :: (Sim f, Ord a) => Int -> (a -> f a) -> a -> f a
n .* afa | n < 0     = afa 
         | otherwise = sequ (replicate n afa)

-- Starting with a strategy, we can describe the whole game
game :: Strategy -> Dist Outcome 
game strat = fmap winOrLose finalState 
  where finalState  = sequ [hidePrize,chooseDoor,openDoor,strat] gameStart
        winOrLose s = if chosen s == prize s then Win else Lose

-- Sometimes we need true pseudorandom samples
newtype Rand a = Rand { unRand :: IO a } deriving (Functor,Applicative,Monad)

-- Random rationals between 0 and 1
randomProb :: Rand Rational 
randomProb = coerce $ do
    denom <- randomIO :: IO Integer 
    if denom <= 0 
      then coerce randomProb
      else do
        num <- randomIO
        if num < 0 || num > denom
          then coerce randomProb
          else return (num % denom)

-- Random sampling from any pure distribution
rSample :: Dist a -> Rand a
rSample d = do
    r <- randomProb
    return (sample r d)

-- A type for randomized (non-pure) distributions
newtype RDist a = RDist { unRDist :: Rand (Dist a) }

-- We can build a randomized distribution from any list of random samples
rDist :: [Rand a] -> RDist a
rDist = RDist . fmap uniform . sequenceA 

-- Lift any pure distribution to a randomized distribution using n samples
rSampleDist :: (Ord a) => Int -> Dist a -> RDist a
rSampleDist n d = rDist (replicate n rSampleD)
  where d'       = norm d 
        rSampleD = rSample d' 

-- Lift any pure state transition to a randomized state 
-- transition using n samples
rSampleTrans :: (Ord a) => Int -> (a -> Dist a) -> (a -> RDist a)
rSampleTrans n f = rSampleDist n . f 

-- Helper for moving IO actions outside distribution
seqDist :: Dist (Rand a) -> Rand (Dist a)
seqDist (Dist fas) = coerce (sequenceA [fmap (\a -> (a,p)) fa | (fa,p) <- fas])

-- Randomized distributions are also functors
instance Functor RDist where
  fmap f (RDist rda) = coerce (fmap (fmap f) rda)

-- Flatten randomized distributions 
joinR :: RDist (RDist a) -> RDist a
joinR = let coerce1 = coerce :: RDist (RDist a) -> Rand (Dist (Rand (Dist a))) 
        in  coerce . fmap join . join . fmap seqDist . coerce1

-- Randomized distributions are applicatives since they are monads
instance Applicative RDist where
  (<*>) = ap
  pure  = return
  
-- Randomized distributions are also monads
instance Monad RDist where
  rda >>= f = (joinR . fmap f) rda
  return    = RDist . return . certainly

-- Examples of randomized distributions:
-- unRDist (rSampleDist 10 (uniform [1,2,3]))
-- unRDist (rSampleDist 1000 coin)
-- unRDist (rSampleDist 6000 die)

-- Sampling from any randomized distribution
(?) :: Event a -> RDist a -> Rand Probability
e ? rda = do
  da <- unRDist rda
  return (e ?? da)

-- We can approximate event probabilities via random sampling. Examples: 
-- (==0)     ? (rSampleDist 1000 coin)
-- (==[1,1]) ? (rSampleDist 1000 dice)


-- Tree Growth simulation
type Height = Int

-- Trees are either alive, hit by lightning (standing), or fallen
data Tree = Alive Height | Hit Height | Fallen deriving (Eq,Ord,Show)

-- Simulate a year's growth for any Tree that is Alive
grow :: Tree -> Dist Tree
grow (Alive h) = fmap (Alive . (+h)) baseDist
  where baseDist = fmap (+1) (binomial 4 (prob (1 % 2)))
grow otherwise = certainly otherwise

-- Simulate a Tree being hit by lightning, retaining its height
hit :: Tree -> Dist Tree
hit (Alive h) = certainly (Hit h)
hit otherwise = certainly otherwise

-- Simulate a Tree falling, losing its height 
fall :: Tree -> Dist Tree
fall _ = certainly Fallen 

-- We need to combine these possible transitions into a single transition
unfoldTrans :: Dist (Tree -> Dist Tree) -> Tree -> Dist Tree
unfoldTrans df t = join (df <*> certainly t)

-- Define a simulation where growth probability is 90%, hit probability is
-- 4%, and fall probability is 6%
oneYear :: Tree -> Dist Tree
oneYear = unfoldTrans (enumDist [(grow, 9 % 10),(hit, 4 % 100),(fall, 6 % 100)])

-- Example: oneYear (Alive 0)
--          (5 .* oneYear) (Alive 0)

-- rSampleTrans 10 (5 .* oneYear) (Alive 0)

