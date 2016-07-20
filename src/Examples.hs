{-# language GeneralizedNewtypeDeriving #-} 
{-# language BangPatterns #-}
{-# language RankNTypes #-}
module Examples 
where

import Data.Map.Strict as M (fromListWith,toList)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Function
import Data.Ratio
import Control.Applicative
import Control.Monad
import System.Random 
import Data.Coerce
import Test.QuickCheck hiding (sample,choose)

-- Type for Numerics so we can take advantage of custom typeclasses 
newtype Numeric = Numeric { unNumeric :: Rational }
  deriving (Show,Eq,Ord,Num,Fractional,Real)

-- At minimum, every Numeric we work with must be a monoid
instance Monoid Numeric where
  mempty        = toNumeric 0
  a `mappend` b = Numeric ((unNumeric a) + (unNumeric b))

-- Any rational is an acceptable Numeric, and that's enough for now
toNumeric :: Rational -> Numeric
toNumeric = Numeric 

-- So we don't have to worry about testing equalities of floats
newtype Probability = P { unProb :: Numeric }
  deriving (Show,Eq,Ord,Num,Fractional,Real)

-- Treat a rational as a probability
prob :: Rational -> Probability
prob p = if p >= 0 && p <= 1 then P (toNumeric p) else P (toNumeric 0)

-- Simple representation of a distribution
newtype Dist a = Dist { unDist :: [(a,Probability)] }

-- Distributions are comparable for equality when their elements are
instance Eq a => Eq (Dist a) where
  (==) = (==) `on` masses

-- Distributions over monoids are also monoids
instance Monoid a => Monoid (Dist a) where
  mappend = liftA2 (<>) 
  mempty  = certainly mempty 

-- Get list of masses underlying distribution
masses :: Dist a -> [(a,Probability)]
masses = unDist 

-- Get just the list of values from a distribution
values :: Dist a -> [a]
values = map fst . masses

-- Get just the list of probabilities from a distribution
probs :: Dist a -> [Probability]
probs = map snd . masses

-- We can show distributions as long as we can simplify them
instance (Ord a, Show a) => Show (Dist a) where
  show = show . masses . simplify 

-- Total putative probability represented by list of masses
totalProb :: [(a,Probability)] -> Numeric 
totalProb = unProb . sum . map snd

-- Determine if a list of masses represents a distribution
isDist :: [(a,Probability)] -> Bool
isDist d = totalProb d == 1

-- Simplify the underlying representation of a distribution by
-- combining masses for the same element. Mathematically this is a no-op.
simplify :: Ord a => Dist a -> Dist a
simplify = Dist . toList . M.fromListWith (+) . masses 

-- Construct a uniform distribution from a finite list. We have no 
-- notion of equality so there cannot be "duplicates" in the result
uniform :: Monoid a => [a] -> Dist a
uniform [] = mempty
uniform as = Dist [(a,p) | a <- as]
  where p = P (toNumeric (1 % (toInteger (length as))))

-- Given explicitly enumerated list of elements and probabilities, produce
-- a distribution
enumDist :: Monoid a => [(a,Rational)] -> Dist a
enumDist ars = if isDist putative then Dist putative else mempty
  where putative = [(a,prob r) | (a,r) <- ars]

-- An unweighted die is a uniform distribution
die :: Dist Numeric 
die = uniform [1,2,3,4,5,6] 

-- An unbiased coin is a uniform distribution
coin :: Dist Numeric 
coin = uniform [0,1] 

-- An unbiased set of numeric functions 
funcs :: Dist (Endo Int)
funcs = uniform [Endo (+1), Endo (+2), Endo (+3)] 

-- Construct a distribution representing a certain occurrence
certainly :: a -> Dist a
certainly a = Dist [(a, 1 :: Probability)] 

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
dice :: Int -> Dist [Numeric]
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
binomial :: Int -> Probability -> Dist Numeric 
binomial n p = Dist [(f (toInteger k), nChoose k * p^k * (1 - p)^(n-k)) | k <- [0..n]]
  where nChoose k = P (f (n `choose` k))
        f         = toNumeric . fromIntegral 

-- Choose an element from a list, removing it 
selectOne :: (Eq a, Monoid a) => [a] -> Dist (a,[a])
selectOne []     = certainly (mempty,[]) 
selectOne (a:as) = uniform [(a, delete a as) | a <- as] 

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
selectMany :: (Eq a, Monoid a) => Int -> [a] -> Dist ([a],[a])
selectMany _ [] = certainly ([],[])
selectMany n as@(a:_) | n > 0 = do
    (a,rs)  <- selectOne as
    (bs,ss) <- selectMany (n-1) rs
    return (a:bs,ss)
selectMany _ _ = certainly ([],[])


-- The Monty Hall problem
data Outcome = Win | Lose deriving (Show,Eq)
instance Monoid Outcome where
  Win  `mappend` _   = Win
  Lose `mappend` x   = x 
  mempty             = Lose

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
cdf :: Dist a -> [(a,Numeric)]
cdf d = [(a,unProb p) | (a,p) <- zip (values d) totalProbs]
  where totalProbs = drop 1 (scanl (+) 0 (probs d))

-- Sample by inverting the cumulative distribution
sample :: Monoid a => Dist a -> Probability -> a
sample d p = if p == 1 then headDef mempty (reverse (values d))
                       else headDef mempty (drop (sampleLength d) (values d))
  where sampleLength = length . takeWhile (\n -> p >= P n) . map snd . cdf  
        headDef d []    = d
        headDef _ (a:_) = a 

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

-- Game states form a monoid representing defined positions
instance Monoid State where
  mappend s1 s2 
    = State {prize  = prize  s1 <|> prize s2, 
             chosen = chosen s1 <|> chosen s2, 
             opened = opened s1 <|> opened s2} 
  mempty = gameStart 

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
openDoor s = uniform [ s { opened = Just d } | d <- doors \\ catMaybes [chosen s, prize s] ] 

-- Contestant strategy of switching to the unchosen, unopened door 
switchDoor :: State -> Dist State
switchDoor s = uniform [ s { chosen = Just d } | d <- doors \\ catMaybes [chosen s, opened s] ] 

-- Contestant strategy of staying with the chosen door
stay :: State -> Dist State
stay = certainly 

-- Some type synonyms
type Strategy = State -> Dist State 
type StateTransition = State -> Dist State 

-- Class of distributions supporting simulation 
class Sim f where
  sequ :: Ord a => [a -> f a] -> a -> f a

-- Pure distributions can be simulated as long as the point type is
-- totally ordered
instance Sim Dist where
  sequ = let op f g = simplify . (f >=> g) 
         in foldr op pure 

-- Randomized distributions can also be simulated
instance Sim RDist where
  sequ = let op f g = mapDist simplify . (f >=> g)
         in foldl' op pure 

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
randomProb :: Rand Probability 
randomProb = do
    denom <- Rand randomIO 
    if denom <= 0 
      then randomProb
      else do
        num <- Rand randomIO 
        if num < 0 || num > denom
          then randomProb
          else return $ (P . toNumeric) (num % denom)

-- Random sampling from any pure distribution
rSample :: Monoid a => Dist a -> Rand a
rSample d = do
  r <- randomProb
  return (sample d r)

-- A type for randomized (non-pure) distributions
newtype RDist a = RDist { unRDist :: Rand (Dist a) }

-- Treat any randomized distribution as an ordinary distribution plus some I/O
runRDist :: RDist a -> IO (Dist a)
runRDist = unRand . unRDist 

-- We can build a randomized distribution from any list of random samples
rDist :: Monoid a => [Rand a] -> RDist a
rDist []  = RDist (return mempty) 
rDist ras = RDist (fmap uniform (sequenceA ras))

-- Lift any pure distribution to a randomized distribution using n samples
rSampleDist :: Monoid a => Int -> Dist a -> RDist a
rSampleDist n d | n <= 0 = RDist (pure d)
                | otherwise = rDist (replicate n (rSample d))

-- Lift any pure state transition to a randomized state 
-- transition using n samples
rSampleTrans :: Monoid a => Int -> (a -> Dist a) -> (a -> RDist a)
rSampleTrans n f = rSampleDist n . f

-- Helper for moving IO actions outside distribution
seqDist :: Dist (Rand a) -> Rand (Dist a)
seqDist dras = Dist <$> (sequenceA [fmap (\a -> (a,p)) ra | (ra,p) <- masses dras])

-- Randomized distributions are also functors
instance Functor RDist where
  fmap f (RDist rda) = RDist (fmap (fmap f) rda)

-- Sometimes we just want to map over the underlying distribution
mapDist :: (Dist a -> Dist b) -> RDist a -> RDist b
mapDist f (RDist rda) = RDist (fmap f rda)

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
type Height = Numeric 

-- Trees are either alive, hit by lightning (standing), or fallen
data Tree = Alive !Height | Hit !Height | Fallen deriving (Eq,Ord,Show)
instance Monoid Tree where
  x `mappend` y = x
  mempty        = Fallen

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

