--inspired from the original source code pfp-jun06, developed by Martin Erwig and Steve Kollmansberger in "A Probabilistic Functional Programming Library for Haskell" (https://web.engr.oregonstate.edu/~erwig/pfp/)

module Library where

import Data.List (sort, sortBy, transpose, groupBy)
import Control.Monad (MonadPlus(..))
import Data.Function (on)

--import for portable randomness
import Data.Time.Clock.POSIX (getPOSIXTime)


-- Generates a pseudo-random float between 0 and 1 using a linear congruential formula
--(Replaces external 'random' package in original authors code)
lcg :: Int -> Float
lcg seed = fromIntegral ( (1103515245 * seed + 12345) `mod` 2147483647 ) / 2147483647.0


--from Probability.hs: checks if a specific value exists within a provided list of items
oneOf :: Eq a => [a] -> a -> Bool
oneOf xs x = x `elem` xs

-- from Dice.hs: calculates the total probability of all outcomes in a distribution that satisfy a given predicate.
infix 8 ??
(??) :: (a -> Bool) -> Dist a -> Probability
p ?? d = P . sum . map snd . filter (p . fst) . unD $ d

-- From Visualize.hs: type class for types that can be converted into a floating-point number
class ToFloat a where
  toFloat :: a -> Float

instance ToFloat Float where toFloat = id


------------------------------------------------------------------------------
--from ListUtils.hs (Authors' Code)
------------------------------------------------------------------------------

--Wraps a single value into a list containing only that element.
singleton :: a -> [a]
singleton x = [x]

-- Required by 'norm' to combine probabilities of identical outcomes
accumBy :: Num b => (a -> a -> Bool) -> [(a,b)] -> [(a,b)]
accumBy f ((x,p):ys@((y,q):xs)) | f x y     = accumBy f ((x,p+q):xs)

                                | otherwise = (x,p):accumBy f ys
accumBy _ xs = xs

------------------------------------------------------------------------------
--from show.hs (Authors' Code)
------------------------------------------------------------------------------

rep :: Int -> a -> [a]
rep n x = take n (repeat x)

--formats strings for aligned table-like output in the console
showR :: Show a => Int -> a -> String
showR n x = rep (n - length s) ' ' ++ s
            where s = show x

------------------------------------------------------------------------------
--from probability.hs (Authors code)
------------------------------------------------------------------------------

type ProbRep = Float
newtype Probability = P ProbRep
newtype Dist a = D {unD :: [(a, ProbRep)]}

-- The Authors' original Monad implementation for probability distributions improved using latest language
instance Monad Dist where
    d >>= f = D [(y, q*p) | (x, p) <- unD d, (y, q) <- unD (f x)]


------------------------------------------------------------------------------
-- new code added for GHC Compliance as Modern Haskell requires Functor and Applicative for every Monad
--these instances allow the authors' 2006 code to run on 2026 compilers.
------------------------------------------------------------------------------

instance Functor Dist where
   fmap f (D d) = D [(f x, p) | (x, p) <- d]


instance Applicative Dist where
  pure x = D [(x, 1)]
  (D fs) <*> (D xs) = D [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

------------------------------------------------------------------------------
--from Probability.hs (Authors code for normalization and printing)
------------------------------------------------------------------------------

--merges same outcomes into one: e.g., [(Win, 0.5), (Win, 0.2)] -> [(Win, 0.7)]
norm :: Ord a => Dist a -> Dist a
norm (D d) = D $ accumBy (==) $ sort d

--authors' logic used to display probabilities as rounded percentages
instance Show Probability where
  show (P p) = showR 4 (round (p * 100)) ++ "%"

instance (Ord a, Show a) => Show (Dist a) where
  show (D xs) = concatMap (\(x, p) -> showR 12 x ++ ": " ++ show (P p) ++ "\n") (unD (norm (D xs)))

------------------------------------------------------------------------------
--from Probability.hs (Authors code for Math & Simulation)
------------------------------------------------------------------------------

--the 'Expected' class used to calculate mathematical averages
class Expected a where
  expected :: a -> Float

instance Expected Float where expected = id

instance Expected a => Expected (Dist a) where
  expected = sum . map (\(x, p) -> expected x * p) . unD

--generators for creating specific distributions
enum :: [ProbRep] -> [a] -> Dist a
enum ps xs = D $ zip xs ps

--from Probability.hs: A simple binary choice helper
choose :: ProbRep -> a -> a -> Dist a
choose p x y = enum [p, 1-p] [x, y]

uniform :: [a] -> Dist a
uniform xs = let p = 1.0 / fromIntegral (length xs) in D [(x, p) | x <- xs]

--selection engine to extract a single value from a distribution
selectP :: Dist a -> ProbRep -> a
selectP (D d) p = scanP d p
  where scanP ((x, q):ps) p' | p' <= q || null ps = x
                             | otherwise          = scanP ps (p' - q)

--code changed to use the internal LCG and system time instead of System.Random
pick :: Dist a -> IO a
pick d = do
    time <- getPOSIXTime
    let seed = round (time * 1000)
    return (selectP d (lcg seed))

--multi-step transition logic (as seen in the Monty Hall code by authors)
type Trans a = a -> Dist a

sequ :: Monad m => [a -> m a] -> a -> m a
sequ = foldl (>@>) return
  where f >@> g = (>>= g) . f


