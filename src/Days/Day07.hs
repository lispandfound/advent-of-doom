module Days.Day07 (runDay, intConcat) where

{- ORMOLU_DISABLE -}
import qualified Util.Util as U
import Util.Parsers
import Data.List as List

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Functor (($>))
import Control.Lens
intConcat x y = x * (10 ^ i) + y{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = linesOf $ (,) <$> decimal <*> (": " >> decimal `sepBy1` " ")

------------ TYPES ------------
type Input = [(Int, [Int])]

type OutputA = Int

type OutputB = Int


-- Yes, I know that this is x * (floor (log10 y) + 1) + y,
-- but this is considerably faster for the inputs given.
intConcat :: Int -> Int -> Int
intConcat x y
  | y < 10 = x * 10 + y
  | y < 100 = x * 100 + y
  | y < 1000 = x * 1000 + y
  | otherwise = x * 10000 + y

------------ PART A ------------
partA :: Input -> OutputA
partA = List.sum . map fst . filter (\(target, terms) -> target `List.elem` go target (reverse terms))
  where
    -- Slow enough that we can brute force it!
    -- Indeed, this is the dumbest way to do it. Much faster approach below!
    -- I just do it like this because I love using Haskell lists for built-in determinism :)
    go :: Int -> [Int] -> [Int]
    go _ [x] = [x]
    go target (x:xs) = do
          f <- [(*), (+)]
          rest <- go target xs
          guard $ rest <= target
          return $ f x rest

-- NOTE: I am quite aware one could do this in reverse and test divisibility, subtractability (is that even a word?) and suffixes for maximum pruning potential.
-- But this partB already executes in 3ms anyway on my laptop with no parallelisation.
-- This is faster than many of the Rust solutions I spotted on adventofcode. If they get to boast about their speed without doing anything special,
-- I'm allowed to boast about speed without doing anything either!


------------ PART B ------------
partB :: Input -> OutputB
partB = List.sum . map fst . filter (\(target, terms) -> go target (List.head terms) (List.tail terms))
  where
    go :: Int -> Int -> [Int] -> Bool
    go target work [] = work == target
    -- What makes this so much faster than the above is that we do not create a list in-memory as we go along.
    -- Moreover two optimisations apply here:
    -- 1. Logical or, which in haskell is a prune equivalent to the following loop due to lazy evaluation:
    --    for rest in [f work x for f in [(*), (+), intConcat]]:
    --        if go target w xs:
    --            return True
    --    return False
    -- 2. The filter that acts like "guard" in partA
    go target work (x:xs) = or . map (\w -> go target w xs) . filter (<= target) . map (\f -> f work x) $ [(*), (+), intConcat]
