module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy decimal endOfLine

------------ TYPES ------------
type Input = [Integer]

type OutputA = Maybe Integer

type OutputB = OutputA


findTargetSum :: Integer -> [Integer] -> Maybe Integer
findTargetSum target = fmap fst . (U.!!? 0) . Map.toList . Map.filter (> 1) . U.freq . map (\x -> if x < target `div` 2 then target - x else x)

target :: Integer
target = 2020

------------ PART A ------------
partA :: Input -> OutputA
partA = fmap prodFactor . findTargetSum target
  where
    prodFactor f = f * (target - f)

------------ PART B ------------
partB :: Input -> OutputB
partB inp = fmap productFactors . find (isJust . snd) . map (\x -> (x, findTargetSum (target - x) inp)) $ inp
  where productFactors (x, Just y) = x * y * (target - x - y)
