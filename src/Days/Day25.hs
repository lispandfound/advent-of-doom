module Days.Day25 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as List
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
import Util.Modular (Modular)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> decimal <*> (endOfLine *> decimal)

------------ TYPES ------------
type Input = (Integer, Integer)

type OutputA = Integer

type OutputB = String


modulus :: Integer
modulus = 20201227

dlog :: Integer -> Integer -> Integer -> Maybe Integer
dlog a b n = fmap fromIntegral . elemIndex b . List.take (fromInteger n - 1) $ iterate ((`mod` n) . (a *)) 1

------------ PART A ------------
partA :: Input -> OutputA
partA (cardKey, doorKey) = doorKey ^ cardLoop `mod` modulus
  where Just cardLoop = dlog 7 cardKey modulus
        Just doorLoop = dlog 7 doorKey modulus

------------ PART B ------------
partB :: Input -> OutputB
partB = const "This one was free!"
