module Days.Day24 (runDay) where

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
import Data.Functor (($>))
import Util.Pair
import Util.Util (hammerN)
import Debug.Trace (traceShowId)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = directions `sepBy` endOfLine
  where
    directions = sum <$> many1 direction
    direction =
      choice
        [ "se" $> Pair (0, 1),
          "ne" $> Pair (1, -1),
          "nw" $> Pair (0, -1),
          "sw" $> Pair (-1, 1),
          "e" $> Pair (1, 0),
          "w" $> Pair (-1, 0)
        ]

------------ TYPES ------------
type Input = [Pair Int]

type OutputA = Int

type OutputB = Int

floorSet :: [Pair Int] -> Set (Pair Int)
floorSet = Map.keysSet . Map.filter ((== 1) . (`mod` 2)) . U.freq

------------ PART A ------------
partA :: Input -> OutputA
partA = Set.size . floorSet

------------ PART B ------------
partB :: Input -> OutputB
partB = Set.size . hammerN 100 go . floorSet
  where go fs = Set.filter (rule fs) . neighbours $ fs
        neighbourSteps = [Pair (0, 1), Pair (1, -1), Pair (0, -1), Pair (-1, 1), Pair (1, 0), Pair (-1, 0)]
        neighbours = U.setConcatMap (\p -> Set.fromList $ p:map (p +) neighbourSteps)
        rule fs p
          | not live && neighbours == 2 = True
          | live && (neighbours == 0 || neighbours > 2) = False
          | otherwise = live
          where
            live = p `Set.member` fs
            neighbours = U.count (`Set.member` fs) . map (p +) $ neighbourSteps
