module Days.Day06 (runDay) where

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
inputParser = group `sepBy` (endOfLine *> endOfLine)
  where group = ((Set.fromList <$> many1 letter) `sepBy` space)

------------ TYPES ------------
type Input = [[Set Char]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map (Set.size . Set.unions)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map (Set.size . intersections)
  where intersections = foldl1 Set.intersection
