module Days.Day05 (runDay) where

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
import Control.Applicative
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =  ((\row col -> 8 * row + col) <$> row <*> col) `sepBy` endOfLine
  where row = foldl (\n d -> 2*n + d) 0 <$> count 7 (high 'B' <|> low 'F')
        col = foldl (\n d -> 2*n + d) 0 <$> count 3 (high 'R' <|> low 'L')
        high c = char c *> pure 1
        low c = char c *> pure 0

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA  = maximum

------------ PART B ------------
partB :: Input -> OutputB
partB inp = maybe (-1) (\(l, _) -> l + 1) . find (\(l, r) -> r - l == 2) $ zip ids (tail ids)
  where ids = sort inp
