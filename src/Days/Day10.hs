module Days.Day10 (runDay) where

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
inputParser = Set.fromList <$> decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = Set Int

type OutputA = Int

type OutputB = Integer

------------ PART A ------------
partA :: Input -> OutputA
partA inp = U.count (== 3) distribution * U.count (== 1) distribution
  where devices = Set.fromList [0, Set.findMax inp + 3] `Set.union` inp
        distribution = U.mapAdjacent (-) . Set.toDescList $ devices

------------ PART B ------------
partB :: Input -> OutputB
partB inp = product . map ((`U.orderedPartitions` 3) . toInteger . length) . U.chunksByPredicate (==1) $ distribution
  where devices = Set.fromList [0, Set.findMax inp + 3] `Set.union` inp
        distribution = U.mapAdjacent (-) . Set.toDescList $ devices
