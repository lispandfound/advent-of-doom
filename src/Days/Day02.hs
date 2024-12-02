module Days.Day02 (runDay) where

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
import Util.Parsers

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Functor (($>))
import Data.Bifunctor
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = linesOf $ decimal `sepBy1` " "

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

pairwiseDifference :: [Int] -> [Int]
pairwiseDifference = zipWith (-) <*> List.tail

safe :: [Int] -> Bool
safe = all ((<= 3) . abs) `and` ((`List.elem` [Set.singleton (-1), Set.singleton 1]) . Set.fromList . map signum)
  where
    and = liftA2 (&&)

------------ PART A ------------
partA :: Input -> OutputA
partA = U.count (safe . pairwiseDifference)

------------ PART B ------------
partB :: Input -> OutputB
partB = U.count (any (safe . pairwiseDifference) . allButOne)
  where
    allButOne l = map (uncurry (<>) . second (drop 1) . flip splitAt l) [0 .. length l]
