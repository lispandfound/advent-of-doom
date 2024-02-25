module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
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
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Maybe Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = fmap head . find (\(x:xs) -> not . isJust $ U.findTargetSum x xs ) . reverse . U.windows (n + 1) . reverse
  where n = 25

------------ PART B ------------
partB :: Input -> OutputB
partB inp = maximum s + minimum s
  where
    s = U.slice left right inp
    Just (left, right) = U.findJust (findSumWithLeft target inp) [0..length inp - 1]
    target = 29221323
    findSumWithLeft n l i = if head partials == n then Just $ (i, i + length partials - 1) else Nothing
      where partials = reverse . L.takeWhile (<= n) . scanl1 (+) . drop i $ l
