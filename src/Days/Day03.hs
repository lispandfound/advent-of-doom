module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import Data.List as List
import qualified Data.Vector as Vec
import qualified Util.Util as U
import qualified Util.Parsers as Up

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Functor
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Up.coordinateParser (choice ["#" $> pure Tree, anyChar $> Nothing]) 0

------------ TYPES ------------
data Tree = Tree deriving (Show)
type Input = Map (Int, Int) Tree

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA m = U.count (`Map.member` m) $ map coordSequence [0..uy]
  where (_, ux, _, uy) = U.mapBoundingBox m
        coordSequence i = (3 * i `mod` (ux + 1), i)


------------ PART B ------------
partB :: Input -> OutputB
partB m = countTrees (path 1 1) * countTrees (path 3 1) * countTrees (path 5 1) * countTrees (path 7 1) * countTrees (path 1 2)
  where
    (_, ux, _, uy) = U.mapBoundingBox m
    countTrees path = U.count (`Map.member` m) . List.takeWhile ((<= uy) . snd) . map path $ [0..]
    path dx dy i = (dx * i `mod` (ux + 1), dy * i)
