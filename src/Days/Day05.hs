module Days.Day05 (runDay) where

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
import Data.Tree (Tree(..))

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Functor (($>))
import Data.Bifunctor
import Util.Parsers
import Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.AdjacencyMap.Algorithm (isTopSortOf, Cycle, topSort)
{- ORMOLU_ENABLE -}

type Graph = AM.AdjacencyMap Int

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (Input . AM.edges <$> linesOf rule) <*> (endOfLine >> endOfLine >> linesOf decimals)
  where
    rule = around decimal "|"
    decimals = decimal `sepBy1` ","

------------ TYPES ------------
data Input = Input
    { rules :: Graph
    , updates :: [[Int]]
    }
    deriving (Show, Eq)

type OutputA = Int

type OutputB = Int

isTopSortPrefOf :: [Int] -> Graph -> Bool
isTopSortPrefOf xs = (xs `isTopSortOf`) . AM.induce (`List.elem` xs)

topSortPrefOf :: [Int] -> Graph -> Maybe [Int]
topSortPrefOf xs = either (const Nothing) pure . topSort . AM.induce (`List.elem` xs)

median :: [Int] -> Int
median l = l !! ((length l - 1) `div` 2)

------------ PART A ------------
partA :: Input -> OutputA
partA (Input{rules = graph, updates = updatesList}) = (List.sum . map median . filter (`isTopSortPrefOf` graph)) updatesList

------------ PART B ------------
partB :: Input -> OutputB
partB (Input{rules = graph, updates = updatesList}) = List.sum . mapMaybe (fmap median . (`topSortPrefOf` graph)) . filter (not . (`isTopSortPrefOf` graph)) $ updatesList
