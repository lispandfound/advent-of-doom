module Days.Day04 where

{- ORMOLU_DISABLE -}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, anyChar)
import Data.Void
import Data.Functor (($>))
import Data.Bifunctor
import Util.Parsers
import Util.Coordinates
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

data Letter = X | M | A | S deriving (Show, Eq, Generic, Ord)

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser (("X" $> pure X) <|> ("M" $> pure M) <|> ("A" $> pure A) <|> ("S" $> pure S) <|> (anyChar $> Nothing)) 0

type Input = CoordinateMap Letter

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA g = length $ do
    ((x, y), l) <- Map.toList g
    guard (l == X)
    (dx, dy) <- [(dx1, dy1) | dx1 <- [-1, 0, 1], dy1 <- [-1, 0, 1], (dx1, dy1) /= (0, 0)]
    guard $ take 4 (rayTillNothing g x y dx dy) == [X, M, A, S]
    return ()

------------ PART B ------------
partB :: Input -> OutputB
partB = Map.size . convolve xmas
  where
    xmas get = do
        a <- get 0 0
        guard $ a == A
        tl <- get (-1) 1
        br <- get 1 (-1)
        guard $ sort [tl, br] == [M, S]
        bl <- get (-1) (-1)
        tr <- get 1 1
        guard $ sort [bl, tr] == [M, S]
        return ()
