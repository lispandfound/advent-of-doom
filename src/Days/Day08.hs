module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Util.Util as U
import qualified Data.Map as Map
import qualified Data.Set as Set
import Util.Coordinates
import Util.Parsers

import qualified Program.RunDay as R (runDay, Day)
import Data.Char (isAlphaNum)
import Data.Attoparsec.Text hiding (takeWhile)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  _a <- coordinateParser (pure <$> satisfy isAlphaNum <|> "." $> pure '.' <|> anyChar $> Nothing) 0
  bounds <- liftMaybe $ U.mapBoundingBox _a
  return $ Input (Map.filter (/= '.') _a) bounds

------------ TYPES ------------
data Input = Input {antennas :: CoordinateMap Char, bounds :: BoundingBox} deriving (Show)

type OutputA = Int

type OutputB = Int

antinode :: (Int, Int) -> (Int, Int) -> (Int, Int)
antinode (a, b) (c, d) = (a + 2 * dx, b + 2 * dy)
  where dx = c - a
        dy = d - b

antinodes :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodes (a, b) (c, d) = ray a b dx dy
  where dx = c - a
        dy = d - b

------------ PART A ------------
partA :: Input -> OutputA
partA (Input {antennas = m, bounds = bounds}) = Set.size . Set.fromList . filter (inBoundingBox bounds) $ do
      (ptA, freqA) <- points
      (ptB, freqB) <- points
      guard $ ptA /= ptB && freqA == freqB
      return $ antinode ptA ptB
  where points = Map.toList m

------------ PART B ------------
partB :: Input -> OutputB
partB (Input {antennas = m, bounds = bounds}) = Set.size . Set.fromList $ do
      (ptA, freqA) <- points
      (ptB, freqB) <- points
      guard $ ptA /= ptB && freqA == freqB
      takeWhile (inBoundingBox bounds) $ antinodes ptA ptB
  where points = Map.toList m
