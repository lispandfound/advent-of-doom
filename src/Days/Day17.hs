module Days.Day17 (runDay) where

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
import Util.Parsers (coordinateParser)
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Bifunctor (Bifunctor(first))
import Util.Util (hammerN)
import Data.Functor
import Control.Applicative
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Set.fromList . map (uncurry (,,0) . fst) . Map.toList <$> coordinateParser (("#" $> pure True) <|> (anyChar $> Nothing)) 0

------------ TYPES ------------
type Input = Set (Int, Int, Int)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA m = Set.size $ hammerN 6 evolve m
  where
    thd (_,_,c) = c
    fst (a,_,_) = a
    snd (_,b,_) = b
    boundingCube m = (minimum . map fst . Set.toList $ m
                     , minimum . map snd . Set.toList $ m
                     , minimum . map thd . Set.toList $ m
                     , maximum . map fst . Set.toList $ m
                     , maximum . map snd . Set.toList $ m
                     , maximum . map thd . Set.toList $ m)
    rule test = case U.count test [(dx, dy, dz) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], (dx, dy, dz) /= (0, 0, 0)] of
                  3 -> True
                  2 -> test (0, 0, 0)
                  _ -> False
    evolve m = foldr (\p m' -> if rule (lookuprel p) then
                                 Set.insert p m'
                                 else
                                 Set.delete p m') m [(x, y, z) | x <- [x0 - 1..x1 + 1], y <- [y0 - 1..y1 + 1], z <- [z0 - 1..z1 + 1]]
      where (x0, y0, z0, x1, y1, z1) = boundingCube m
            lookuprel (x, y, z) (dx, dy, dz) = (x + dx, y + dy, z + dz) `Set.member` m






------------ PART B ------------
partB :: Input -> OutputB
partB m = Set.size . hammerN 6 evolve . Set.fromList . map (\(x, y, z) -> (x, y, z, 0)) . Set.toList $ m
  where
    thd (_,_,c, _) = c
    fst (a,_,_, _) = a
    snd (_,b,_, _) = b
    fth (_, _, _, d) = d
    boundingCube m = (minimum . map fst . Set.toList $ m
                     , minimum . map snd . Set.toList $ m
                     , minimum . map thd . Set.toList $ m
                     , maximum . map fst . Set.toList $ m
                     , maximum . map snd . Set.toList $ m
                     , maximum . map thd . Set.toList $ m
                     , minimum . map fth . Set.toList $ m
                     , maximum . map fth . Set.toList $ m)
    rule test = case U.count test [(dx, dy, dz, dw) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dw <- [-1..1], (dx, dy, dz, dw) /= (0, 0, 0, 0)] of
                  3 -> True
                  2 -> test (0, 0, 0, 0)
                  _ -> False
    evolve m = foldr (\p m' -> if rule (lookuprel p) then
                                 Set.insert p m'
                                 else
                                 Set.delete p m') m [(x, y, z, w) | x <- [x0 - 1..x1 + 1], y <- [y0 - 1..y1 + 1], z <- [z0 - 1..z1 + 1], w <- [w0 - 1 .. w1 + 1]]
      where (x0, y0, z0, x1, y1, z1, w0, w1) = boundingCube m
            lookuprel (x, y, z, w) (dx, dy, dz, dw) = (x + dx, y + dy, z + dz, w + dw) `Set.member` m
