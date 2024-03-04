module Days.Day11 (runDay) where

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
import qualified Util.Coordinates as Uc
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
inputParser =  Up.coordinateParser (choice ["L" $> pure Empty
                                           , "#" $> pure Occupied
                                           , "." $> pure Floor]) 0



data Cell = Empty | Floor | Occupied deriving (Show, Eq)

------------ TYPES ------------
type Input = Uc.CoordinateMap Cell

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = U.count ((== Occupied) . snd) . Map.toList . U.hammer (Uc.convolve Floor kernel)
  where
    rule Empty 0 = Occupied
    rule Occupied x = if x >= 4 then Empty else Occupied
    rule s _ = s
    kernel get = rule (get 0 0) (U.count (== Occupied) [ get (-1) (-1)
                                                         , get (-1) 0
                                                         , get (-1) 1
                                                         , get 0 (-1)
                                                         , get 0 1
                                                         , get 1 (-1)
                                                         , get 1 0
                                                         , get 1 1
                                                         ])



------------ PART B ------------
partB :: Input -> OutputB
partB m = U.count ((== Occupied) . snd) . Map.toList . U.hammer update $ m
  where
    box = U.mapBoundingBox m
    rule Floor _ = Floor
    rule Empty 0 = Occupied
    rule Empty _ = Empty
    rule Occupied x = if x >= 5 then Empty else Occupied
    visible m cx cy dx dy = fromMaybe Floor . List.find (/= Floor) . map (get m) . List.takeWhile (Uc.inBoundingBox box) . tail $ Uc.ray cx cy dx dy
    get m key = Map.findWithDefault Floor key m
    stencil m c@(cx, cy) = rule (get m c) (U.count (== Occupied) [ nearest (-1) (-1)
                                                         , nearest (-1) 0
                                                         , nearest (-1) 1
                                                         , nearest 0 (-1)
                                                         , nearest 0 1
                                                         , nearest 1 (-1)
                                                         , nearest 1 0
                                                         , nearest 1 1
                                                         ])
      where nearest = visible m cx cy
    update m = List.foldr (\key@(cx, cy) m' -> Map.insert key (stencil m key) m') m $ Map.keys m
