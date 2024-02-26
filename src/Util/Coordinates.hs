module Util.Coordinates where
import Data.Map (Map)
import Data.Map as Map
import Data.List as List

type CoordinateMap a = Map (Int, Int) a


-- Takes a nested list (to be thought of as a 2D structure).
-- Returns a map from "co-ordinates" to the items in the list.
-- For example:
--     Input: [[a,b,c],[d,e]]
--     Output: Map.fromList [((0,0),a), ((0,1),b), ((0,2),c), ((1,0),d), ((1,1),e)]
mapFromNestedLists :: (Ord a) => [[a]] -> CoordinateMap a
mapFromNestedLists = Map.fromList . attachCoords 0 0
  where
    attachCoords _ _ [] = []
    attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
    attachCoords x y ((l : ls) : lss) = ((x, y), l) : attachCoords x (y + 1) (ls : lss)

-- Execute a convolution function at every defined point of a CoordinateMap. The caller is passed a function get, which can retreive nearby cells
convolve :: a -> ((Int -> Int -> a) -> a) -> CoordinateMap a -> CoordinateMap a
convolve def f m = List.foldr (\key@(cx, cy) m' -> Map.insert key (f $ get cx cy) m') m $ Map.keys m
  where get cx cy dx dy = Map.findWithDefault def (cx + dx, cy + dy) m

-- Retreive all coordinates in a given direction
ray :: Int -> Int -> Int -> Int -> [(Int, Int)]
ray cx cy dx dy = [(cx + i * dx, cy + i * dy) | i <- [0..]]

type BoundingBox = (Int, Int, Int, Int)

inBoundingBox :: BoundingBox -> (Int, Int) -> Bool
inBoundingBox (lx, ux, ly, uy) (x, y) = lx <= x && x <= ux && ly <= y && y <= uy
