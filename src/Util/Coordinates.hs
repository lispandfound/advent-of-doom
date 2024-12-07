module Util.Coordinates where

import Control.Arrow ((***))
import Control.Comonad
import Control.Comonad.Trans.Store

-- import Data.Graph.Inductive.Graph
-- import Data.Graph.Inductive.PatriciaTree (Gr (..))
import Data.Map qualified as Map

type CoordinateMap = Map (Int, Int)
type CoordinateStore = Store (Int, Int)

cantor :: Int -> Int -> Int
cantor a b = ((m + n) * (m + n + 1)) `div` 2 + n
  where
    neg k = if k < 0 then 2 * k + 1 else 2 * k
    m = neg a
    n = neg b

-- Takes a nested list (to be thought of as a 2D structure).
-- Returns a map from "co-ordinates" to the items in the list.
-- For example:
--     Input: [[a,b,c],[d,e]]
--     Output: Map.fromList [((0,0),a), ((0,1),b), ((0,2),c), ((1,0),d), ((1,1),e)]
mapFromNestedLists :: (Ord a) => [[a]] -> CoordinateMap a
mapFromNestedLists = fromList . attachCoords 0 0
  where
    attachCoords _ _ [] = []
    attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
    attachCoords x y ((l : ls) : lss) = ((x, y), l) : attachCoords x (y + 1) (ls : lss)

mapToStore :: CoordinateMap a -> CoordinateStore (Maybe a)
mapToStore m = store (`Map.lookup` m) (0, 0)

storeToMap :: CoordinateStore (Maybe a) -> [(Int, Int)] -> CoordinateMap a
storeToMap store = Map.fromList . mapMaybe (\c -> (c,) <$> peek c store)

at :: Int -> Int -> CoordinateStore a -> a
at dx dy = peeks ((+ dx) *** (+ dy))

move :: Int -> Int -> CoordinateStore a -> CoordinateStore a
move dx dy = seeks ((+ dx) *** (+ dy))

-- Execute a convolution function at every defined point of a CoordinateMap. The caller is passed a function get, which can retreive nearby cells
convolve :: ((Int -> Int -> Maybe a) -> Maybe b) -> CoordinateMap a -> CoordinateMap b
convolve f m =
    foldr
        ( \key@(cx, cy) m' -> case (f $ get cx cy) of
            Just v -> Map.insert key v m'
            Nothing -> m'
        )
        mempty
        $ Map.keys m
  where
    get cx cy dx dy = Map.lookup (cx + dx, cy + dy) m

-- Retrieve all coordinates in a given direction
ray :: Int -> Int -> Int -> Int -> [(Int, Int)]
ray cx cy dx dy = [(cx + i * dx, cy + i * dy) | i <- [0 ..]]

rayTillNothing :: CoordinateMap a -> Int -> Int -> Int -> Int -> [a]
rayTillNothing m cx cy dx dy = catMaybes . takeWhile isJust . map (`Map.lookup` m) $ ray cx cy dx dy

type BoundingBox = (Int, Int, Int, Int)

inBoundingBox :: BoundingBox -> (Int, Int) -> Bool
inBoundingBox (lx, ux, ly, uy) (x, y) = lx <= x && x <= ux && ly <= y && y <= uy

-- mapToFollowGraph :: CoordinateMap a -> Gr a ()
-- mapToFollowGraph = mapToFollowGraphOn (\_ _ -> True)

-- mapToFollowGraphOn :: (a -> a -> Bool) -> CoordinateMap a -> Gr a ()
-- mapToFollowGraphOn follows = liftA2 mkGraph nodes edges
--   where
--     nodes = map (first (uncurry cantor)) . Map.toList
--     edges m =
--         [ (cantor x1 y1, cantor (x1 + dx) (y1 + dy), ())
--         | (x1, y1) <- Map.keys m
--         , dx <- [-1, 0, 1]
--         , dy <- [-1, 0, 1]
--         , (dx, dy) /= (0, 0) && fromMaybe False (follows <$> Map.lookup (x1, y1) m <*> Map.lookup (x1 + dx, y1 + dy) m)
--         ]

-- terminalPaths :: [Node] -> Gr a b -> [Path]
-- terminalPaths nodes graph = concatMap go nodes
--   where
--     go :: Node -> [Path]
--     go from
--         | outdeg graph from == 0 = [[from]]
--         | otherwise = do
--             next <- suc graph from
--             path <- go next
--             return (from : path)
