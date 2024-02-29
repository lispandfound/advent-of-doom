module Util.Util where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import Data.List
import Data.Maybe
import Control.Monad
import Data.Tree
import Math.Combinatorics.Exact.Binomial
import qualified Data.Set as Set
import Data.Graph.Inductive
{- ORMOLU_ENABLE -}

{-
This module contains a series of miscellaneous utility functions that I have found helpful in the past.
-}

-- Takes a list.
-- Returns a map from elements of that list to the number of times they appeared in the list.
freq :: (Ord a) => [a] -> Map a Int
freq = Map.fromListWith (+) . fmap (,1)


-- Splits a list into chunks of the specified size.
-- The final chunk may be smaller than the chunk size.
-- Chunk size must be positive.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls
  | n <= 0 = error "Cannot split into chunks of negative length."
  | null ls = []
  | length ls < n = [ls]
  | otherwise = (take n ls) : (chunksOf n (drop n ls))

-- Returns all contiguous subsets of a list of the specified size
-- All chunks are guaranteed to be of the specified size, the tails of the last n elements are not included
windows :: Int -> [a] -> [[a]]
windows n ls
  | length ls < n = []
  | otherwise = take n ls : windows n (tail ls)


-- Splits a list into maximal contiguous chunks that satisfy the given predicate.
-- For example:
--     Input: (> 3) [5,4,3,2,7,6,3,4]
--     Output: [[5,4],[7,6],[4]]
chunksByPredicate :: (a -> Bool) -> [a] -> [[a]]
chunksByPredicate p ls
  | null ls = []
  | otherwise =
    let (prefix, rest) = span p ls
     in if null prefix
          then (chunksByPredicate p $ dropWhile (not . p) rest)
          else prefix : (chunksByPredicate p $ dropWhile (not . p) rest)

-- Allows the user to log out some context and then the result of some expression
-- For example, supposing a is 2, and b is 5:
--     Input: traceShowIdWithContext (a, b) $ a + b
--     Output: (2, 5)	7
traceShowIdWithContext :: (Show a, Show b) => a -> b -> b
traceShowIdWithContext context result = trace (show context ++ "\t" ++ show result) result

-- Like !!, but with bounds checking
(!!?) :: [a] -> Int -> Maybe a
list !!? index =
  if
      | index < 0 -> Nothing
      | index >= (length list) -> Nothing
      | otherwise -> Just $ list !! index

-- Given a map where the keys are co-ordinates, returns the minimum x, maximum x, minimum y, and maximum y; in that order.
mapBoundingBox :: Map (Int, Int) a -> (Int, Int, Int, Int)
mapBoundingBox m =
  (,,,)
    (minimum . fmap fst . Map.keys $ m)
    (maximum . fmap fst . Map.keys $ m)
    (minimum . fmap snd . Map.keys $ m)
    (maximum . fmap snd . Map.keys $ m)


count :: (a -> Bool) -> [a] -> Int
count m = length . filter m

paths :: Tree a -> [[a]]
paths (Node v []) = [[v]]
paths (Node v xs) = concatMap (map (v:) . paths) xs

findTargetSum :: (Integral a, Eq a) => a -> [a] -> Maybe a
findTargetSum target = fmap fst . (!!? 0) . Map.toList . Map.filter (> 1) . freq . map (\x -> if x <= target `div` 2 then target - x else x)

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f = join . find isJust . map f

slice :: Int -> Int -> [a] -> [a]
slice i j = take (j - i + 1) . drop i

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f l = map (uncurry f) $ zip l (tail l)

orderedPartitions :: Integral a => a -> a -> a
orderedPartitions n k = sum . map (choose (n - 1)) $ [0..k - 1]

hammer :: Eq a => (a -> a) -> a -> a
hammer f x
  | x' == x = x'
  | otherwise = hammer f x'
  where x' = f x

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy go
  where go x y = compare (f x) (f y)


egcd :: Integral a => a -> a -> (a, a)
egcd a b = go a b 1 0 0 1
  where
    go _ 0 s _ t _ = (s, t)
    go rp r sp s tp t = go r (rp - q * r) s (sp - q * s) t (tp - q * t)
      where q = rp `div` r

-- crt x1 x2 m1 m2 solves the simultaneous equation x = x1 mod m1, x = x2 mod m2 using the chinese remainder theorem
-- assumes gcd m1 m2 == 1
-- returns the smallest positive solution
crt :: Integral a => a -> a -> a -> a -> a
crt x1 x2 m1 m2 = mabs . (`mod` (m1 * m2)) $ x2 * s * m1 + x1 * t * m2
  where (s, t) = egcd m1 m2
        mabs x = if x > 0 then x else m1 * m2 - x

-- crtSystem solves the simultaneous equations x = xi mod mi for some list [(xi, mi), ...]
-- assumes gcd mi mj == 1
-- returns the smallest positive solution
crtSystem :: Integral a => [(a, a)] -> a
crtSystem = fst . foldr1 (\(x1, m1) (x2, m2) -> (crt x1 x2 m1 m2, m1 * m2))

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f x = do
    x' <- f x
    (x':) `liftM` iterateM f x'

hammerN :: Eq a => Int -> (a -> a) -> a -> a
hammerN 0 _ v = v
hammerN n f v = let v' = f v in
                 if v == v' then v else hammerN (n - 1) f v'


data MatchVerts = Source | Sink | U Int | V Int deriving (Show, Eq)


instance Ord MatchVerts where
  compare Source _ = LT
  compare _ Source = GT
  compare Sink _ = LT
  compare _ Sink = GT
  compare (U _) (V _) = LT
  compare (V _) (U _) = GT
  compare (U a) (U b) = compare a b
  compare (V a) (V b) = compare a b


matching ::  [(Int, Int)] -> Map Int Int
matching edges = Map.fromList . mapMaybe (\case
                              (0, _, _) -> Nothing
                              (_, 1, _) -> Nothing
                              (eu, ev, (1, _)) -> Just (eu - 2, ev - Set.size v - 2)
                              _ -> Nothing) . labEdges $ maxFlowgraph g 0 1
  where
    u = Set.fromList (map (U . fst) edges)
    v = Set.fromList (map (V . snd) edges)
    verts = Set.toAscList $ Set.unions [Set.fromList [Source, Sink]
                       , u
                       , v]
    g :: Gr MatchVerts Int
    g = mkGraph (zip [0..] verts) (map edgeMap edges
                                   ++ map (0,,1) [2..Set.size u + 1]
                                   ++ map (,1,1) [Set.size u + 2 .. 2 * Set.size u + 1])
    edgeMap (eu, ev) = (2 + eu, 2 + Set.size u + ev, 1)
