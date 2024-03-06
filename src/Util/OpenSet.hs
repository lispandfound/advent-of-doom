module Util.OpenSet (OpenSet, fromIntervals, toIntervals, member, union) where
import Util.Pair
import Data.List (sortOn)

newtype OpenSet a = OpenSet {toPairs :: [Pair a]} deriving (Show, Eq)

intervalContains :: Ord a => Pair a -> Pair a -> Bool
intervalContains (Pair (a, b)) (Pair (c, d)) = a <= c && d <= b

intervalUnion :: Ord a => Pair a -> Pair a -> [Pair a]
intervalUnion p1@(Pair (a, b)) p2@(Pair (c, d))
  | p2 `intervalContains` p1 = [p2]
  | p1 `intervalContains` p2 = [p1]
  | b > c && a < d = [Pair (a, d)]
  | d > a && c < b = [Pair (c, b)]
  | a > d = [p1, p2]
  | otherwise = [p2, p1]


fromIntervals :: (Ord a, Num a) => [(a, a)] -> OpenSet a
fromIntervals = OpenSet . foldr (go . Pair) []  . sortOn (negate . fst)
  where go x []  = [x]
        go p2 l@(p1:xs)  = p1 `intervalUnion` p2 ++ xs

toIntervals :: OpenSet a -> [(a, a)]
toIntervals = map getPair . toPairs

member :: Ord a => a -> OpenSet a -> Bool
member x = any (intMember x) . toPairs
  where intMember x (Pair (c, d)) = c < x && x < d

union :: (Ord a, Num a) => OpenSet a -> OpenSet a -> OpenSet a
union (OpenSet ps) (OpenSet os) = fromIntervals $ map getPair ps ++ map getPair os
