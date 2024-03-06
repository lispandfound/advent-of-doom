-- |

module Util.CList where

import Data.Sequence (Seq(..), ViewR(..), ViewL(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq

data CList a = Null | CList (Seq a) a (Seq a)

instance Show a => Show (CList a) where
  show (CList l x r) = "CList (" ++ show l ++ ") " ++ show x ++ " (" ++ show r ++ ")"

singleton :: a -> CList a
singleton x = CList mempty x mempty

empty :: CList a
empty = Null

null :: CList a -> Bool
null Null = True
null _ = False

focus :: CList a -> Maybe a
focus Null = Nothing
focus (CList _ x _) = pure x

rightElements :: CList a -> Seq a
rightElements (CList l x r) = (x <| r) >< l
rightElements Null = mempty

rightNElements :: Int -> CList a -> Seq a
rightNElements 0 _ = mempty
rightNElements k (CList l x r) = (x <| Seq.take (max 0 $ k - 1) r) >< Seq.take (max 0 $ k - 1 - Seq.length r) l

rotateTo :: Eq a => a -> CList a -> Maybe (CList a)
rotateTo _ Null = Nothing
rotateTo y cl@(CList l x r)
  | x == y = pure cl
  | otherwise = case Seq.elemIndexL y l of
      Just i -> Just $ CList (Seq.take i l) y ((Seq.drop (i + 1) l |> x) >< r)
      Nothing -> case Seq.elemIndexL y r of
        Just j -> Just $ CList ((l |> x) >< Seq.take j r) y (Seq.drop (j + 1) r)
        Nothing -> Nothing

fromList :: [a] -> CList a
fromList [] = Null
fromList (x:xs) = CList mempty x (Seq.fromList xs)

rotR :: CList a -> CList a
rotR (CList l x (y :<| rs)) = CList (l |> x) y rs
rotR (CList (y :<| ls) x Empty) = CList (Seq.singleton x) y ls
rotR _ = Null


removeR :: CList a -> CList a
removeR (CList l x (y :<| rs)) = CList l y rs
removeR (CList (y :<| ls) x Empty) = CList Empty y ls
removeR _ = Null

filterR :: (a -> Bool) -> CList a -> CList a
filterR f (CList l x r) = case (Seq.filter f l, Seq.filter f r) of
  (l', r') | f x -> CList l' x r'
  (l', x :<| rs) -> CList l' x rs
  (x :<| ls, Empty) -> CList Empty x ls
  (Empty, Empty) -> Null

insertL :: a -> CList a -> CList a
insertL y Null = singleton y
insertL y (CList l x r) = CList (l |> x) y r

insertLS :: CList a -> Seq a -> CList a
insertLS x Empty = x
insertLS Null (ys :|> y) = CList mempty y ys
insertLS (CList l x r) (ys :|> y) = CList ((l |> x) >< ys) y r

insertFarRight :: CList a -> Seq a -> CList a
insertFarRight x Empty = x
insertFarRight Null (x :<| xs) = CList mempty x xs
insertFarRight (CList l x r) xs = CList l x (r >< xs)

maxEl :: Ord a => CList a -> a
maxEl = maximum . rightElements
