module Days.Day22 (runDay) where

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
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Sequence (Seq(..), ViewR(..), ViewL(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import Data.Bifunctor
import Data.Monoid (Sum(Sum, getSum))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> player <*> (endOfLine >> endOfLine >> player)
  where player = do
          "Player "
          decimal
          ":"
          endOfLine
          decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = ([Int], [Int])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . zipWith (*) [1..] . reverse . uncurry play
  where play [] y = y
        play x [] = x
        play (x:xs) (y:ys) = if x > y then play (xs ++ [x, y]) ys else play xs (ys ++ [y, x])

data Outcome = Player1 (Seq Int) | Player2 (Seq Int) deriving (Show, Eq)

------------ PART B ------------
partB :: Input -> OutputB
partB = getSum . Seq.foldMapWithIndex (\i x -> Sum $ (i + 1) * x) . Seq.reverse . stack . play mempty . sequify
  where
    sequify :: ([Int], [Int]) -> (Seq Int, Seq Int)
    sequify = bimap Seq.fromList Seq.fromList
    stack (Player1 v) = v
    stack (Player2 v) = v
    play :: Set (Seq Int, Seq Int) -> (Seq Int, Seq Int) -> Outcome
    play _ (x, Empty) = Player1 x
    play _ (Empty, y) = Player2 y
    play history d@(x :<| xs, y :<| ys)
         | d `Set.member` history = Player1 (fst d)
         | x <= Seq.length xs && y <= Seq.length ys = case play mempty (Seq.take x xs, Seq.take y ys) of
              Player1 _ ->  play (Set.insert d history) (xs >< Seq.fromList [x, y], ys)
              Player2 _ ->  play (Set.insert d history) (xs, ys >< Seq.fromList [y, x])
         | x > y = play (Set.insert d history) (xs >< Seq.fromList [x, y], ys)
         | otherwise = play (Set.insert d history) (xs, ys >< Seq.fromList [y, x])
