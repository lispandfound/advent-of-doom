module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Data.List as List
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Control.Monad.State
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` ","

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

data CounterState = CounterState {
  ix :: Int
  , memory :: Map Int (Int, Int)
                            } deriving (Show, Eq)

------------ PART A ------------
partA :: Input -> OutputA
partA inp = (!! (2020 - length inp - 1)) . evalState (U.iterateM go x0) $ initialState
   where initialState = CounterState (length inp + 1) $ Map.fromList . zipWith (\i x -> (x, (i, i))) [1 ..] $ inp
         x0 = last inp
         go x = do
           i <- gets ix
           (i1, i2) <- gets (Map.findWithDefault (i, i) x . memory)
           modify (\st -> st {memory = Map.insertWith (const $ push i) (i1 - i2) (i, i) $ memory st, ix = i + 1})
           mem <- gets memory
           return $ i1 - i2
         push i (x, y) = (i, x)

------------ PART B ------------
partB :: Input -> OutputB
partB inp = go (length inp + 1) m x0
  where m = Map.fromList . zipWith (\i x -> (x, (i, i))) [1 ..] $ inp
        x0 = last inp
        go i m x
          | i == 30000000 + 1 = x
          | otherwise = let (i1, i2) = Map.findWithDefault (i, i) x m in
              go (i + 1) (Map.insertWith (const $ push i) (i1 - i2) (i, i) m) (i1 - i2)
        push i (x, y) = (i, x)
