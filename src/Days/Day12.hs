module Days.Day12 (runDay) where

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
import Control.Applicative
import Data.Void
import Util.Pair
import Control.Monad.State.Strict
import Debug.Trace
import Data.Tuple (swap)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = instruction `sepBy` endOfLine
  where inst c = char c *> decimal
        north = North <$> inst 'N'
        west = West <$> inst 'W'
        east = East <$> inst 'E'
        south = South <$> inst 'S'
        left = L <$> inst 'L'
        right = R <$> inst 'R'
        forward = Forward <$> inst 'F'
        instruction = north <|> west <|> east <|> south <|> left <|> right <|> forward

data Instruction = North Int | South Int | East Int | West Int | L Int | R Int | Forward Int deriving Show
data ShipState = ShipState {
  heading :: Int
  , pos :: Pair Int
                           } deriving (Show, Eq)
data ShipWaypointState = ShipWaypointState {
  shipPos :: Pair Int
  , waypointPos :: Pair Int
                                           } deriving (Show, Eq)
------------ TYPES ------------
type Input = [Instruction]

type OutputA = Int

type OutputB = Int


------------ PART A ------------
partA :: Input -> OutputA
partA inst = manhattan $ evalState (mapM_ execute inst >> position) initialState
  where
    position = gets pos
    move vec = modify (\st -> st { pos = vec + pos st })
    rotate count = modify (\st -> st { heading = (count + heading st) `mod` 4 })
    headingVec = gets $ Pair . ([(1, 0), (0, -1), (-1, 0), (0, 1)] !!) . heading
    execute (North x) = move . Pair $ (0, x)
    execute (South x) = move . Pair $ (0, -x)
    execute (East x) = move . Pair $ (x, 0)
    execute (West x) = move . Pair $ (-x, 0)
    execute (L x) = rotate (-x `div` 90)
    execute (R x) = rotate (x `div` 90)
    execute (Forward x) = headingVec >>= move . fmap (* x)
    initialState = ShipState 0 (Pair (0, 0))


------------ PART B ------------
partB :: Input -> OutputB
partB inst = manhattan $ evalState (forM_ inst execute >> position) initialState
  where
    initialState = ShipWaypointState (Pair (0, 0)) (Pair (10, 1))
    position = gets shipPos
    left = modify (\st -> st { waypointPos = pmap swap . second negate $ waypointPos st })
    right = modify (\st -> st { waypointPos = pmap swap . first negate $ waypointPos st })
    move vec = modify (\st -> st { shipPos = vec + shipPos st })
    moveW vec = modify (\st -> st { waypointPos = vec + waypointPos st })
    execute (North x) = moveW . Pair $ (0, x)
    execute (South x) = moveW . Pair $ (0, -x)
    execute (East x) = moveW . Pair $ (x, 0)
    execute (West x) = moveW . Pair $ (-x, 0)
    execute (L x) = mapM_ (const left) [0..(x `div` 90 - 1) `mod` 4]
    execute (R x) = mapM_ (const right) [0..(x `div` 90 - 1) `mod` 4]
    execute (Forward x) = gets waypointPos >>= move . fmap (* x)
