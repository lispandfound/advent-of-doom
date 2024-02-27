module Days.Day14 (runDay) where

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
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative
import Data.Functor
import Data.Bits
import Control.Monad.State.Strict
import Data.List as List
import Debug.Trace
import Numeric.Natural
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = inst `sepBy` endOfLine
  where
    inst :: Parser Instruction
    inst = mask <|> set
    mask :: Parser Instruction
    mask = do
      "mask = "
      mask <- many1 ("X" $> Nothing <|> "0" $> Just False <|> "1" $> Just True)
      return $ SetMask mask
    set :: Parser Instruction
    set = do
      "mem["
      loc <- decimal
      "] = "
      value <- decimal
      return $ SetMemory loc value


type Mask = [Maybe Bool]
data Instruction = SetMask Mask | SetMemory Natural Natural deriving (Show)
data ComputerState = ComputerState {
  setMask :: Natural
  , resetMask :: Natural
  , memory :: Map Natural Natural
                                   }
------------ TYPES ------------
type Input = [Instruction]

type OutputA = Natural

type OutputB = Natural

binaryRep :: Int -> Natural -> String
binaryRep n x = reverse . List.take n . map (\i -> if testBit x i then '1' else '0') $ [0..]

------------ PART A ------------
partA :: Input -> OutputA
partA inst = evalState (forM_ inst execute >> memSum) initialState
  where

    initialState = ComputerState 0 0 mempty
    memSum = gets (sum . map snd . Map.toList . memory)
    writeMask mask = modify (\st ->
                               st {setMask = sm, resetMask = rm})
      where sm = foldl (\m b -> case b of
                                 Just True -> 2 * m + 1
                                 _ -> 2 * m) 0 mask
            rm = foldl (\m b -> case b of
                                 Just False -> 2 * m
                                 _ -> 2 * m + 1) 0 mask
    write loc value = modify (\st -> st { memory = Map.insert loc ((value .|. setMask st) .&. resetMask st) $ memory st })
    ones n = (1 .<<. (n + 1)) - 1
    execute (SetMask mask) = writeMask mask
    execute (SetMemory loc value) = write loc value

allMasks :: [Maybe Bool] -> [Natural -> Natural]
allMasks [] = return id
allMasks (Nothing:xs) = do
  x <- [clearBit, setBit]
  (flip x (length xs) .) <$> allMasks xs
allMasks (Just True:xs) = ((`setBit` length xs) .) <$> allMasks xs
allMasks (Just False:xs) = allMasks xs

data Version2Computer = Version2Computer {
  masks :: [Natural -> Natural]
  , v2memory :: Map Natural Natural
                                         }

------------ PART B ------------
partB :: Input -> OutputB
partB inst = evalState (forM_ inst execute >> memSum) initialState
  where
    initialState = Version2Computer (allMasks []) mempty
    memSum = gets $ sum . map snd . Map.toList . v2memory
    execute (SetMask m) = modify (\st -> st { masks = allMasks m })
    execute (SetMemory loc value) = do
      msks <- gets masks
      forM_ (map (\m -> m loc) msks) (\loc -> modify (\st -> st { v2memory = Map.insert loc value $ v2memory st }))
