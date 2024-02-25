module Days.Day08 (runDay) where

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
import Control.Applicative
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Control.Monad.State
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = instruction `sepBy` endOfLine
  where instruction = nop <|> acc <|> jmp
        nop = "nop " *> pure Nop <*> signed decimal
        acc = "acc " *> pure Acc <*> signed decimal
        jmp = "jmp " *> pure Jmp <*> signed decimal

data Instruction = Nop Int | Acc Int | Jmp Int deriving (Show, Eq)
data ProgramState = ProgramState {
  pc :: Int
  , accumulator :: Int
  , executed :: Set Int
                                 } deriving (Show, Eq)

------------ TYPES ------------
type Input = [Instruction]

type OutputA = Int

type OutputB = Maybe Int

------------ PART A ------------
partA :: Input -> OutputA
partA program = evalState run $ initialState
  where
    initialState = ProgramState 0 0 mempty
    run = do
          curPc <- pc <$> get
          curAccumulator <- accumulator <$> get
          exec <- executed <$> get
          if curPc `Set.member` exec then
            return curAccumulator
          else do
            case program !! curPc of
              Nop _ -> modify (\st -> st {pc = curPc + 1})
              Acc dx -> modify (\st -> st {pc = curPc + 1, accumulator = curAccumulator + dx})
              Jmp j -> modify (\st -> st {pc = curPc + j})
            modify (\st -> st {executed = Set.insert curPc exec})
            run

------------ PART B ------------
partB :: Input -> OutputB
partB program = join . find isJust . map (\i -> evalStateT (run i) initialState) . findIndices jmpOrNop $ program
  where
    jmpOrNop (Acc _) = False
    jmpOrNop _ = True
    initialState = ProgramState 0 0 mempty
    run sw = do
          curPc <- pc <$> get
          curAccumulator <- accumulator <$> get
          exec <- executed <$> get
          guard $ not (curPc `Set.member` exec)
          if curPc >= length program then
            return curAccumulator
          else do
            case program !! curPc of
              Nop i | curPc == sw -> jmp i
              Nop _ -> nop
              Acc dx -> acc dx
              Jmp _ | curPc == sw -> nop
              Jmp j -> jmp j
            modify (\st -> st {executed = Set.insert curPc exec})
            run sw
    nop = do
           curPc <- pc <$> get
           modify (\st -> st {pc = curPc + 1})
    acc dx = do
           curPc <- pc <$> get
           curAccumulator <- accumulator <$> get
           modify (\st -> st {pc = curPc + 1, accumulator = curAccumulator + dx})
    jmp j = do
           curPc <- pc <$> get
           modify (\st -> st {pc = curPc + j})
