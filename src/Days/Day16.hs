module Days.Day16 (runDay) where

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
import Data.Text (Text)
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import qualified Util.OpenSet as OS
import qualified Util.Parsers as Up
import Util.OpenSet (OpenSet)
import Debug.Trace
import Data.Bifunctor (bimap)
import qualified Data.Text as Text
import Util.Matching (matching)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

field = (,) <$> takeTill (== ':') <*> (": " *> os)
os = OS.fromIntervals <$> (interval `sepBy` " or ")
interval = do
  (a, b) <- decimal `Up.around` "-"
  return (a - 1, b + 1)
ticketP = decimal `sepBy` ","

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  fs <- (field) `sepBy` endOfLine
  endOfLine
  endOfLine
  "your ticket:"
  endOfLine
  t <- ticketP
  endOfLine
  endOfLine
  "nearby tickets:"
  endOfLine
  ts <- ticketP `sepBy` endOfLine
  return $ Notes (Map.fromList fs) t ts

------------ TYPES ------------
data Notes = Notes {
  fields :: Map Text (OpenSet Int)
  , ticket :: [Int]
  , otherTickets :: [[Int]]
                   } deriving (Show, Eq)
type Input = Notes

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA inp = sum . concatMap invalidFields . otherTickets $ inp
  where
    invalidFields =  filter (not . (`OS.member` os))
    os = foldr1 OS.union . map snd . Map.toList . fields $ inp

------------ PART B ------------
partB :: Input -> OutputB
partB inp = product . map snd . filter (("departure" `Text.isPrefixOf`). fst). Map.toList $ mapping
  where
    os = foldr1 OS.union . Map.elems . fields $ inp
    consistentTickets = filter (all (`OS.member` os)) $ otherTickets inp
    columns = transpose consistentTickets
    fieldIndexCandidates = consistentColumns <$> fields inp
    consistentColumns os = findIndices (all (`OS.member` os)) columns
    names = Map.keys . fields $ inp
    nameIndexLookup = Map.fromList $ zip names [0..]
    edges = concatMap (\(k, v) -> map (fromJust $ Map.lookup k nameIndexLookup,) v) . Map.toList $ fieldIndexCandidates
    mapping = Map.fromList . map (bimap (names !!) (ticket inp !!)) . Map.toList . matching . Set.fromList $ edges
