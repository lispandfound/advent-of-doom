module Days.Day13 (runDay) where

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
import Data.Bifunctor
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Functor
import Control.Applicative
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  t <- decimal
  endOfLine
  busIds <- (Just <$> decimal <|> ("x" $> Nothing)) `sepBy` ","
  return $ Timetable t busIds


------------ TYPES ------------
data Timetable = Timetable {
  curTime :: Integer
  , busses :: [Maybe Integer]
                       } deriving (Show, Eq)
type Input = Timetable

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
partA :: Input -> OutputA
partA (Timetable t busses) = timestampF . U.minimumOn nearestTime . catMaybes $ busses
  where nearestTime period = period - t `mod` period
        timestampF period = period * nearestTime period

------------ PART B ------------
-- Chinese remainder theorem 2: electric boogaloo
partB :: Input -> OutputB
partB = U.crtSystem . map (bimap negate fromJust) . filter (isJust . snd) . zip [0..] . busses
