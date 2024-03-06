module Days.Day23 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Char (digitToInt)
import Control.Applicative
import Util.CList (CList)
import qualified Util.CList as CL
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = CL.fromList <$> many (digitToInt <$> digit)

------------ TYPES ------------

type Input = CList Int

type OutputA = CList Int

type OutputB = Seq Int


step :: Int -> CList Int -> CList Int
step m cl = cl'
  where
    Just x = CL.focus cl
    top = CL.rightNElements 3 . CL.removeR $ cl
    smallCl :: CList Int
    smallCl = U.hammerN 3 CL.removeR (CL.rotR cl)
    Just dest = find (isNothing . (`Seq.elemIndexL` top)) $ [x - 1, x - 2 .. 1] ++ [m, m - 1 ..]
    Just smallClF = CL.rotateTo dest smallCl
    Just cl' = fmap CL.rotR . CL.rotateTo x . CL.insertLS smallClF $ top

------------ PART A ------------
partA :: Input -> OutputA
partA = U.hammerN 100 (step 9)

------------ PART B ------------
partB :: Input -> OutputB
partB cl = CL.rightNElements 3 . fromJust . CL.rotateTo 1 . U.hammerN 10000000 (step 1000000) $ CL.insertFarRight cl (Seq.fromList [10..1000000])
