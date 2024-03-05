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

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Char (digitToInt)
import Control.Applicative
import Data.CircularList (CList)
import qualified Data.CircularList as CL
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = CL.fromList <$> many (digitToInt <$> digit)

------------ TYPES ------------

type Input = CList Int

type OutputA = String

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = foldMap show . tail . CL.rightElements . fromJust . CL.rotateTo 1 . U.hammerN 100 go
  where go cl = cl'
          where
            Just x = CL.focus cl
            top = List.take 3 . CL.rightElements . CL.removeR $ cl
            smallCl = U.hammerN 3 CL.removeR (CL.rotR cl)
            smallerThanXCL = CL.filterR (< x) smallCl
            dest = maximum . CL.rightElements $ if smallerThanXCL == CL.empty then smallCl else smallerThanXCL
            Just smallClF = CL.rotateTo dest smallCl
            Just cl' = fmap CL.rotR . CL.rotateTo x . foldr CL.insertL smallClF . reverse $ top



------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
