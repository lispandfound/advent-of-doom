module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Functor (($>))
import Data.Bifunctor
import Control.Lens
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = error "Not implemented yet!"

------------ TYPES ------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
