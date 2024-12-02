module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as List hiding (sum, notElem)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Util.Util as U
import Util.Parsers as P
import Data.Char as Char

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Void
import Data.Functor (($>))
import Data.Bifunctor
import Control.Lens
import Control.Arrow
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = linesOf $ decimal `around` many space

------------ TYPES ------------
type Input = [(Int, Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = getSum . foldMap (Sum . abs . uncurry (-)) . left . right
  where
    left = partsOf (traversed . _1) %~ sort
    right = partsOf (traversed . _2) %~ sort

------------ PART B ------------
partB :: Input -> OutputB
partB entries = sum . map (\l -> right ^. at l . non 0 * l) $ left
  where
    right = freq $ map snd entries
    left = map fst entries
