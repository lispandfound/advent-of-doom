{-# LANGUAGE TemplateHaskell #-}

module Days.Day03 (runDay, Function (..), name, arguments) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Util.Util ((<<))
import Util.Parsers (between)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Functor (($>))
import Data.Bifunctor
import Replace.Attoparsec.Text
import Data.Ix (inRange)
import Control.Lens
import Data.Text (isSuffixOf)
import Data.Char (isAlpha)
{- ORMOLU_ENABLE -}

data Function = Function
  { _name :: Text,
    _arguments :: [Int]
  }
  deriving (Show, Generic)

$(makeLenses ''Function)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = toListOf (traversed . _Right) . splitCap function <$> takeText
  where
    function = Function <$> takeTill (\c -> c /= '_' && c /= '\'' && not (isAlpha c)) <*> between "(" ")" (decimal `sepBy` ",")

------------ TYPES ------------
type Input = [Function]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = getSum . foldOf (traversed . isMul . correctArity . argsInRange . arguments . to (Sum . product))
  where
    isMul = filtered (("mul" `isSuffixOf`) . _name)
    correctArity = filtered ((== 2) . length . _arguments)
    argsInRange = filtered (all (inRange (0, 999)) . _arguments)

------------ PART B ------------
partB :: Input -> OutputB
partB = fst . foldl' go (0, True)
  where
    go (total, doit) (Function fn args)
      | "mul" `isSuffixOf` fn && all (inRange (0, 999)) args && length args == 2 && doit = (total + product args, doit)
      | "do" `isSuffixOf` fn = (total, True)
      | "don't" `isSuffixOf` fn = (total, False)
      | otherwise = (total, doit)
