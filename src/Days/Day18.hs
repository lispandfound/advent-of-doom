module Days.Day18 (runDay) where

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

import qualified Program.RunDay as R (runDayAB, Day)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative
import Data.Functor
import Util.Parsers
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDayAB inputParserA inputParserB partA partB

------------ PARSER ------------
inputParserA :: Parser [Expression]
inputParserA = expr `sepBy` endOfLine
  where
    expr :: Parser Expression
    expr = factor `chainl1` op
    op = " + " $> Add <|> " * " $> Mul
    factor = parens expr <|> integer
    parens :: Parser a -> Parser a
    parens p = "(" *> p <* ")"
    integer = Num <$> decimal

inputParserB :: Parser [Expression]
inputParserB = expr `sepBy` endOfLine
  where
    expr :: Parser Expression
    expr = term `chainl1` mul
    add = " + " $> Add
    term = factor `chainl1` add
    mul = " * " $> Mul
    factor = parens expr <|> integer
    parens :: Parser a -> Parser a
    parens p = "(" *> p <* ")"
    integer = Num <$> decimal

------------ TYPES ------------
data Expression = Add Expression Expression | Mul Expression Expression | Num Int deriving (Show, Eq)

evaluate :: Expression -> Int
evaluate (Add l r) = evaluate l + evaluate r
evaluate (Mul l r) = evaluate l * evaluate r
evaluate (Num x) = x

type Input = [ Expression ]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map evaluate

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map evaluate
