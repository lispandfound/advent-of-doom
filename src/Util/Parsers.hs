module Util.Parsers where

import Data.Text
import Util.Coordinates
import Data.Attoparsec.Text
import Data.List as List
import Data.Map (Map)
import Control.Applicative (many, (<|>))
import qualified Data.Map as Map

{-
This module contains a list of parsers and combinators which are likely to be useful for Advent of Code problems.
-}

------------ PARSERS ------------
-- Takes a "mapper" "function that might map a char to a datatype, and an initial index (usually 0 or 1)
-- Returns a parser that returns a map from coordinates to all instances where the function returns Just
coordinateParser :: Parser (Maybe a) -> Int -> Parser (CoordinateMap a)
coordinateParser p start = coordinateParser' start start
  where
    coordinateParser' x y =
      choice
        -- First we look for a line break, and we reset the coordinates appropriately
        [ endOfLine >> coordinateParser' start (y + 1),
          -- Then we look for a character, and map it
          p >>= (\c -> Map.alter (const c) (x, y) <$> coordinateParser' (x + 1) y),
          -- Catches the EOF
          return Map.empty
        ]

------------ COMBINATORS ------------

-- Takes a parser and a separator. Parses one instance of the parser before the separator and one afterwards, returning the parsed values as a pair.
around :: Parser a -> Parser b -> Parser (a, a)
around p sep = do
  a <- p
  sep
  b <- p
  return (a, b)

asText :: Parser String -> Parser Text
asText = fmap pack

takeText :: Parser Char -> Parser Text
takeText = asText . many

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 term op = scan
  where scan = term >>= rest
        rest x = (op <*> pure x <*> scan) <|> pure x

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 term op = scan
  where scan = term >>= rest
        rest x = ((op <*> pure x <*> term) >>= rest) <|> pure x


countMatch :: Parser a -> Parser Int
countMatch = fmap List.length . many1
