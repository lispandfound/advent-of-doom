module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (ord)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import qualified Util.Parsers as Up
import Control.Applicative
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text as P
import Data.Text as Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = List.filter (not . List.null) <$> sepBy record (endOfLine *> endOfLine)
  where record = Map.fromList <$> sepBy keyValue space
        keyValue = Up.around (Up.takeText (char '#' <|> digit <|> letter)) ":"

------------ TYPES ------------
type Input = [Map Text Text]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA  = U.count validPassport
  where validPassport p = Map.size p == 8 || (Map.size p == 7 && not (Map.member "cid" p))

------------ PART B ------------
partB :: Input -> OutputB
partB inp = U.count validPassport inp
  where validPassport p =  and [
          ecl p
          , byr p
          , iyr p
          , eyr p
          , hgt p
          , hcl p
          , pid p]
        context p = [
          ecl p
          , byr p
          , iyr p
          , eyr p
          , hgt p
          , hcl p
          , pid p]
        match p = either (const False) id . parseOnly (p <* endOfInput)
        ecl p = Map.lookup "ecl" p `List.elem` List.map Just ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        byr p = maybe False (match $ fmap (\yr -> 1920 <= yr && yr <= 2002) year) (Map.lookup "byr" p)
        iyr p = maybe False (match $ fmap (\yr -> 2010 <= yr && yr <= 2020) year) (Map.lookup "iyr" p)
        eyr p = maybe False (match $ fmap (\yr -> 2020 <= yr && yr <= 2030) year) (Map.lookup "eyr" p)
        year = List.foldl (\n d -> n*10 + ord d - ord '0') 0 <$> P.count 4 digit
        hgt p = maybe False (match height) $ Map.lookup "hgt" p
        height = do
          h <- decimal
          unit <- "cm" <|> "in"
          return $ unit == "cm" && h >= 150 && h <= 193 || unit == "in" && h >= 59 && h <= 76
        hcl p = maybe False (match colour) (Map.lookup "hcl" p)
        colour = "#" *> P.count 6 (satisfy (\c -> c >= 'a' && c <= 'f') <|> digit) *> pure True
        pid p = maybe False (match idn) $ Map.lookup "pid" p
        idn = P.count 9 digit *> pure True
