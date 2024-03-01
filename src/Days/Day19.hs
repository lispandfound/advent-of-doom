module Days.Day19  where

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
import Control.Applicative
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Either
import Util.Util as U
import Control.Monad (void)
import Data.Text (Text)
import Util.Parsers (countMatch)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Input <$> fmap arrangeRules rules  <*> ((endOfLine *> endOfLine *> tests )<* endOfInput)
  where
    rules = rule `sepBy` endOfLine
    rule = do
            ix <- decimal
            ": "
            term <- literal <|> plusRule <|> orRule <|> follows
            return (ix, term)
    plusRule = do
            r <- decimal
            "+"
            return $ P r
    orRule = do
            r1s <- decimal `sepBy` char ' '
            " | "
            r2s <- decimal `sepBy` char ' '
            return $ O r1s r2s
    follows = do
            r1s <- decimal `sepBy1` char ' '
            return $ F r1s
    literal = do
            "\""
            c <- anyChar
            "\""
            return $ L c
    tests = test `sepBy` endOfLine
    test = takeTill (== '\n')
    arrangeRules = buildRuleParser . map snd . sortOn fst
    buildRuleParser rules = rts
      where
        rts = map (\case
                          L c -> void $ char c
                          F rs -> mapM_ (try . (rts !!)) rs
                          P r -> void . try . many1 . try $  rts !! r
                          O r1s r2s -> mapM_ (try . (rts !!)) r1s <|>  mapM_ (try . (rts !!)) r2s) rules


data Rule = L Char | F [Int] | P Int | O [Int] [Int]

------------ TYPES ------------
data Input = Input {
  inputRules :: [Parser ()]
  , inputTests :: [Text]
                   }

instance Show Input where
  show (Input _ tests) = "Input <Parser> " ++ show tests

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (Input rules tests) = U.count (isRight . parseOnly (head rules *> endOfInput))  tests

------------ PART B ------------
partB :: Input -> OutputB
partB (Input rules tests) = U.count (isRight . parseOnly (check *> endOfInput)) tests
  where
    check = do
      match42 <- countMatch (rules !! 42)
      match32 <- countMatch (rules !! 31)
      if match42 > match32 then pure () else fail "Cannot match enough rule 42"
