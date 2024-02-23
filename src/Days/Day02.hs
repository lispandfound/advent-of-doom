module Days.Day02 (runDay) where

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
import qualified Util.Parsers as Up
import Data.Text as Text
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy rule endOfLine
  where rule = do
          (low, high) <- Up.around decimal "-"
          space
          target <- letter
          ": "
          password <- Up.takeText letter
          return $ Rule low high target password

------------ TYPES ------------
data Rule = Rule {
  low :: Int
  , high :: Int
  , target :: Char
  , password :: Text
                   } deriving (Show, Eq)
type Input = [Rule]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = U.count ruleMatch
  where ruleMatch r = let c = Text.count (Text.singleton $ target r) (password r) in
           (low r <= c && c <= high r)


------------ PART B ------------
partB :: Input -> OutputB
partB = U.count ruleMatch
  where ruleMatch r = (password r `index` (low r - 1) == target r) `xor` (password r `index` (high r - 1) == target r)
        xor = (/=)
