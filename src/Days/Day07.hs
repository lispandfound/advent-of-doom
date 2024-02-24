module Days.Day07 (runDay) where

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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Graph.Inductive
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Tuple (swap)
import Control.Applicative
import Data.Tree
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  edges <- concat <$> rule `sepBy` endOfLine
  let nodes = Set.unions . map (\(u, v, _) -> Set.fromList [u, v]) $ edges
      lnodes = zip [0..] (Set.toList nodes)
      idLookup = Map.fromList . map swap $ lnodes
      ledges = catMaybes $ map (\(u, v, b) -> (,,) <$> Map.lookup u idLookup <*> Map.lookup v idLookup <*> pure b) edges
    in
    return $ mkGraph lnodes ledges

  where
        rule = do
          source <- label
          " bags contain "
          edges <- ("no other bags" *> pure []) <|> (edge `sepBy` ", ")
          "."
          return [(source, v, count) | (v, count) <- edges]
        label = mconcat <$> sequence [word, " ", word]
        edge = do
          c <- decimal
          " "
          bag <- label
          " "
          "bags" <|> "bag"
          return (bag, c)
        word = Up.takeText letter

------------ TYPES ------------
type Bag = Text
type Count = Int
type Input = Gr Bag Count

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA inp = (\x -> x - 1) . length . rdfs [shinyGold] $ inp
  where Just shinyGold = fmap fst . find ((== "shiny gold") . snd) . labNodes $ inp

------------ PART B ------------
partB :: Input -> OutputB
partB g = Map.findWithDefault 0 shinyGold costMap
  where Just shinyGold = fmap fst . find ((== "shiny gold") . snd) . labNodes $ g
        component = Set.fromList . reachable shinyGold $ g
        top = filter (`Set.member` component) . topsort $ g
        costMap = foldr go mempty top
          where go v m = Map.insert v (sum . map (\(u, c) -> c * (1 + Map.findWithDefault 0 u m)) $ lsuc g v) m
