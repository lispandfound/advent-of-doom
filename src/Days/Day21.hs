module Days.Day21 (runDay) where

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
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Char (isAlpha)
import Util.Matching (matching)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = recipe `sepBy` endOfLine
  where
    word = takeWhile1 isAlpha
    recipe = do
          ingredients <- word `sepBy` " "
          " (contains "
          allergens <- word `sepBy` ", "
          ")"
          return (Set.fromList ingredients, Set.fromList allergens)


type Ingredient = Text
type Allergen = Text
------------ TYPES ------------
type Input = [(Set Ingredient, Set Allergen)]

type OutputA = Int

type OutputB = Text


ingredients :: Input -> [Ingredient]
ingredients = Set.toList . Set.unions . map fst
allergens :: Input -> [Ingredient]
allergens = Set.toList . Set.unions . map snd


allergenPossibilities :: Input -> Map Allergen (Set Ingredient)
allergenPossibilities recipes = U.mapFromFunction (\a -> U.intersections . map fst . filter ((a `Set.member`) . snd) $ recipes) $ as
  where as = allergens recipes

------------ PART A ------------
partA :: Input -> OutputA
partA recipes = sum . map (\i -> U.count ((i `elem`) . fst) recipes) $ safeIngredients
  where
        aPoss = Map.elems . allergenPossibilities $ recipes
        safeIngredients = filter (\i -> all (i `notElem`) aPoss) (ingredients recipes)

------------ PART B ------------
partB :: Input -> OutputB
partB = Text.intercalate "," . Map.elems . matching . Set.fromList . concatMap (\(a, is) -> map (a,) (Set.toList is)) . Map.toList . allergenPossibilities
