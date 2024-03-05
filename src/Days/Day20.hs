module Days.Day20 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Util.Util as U
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Util.Coordinates (CoordinateMap)
import Util.Parsers (coordinateParser)
import Data.Functor (($>))
import Data.Bifunctor
import Data.Graph.Inductive
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.fromList <$> many1 tile
  where tile = do
          c <- peekChar
          "Tile "
          id <- decimal
          ":"
          endOfLine
          t <- coordinateParser (choice [pure <$> "#", pure <$> "."]) 0
          c <- peekChar
          return (id, t)


------------ TYPES ------------
type Input = Map Int (CoordinateMap Text)

type OutputA = Int

type OutputB = Int

border :: CoordinateMap Text -> [Text]
border tile = map (borderText . flip filterKey tile) [(== 0) . fst
                                 , (== 0) . snd
                                 , (== ux) . fst
                                 , (== uy) . snd]
  where
    (ux, uy) = fst . Map.findMax $ tile

-- Assumes sharing border is not incidental
shareBorder :: CoordinateMap Text -> CoordinateMap Text -> Bool
shareBorder l r = l /= r && any (\b -> b `elem` border l || T.reverse b `elem` border l) (border r)

flipVertically :: CoordinateMap Text -> CoordinateMap Text
flipVertically m = Map.fromList . map (first reflect) . Map.toList $ m
  where reflect (x, y) = (x, uy - y)
        (_, uy) = fst . Map.findMax $ m

rotate :: CoordinateMap Text -> CoordinateMap Text
rotate m = Map.fromList . map (first rot) . Map.toList $ m
  where (ux, uy) = fst . Map.findMax $ m
        rot (x, y) = (y, ux - x)

allMaps :: CoordinateMap Text -> [CoordinateMap Text]
allMaps m = scanr (const rotate) m [0..2] ++ scanr (const rotate) (flipVertically m) [0..2]


filterKey :: Ord k => (k -> Bool) -> Map k a -> Map k a
filterKey f = Map.filterWithKey (\k v -> f k)


borderText :: CoordinateMap Text -> Text
borderText = mconcat . map snd . Map.toAscList

southBorder :: CoordinateMap Text -> Text
southBorder m = borderText . filterKey ((== uy) . snd) $ m
  where (_, uy) = fst . Map.findMax $ m

northBorder :: CoordinateMap Text -> Text
northBorder = borderText . filterKey ((== 0) . snd)

eastBorder :: CoordinateMap Text -> Text
eastBorder m = borderText . filterKey ((== ux) . fst) $ m
  where (ux, _) = fst . Map.findMax $ m

westBorder :: CoordinateMap Text -> Text
westBorder = borderText . filterKey ((== 0) . fst)


-- Orientate the map m to match the southern border of the map to the north. If no orientation can be found returns Nothing.
orientateNorth :: CoordinateMap Text -> CoordinateMap Text -> Maybe (CoordinateMap Text)
orientateNorth north m = if m `elem` northMaps then Nothing else find (\m' -> northBorder m' == southBorder north) . allMaps $ m
  where
    northMaps = allMaps north
    (_, uy) = fst . Map.findMax $ m


-- Orientate the map to match the western border of m to the eastern border of west. If no orientation can be found return Nothing.
orientateWest :: CoordinateMap Text -> CoordinateMap Text -> Maybe (CoordinateMap Text)
orientateWest west m = if m `elem` westMaps then Nothing else find (\m' -> westBorder m' == eastBorder west) . allMaps $ m
  where
    westMaps = allMaps west
    (ux, _) = fst . Map.findMax $ m


topLeft :: [CoordinateMap Text] -> [Int]
topLeft tiles = map (\m -> U.count (\m' -> m /= m' && (westBorder m' == eastBorder m || northBorder m' == southBorder m || southBorder m' == northBorder m || eastBorder m' == westBorder m)) ms) ms
                     -- && all (\m' -> eastBorder m' /= westBorder m) ms
                     -- && all (\m' -> southBorder m' /= northBorder m) ms)
  where ms = concatMap allMaps tiles



orientate :: [CoordinateMap Text] -> CoordinateMap Text -> Maybe (CoordinateMap Text)
orientate neighbours m = find (\m' -> any (shareBorder m') others) . allMaps $ m
  where others = filter (/= m) neighbours

neighbours :: Input -> CoordinateMap Text -> [Int]
neighbours m u = Map.keys . Map.filter (shareBorder u) $ m


showTile :: CoordinateMap Text -> String
showTile m = unlines [line y | y <- [0..uy]]
  where
    (ux, uy) = fst . Map.findMax $ m
    line :: Int -> String
    line y = concat [T.unpack . fromMaybe "!" . Map.lookup (x, y) $ m  | x <- [0..ux]]
mapKey :: (Ord k, Ord k') => (k -> k') -> Map k a -> Map k' a
mapKey f = Map.fromList . map (first f) . Map.toList

glue :: [[CoordinateMap Text]] -> CoordinateMap Text
glue tiles = Map.unions [mapKey (shift (ix * ux) (iy * uy)) ((tiles !! iy) !! ix) | ix <- [0..n - 1], iy <- [0..n - 1]]
  where
        (ux, uy) = fst . Map.findMax . head . head $ tiles
        n =  length tiles
        shift dx dy (x, y) = (x + dx, y + dy)
------------ PART A ------------
partA :: Input -> OutputA
partA tileMap = product . Map.keys . Map.filter ((== 2) . share) $ tileMap
  where
    share tile = U.count (shareBorder tile) (filter (/= tile) tiles)
    tiles = Map.elems tileMap
-- Check if a sea monster begins at a chosen x y coordinate pair
seaMonster :: CoordinateMap Text -> Int -> Int -> Bool
seaMonster m x y = all ((== Just "#") . (`Map.lookup` m) . bimap (+x) (+y)) [(0, 18), (1, 0), (1, 5), (1, 6), (1, 11), (1, 12), (1, 17), (1,18), (1, 19), (2, 1), (2, 4), (2, 7), (2, 10), (2, 13), (2, 16)]
seaMonsterSize :: Int
seaMonsterSize = length [(0, 18), (1, 0), (1, 5), (1, 6), (1, 11), (1, 12), (1, 17), (1,18), (1, 19), (2, 1), (2, 4), (2, 7), (2, 10), (2, 13), (2, 16)]
------------ PART B ------------
partB :: Input -> OutputB
partB tileMap = roughness
  where
    -- Fix one corner because it doesn't matter
    tiles = Map.elems tileMap
    share tile = U.count (shareBorder tile) (filter (/= tile) tiles)
    n = floor . sqrt . fromIntegral . Map.size $ tileMap
    corners = filter ((== 2) . share) tiles
    otherBorders = do
      m <- concatMap allMaps . filter (`notElem` corners) $ tiles
      border m
    orientate corner = find (\c -> (northBorder c `notElem` otherBorders) && (westBorder c `notElem` otherBorders) ) $ allMaps corner
    (ux, uy) = fst . Map.findMax . head $ tiles
    shave = filterKey (\(x, y) -> not $ x == 0 || y == 0 || x == ux || y == uy)
    puzzle = [[go x y | x <- [0..(n - 1)]] | y <- [0 .. (n - 1)]]
    go 0 0 = fromJust . orientate . head $ corners
    go 0 y = fromJust . U.findMaybe (orientateNorth (head $ puzzle !! (y - 1))) $ tiles
    go x y = fromJust . U.findMaybe (orientateWest ((puzzle !! y) !! (x - 1))) $ tiles
    dec x = x - 1
    image = mapKey (bimap dec dec) . glue $ map (map shave) puzzle
    (ix, iy) = fst . Map.findMax $ image
    seaMonsterImage = fromJust . find (\m -> or [seaMonster m x y | x <- [0..ix], y <- [0..iy]]) . allMaps $ image
    roughness = Map.size (Map.filter (== "#") seaMonsterImage) - seaMonsterSize * U.count (uncurry $ seaMonster seaMonsterImage) [(x, y) | x <- [0..ix], y <- [0..iy]]
