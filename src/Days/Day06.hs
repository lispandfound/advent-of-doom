module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Util.Util as U
import Util.Coordinates as C
import Util.Parsers as P

import qualified Program.RunDay as R (runDay, Day)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Attoparsec.Text hiding (D, takeWhile, take)
import Data.Void
import Data.Functor (($>))
import Data.Bifunctor
import Control.Comonad
import Control.Comonad.Trans.Store

{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

data Marker = Obstacle | Guard deriving (Show, Eq)

data Heading = U | L | D | R deriving (Show, Eq, Ord)

headingToVector :: Heading -> (Int, Int)
headingToVector U = (0, -1)
headingToVector L = (-1, 0)
headingToVector R = (1, 0)
headingToVector D = (0, 1)

rotateRight :: Heading -> Heading
rotateRight U = R
rotateRight R = D
rotateRight D = L
rotateRight L = U

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser (("#" $> pure Obstacle) <|> ("^" $> pure Guard) <|> (anyChar $> Nothing)) 0 >>= setup
  where
    setup m = liftMaybe $ do
        pos <- find ((== Guard) . snd) . Map.toList $ m
        bounds <- mapBoundingBox m
        return $ Input m (fst pos) bounds

------------ TYPES ------------
data Input = Input {obstacleMap :: CoordinateMap Marker, initialPosition :: (Int, Int), bounds :: (Int, Int, Int, Int)} deriving (Show, Eq)

type OutputA = Int

type OutputB = Int

walkFrom :: CoordinateMap Marker -> BoundingBox -> (Int, Int) -> [((Int, Int), Heading)]
walkFrom m bounds initialPosition = takeWhile (inBoundingBox bounds . fst) . go mempty U . seek initialPosition . mapToStore $ m
  where
    go seen heading store
        | (pos store, heading) `Set.member` seen = [(pos store, heading)]
        | otherwise =
            (pos store, heading) : case at dx dy store of
                Just Obstacle -> go (Set.insert (pos store, heading) seen) (rotateRight heading) store
                _ -> go (Set.insert (pos store, heading) seen) heading (move dx dy store)
      where
        (dx, dy) = headingToVector heading

------------ PART A ------------
partA :: Input -> OutputA
partA (Input{obstacleMap = m, initialPosition = initialPosition, bounds = bounds}) = Set.size . Set.fromList . map fst $ walkFrom m bounds initialPosition

------------ PART B ------------
partB :: Input -> OutputB
partB (Input{obstacleMap = m, initialPosition = initialPosition, bounds = bounds}) =
    length
        . filter (not . hasLoop)
        . map
            ( \p ->
                walkFrom (Map.insert p Obstacle m) bounds initialPosition
            )
        $ trialPositions
  where
    trialPositions = toList . Set.fromList . map fst $ walkFrom m bounds initialPosition
    hasLoop walk = Set.size (Set.fromList walk) == length walk
