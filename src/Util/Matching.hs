
module Util.Matching (matching) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List ( foldl' )
import Data.Tuple (swap)

matching :: (Ord a, Ord b) => S.Set (a,b) -> M.Map a b
matching g = invMap $ opt (M.keys fwd, []) fwd M.empty
    where
     fwd = foldl' (\m (x,y) -> M.insertWith (++) x [y] m) M.empty $ S.toList g
     invMap = M.fromList . map swap . M.toList

opt :: (Ord a, Ord b) => ([a],[a]) -> M.Map a [b] -> M.Map b a -> M.Map b a
opt (x:free,failed) fwd mat
  = either (flip (opt (free,x:failed)) mat) (opt (free++failed,[]) fwd)
    $ right fwd [] x
  where
    right rem path x
      = maybe (Left rem) (left $ M.delete x rem) $ M.lookup x rem
      where
        left rem [] = Left rem
        left rem (y:ys)
          = maybe
            (Right $ foldr (uncurry $ flip M.insert) mat path') --A
            (either (flip left ys) Right . right rem path')     --B
            (M.lookup y mat)
          where
            path' = (x,y):path
opt ([],failed) fwd mat = mat
