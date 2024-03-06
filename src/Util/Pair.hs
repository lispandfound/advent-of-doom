module Util.Pair where
import Control.Applicative

newtype Pair a = Pair {getPair :: (a, a)}
  deriving (Show, Eq, Ord, Functor, Traversable)

instance Foldable Pair where
  foldMap f (Pair (x, y))  = f x `mappend` f y
  length _ = 2

instance Applicative Pair where
  pure x = Pair (x, x)
  (Pair (f, g)) <*> (Pair (x, y)) = Pair (f x, g y)

instance (Semigroup a) => Semigroup (Pair a) where
  (Pair (a, b)) <> (Pair (c, d)) = Pair (a <> c, b <> d)

instance (Monoid a) => Monoid (Pair a) where
  mempty = Pair (mempty, mempty)


instance (Num a) => Num (Pair a) where
  fromInteger = pure . fromInteger
  (+) = (<+>)
  (-) = (<->)
  x * y = (*) <$> x <*> y
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate

(<->) :: Num a => Pair a -> Pair a -> Pair a
x <-> y = (-) <$> x <*> y

(<+>) :: Num a => Pair a -> Pair a -> Pair a
x <+> y = (+) <$> x <*> y

manhattan :: Num a => Pair a -> a
manhattan = sum . abs

first :: (a -> a) -> Pair a -> Pair a
first f = liftA2 id (Pair (f,id))

second :: (a -> a) -> Pair a -> Pair a
second g = liftA2 id (Pair (id,g))

pmap :: ((a, a) -> (c, c)) -> Pair a -> Pair c
pmap f = Pair . f . getPair
