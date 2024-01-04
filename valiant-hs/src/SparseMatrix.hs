{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- TODO: find a way to remove UndecidableInstances (seems like a bad idea).  Likely this would be by using `IsZeroType`
{-# LANGUAGE UndecidableInstances #-}

module SparseMatrix where

import Ring

-------------------------------- Data ------------------------------------------

data N = Z | S N deriving (Eq, Show)

data Matrix (n :: N) m where
  SquareMatrix :: Matrix ('S n) m -> Matrix ('S n) m -> Matrix ('S n) m -> Matrix ('S n) m -> Matrix ('S ('S n)) m
  UpperRightTriangularMatrix :: Matrix ('S n) m -> Matrix ('S n) m -> Matrix ('S n) m -> Matrix ('S ('S n)) m
  UnitMatrix :: m -> Matrix ('S 'Z) m
  Empty :: Matrix n m

-- TODO: this should not remain unused
class IsZeroType n where
  isZeroType :: Matrix n m -> Bool

instance IsZeroType 'Z where
  isZeroType _ = True

instance IsZeroType n => IsZeroType ('S n) where
  isZeroType _ = False

-- Test stuff

mat1 n = UpperRightTriangularMatrix t2 as t2
  where
    a = SquareMatrix b c d e
    b = (+ 1) <$> UnitMatrix n -- :: Matrix a
    c = UnitMatrix n -- :: Matrix a
    d = UnitMatrix n -- :: Matrix a
    e = UnitMatrix n -- :: Matrix a
    as = SquareMatrix a (subtract 1 <$> a) a (subtract 1 <$> a)
    t = UpperRightTriangularMatrix (UnitMatrix n) (UnitMatrix n) (UnitMatrix n)
    t2 = UpperRightTriangularMatrix t a t

mat2 n = SquareMatrix (mat1 n) (mat1 n) (mat1 n) (mat1 n)

------------------------ Functors and things -----------------------------------

instance Foldable (Matrix n) where
  foldr :: (a -> b -> b) -> b -> Matrix n a -> b
  foldr f acc (SquareMatrix a b c d) = foldr (flip $ foldr f) acc [a, b, c, d]
  foldr f acc (UpperRightTriangularMatrix a b d) = foldr (flip $ foldr f) acc [a, b, d]
  foldr f acc (UnitMatrix a) = f a acc
  foldr _ acc Empty = acc

instance Functor (Matrix n) where
  fmap f (SquareMatrix a b c d) = SquareMatrix (fmap f a) (fmap f b) (fmap f c) (fmap f d)
  fmap f (UpperRightTriangularMatrix a b d) = UpperRightTriangularMatrix (fmap f a) (fmap f b) (fmap f d)
  fmap f (UnitMatrix a) = UnitMatrix (f a)
  fmap _ Empty = Empty

instance Ring a => Ring (Matrix 'Z a) where
  zero = Empty
  add = undefined
  mul = undefined

instance (Ring a, Applicative (Matrix ('S n))) => Ring (Matrix ('S ('S n)) a) where
  zero = Empty
  add = (<*>) . (add <$>)
  mul = (<*>) . (mul <$>)

instance Applicative (Matrix 'Z) where
  pure = const Empty
  _ <*> _ = Empty

instance Applicative (Matrix ('S 'Z)) where
  pure = UnitMatrix
  (UnitMatrix a) <*> (UnitMatrix b) = UnitMatrix $ a b
  Empty <*> _ = Empty
  _ <*> Empty = Empty

instance Applicative (Matrix ('S n)) => Applicative (Matrix ('S ('S n))) where
  pure m = SquareMatrix (pure m) (pure m) (pure m) (pure m)
  (SquareMatrix a b c d) <*> (SquareMatrix e f g h) = SquareMatrix (a <*> e) (b <*> f) (c <*> g) (d <*> h)
  (UpperRightTriangularMatrix a b d) <*> (UpperRightTriangularMatrix e f h) = UpperRightTriangularMatrix (a <*> e) (b <*> f) (d <*> h)
  (UpperRightTriangularMatrix a b d) <*> (SquareMatrix e f _ h) = UpperRightTriangularMatrix (a <*> e) (b <*> f) (d <*> h)
  (SquareMatrix a b _ d) <*> (UpperRightTriangularMatrix e f h) = UpperRightTriangularMatrix (a <*> e) (b <*> f) (d <*> h)
  Empty <*> _ = Empty
  _ <*> Empty = Empty

instance (Applicative (Matrix n), Semigroup a) => Semigroup (Matrix n a) where
  (<>) = (<*>) . ((<>) <$>)

instance (Applicative (Matrix n), Semigroup a) => Monoid (Matrix n a) where
  mempty = Empty

---------------------------------- Show ----------------------------------------

-- Richard Bird repmin
class (Show a) => BirdWalk a where
  walk :: Int -> a -> (Int, String)

instance (Show m) => Show (Matrix 'Z m) where
  show m = s
    where
      (topMax, s) = walk topMax m

instance (Show m) => BirdWalk (Matrix 'Z m) where
  walk topMax Empty = (0, concat . replicate len $ replicate ((topMax + 1) * len) ' ' ++ "\n")
    where
      len = 1

instance (Show m, Show (Matrix ('S 'Z) m)) => BirdWalk (Matrix ('S 'Z) m) where
  walk topMax (UnitMatrix m) = (length s, fixLength topMax s)
    where
      s = show m
      fixLength n x
        | len == 0 = replicate n '+'
        | otherwise = replicate md ' ' ++ x ++ replicate dv ' '
        where
          len = length x
          (dv, md) = n `divMod` len
  --          Empty
  walk topMax mat = (0, concat . replicate (size mat) $ replicate ((topMax + 1) * size mat) ' ' ++ "\n")

instance
  (Show m, Show (Matrix ('S ('S n)) m), Size (Matrix ('S n) m), BirdWalk (Matrix ('S n) m)) =>
  BirdWalk (Matrix ('S ('S n)) m)
  where
  walk topMax mat = case mat of
    (SquareMatrix a b c d) -> (foldr max 0 ns, concatQuads sa sb sc sd)
      where
        (ns, [sa, sb, sc, sd]) = unzip $ map (walk topMax) [a, b, c, d]
    (UpperRightTriangularMatrix a b d) -> (foldr max 0 ns, concatQuads sa sb sc sd)
      where
        (ns, [sa, sb, sc, sd]) = unzip $ map (walk topMax) [a, b, Empty, d]
    Empty -> (0, concat . replicate (size mat) $ replicate ((topMax + 1) * size mat) ' ' ++ "\n")

concatQuads :: String -> String -> String -> String -> String
concatQuads a b c d = concatMap pairConcat [(a, b), (c, d)]
  where
    lhf = lines
    rhf = map (++ "\n") . lines
    pairConcat (x, y) = concat $ zipWith (++) (lhf x) (rhf y)

instance
  (Show m, BirdWalk (Matrix ('S n) m), BirdWalk (Matrix n m), Size (Matrix n m)) =>
  Show (Matrix ('S n) m)
  where
  show mat = s
    where
      (topMax, s) = walk topMax mat

class Size a where
  size :: a -> Int

instance Size (Matrix 'Z m) where
  size Empty = 0

instance Size (Matrix ('S 'Z) m) where
  size _ = 1

instance (Size (Matrix ('S n) m)) => Size (Matrix ('S ('S n)) m) where
  size m = case m of
    SquareMatrix a b _ _ -> size a + size b
    UpperRightTriangularMatrix a b _ -> size a + size b
    Empty -> 2 * n
      where
        helper :: Matrix ('S n) m -> Matrix n m
        helper _ = Empty
        n = size $ helper m

---------------------------- Constructors --------------------------------------

nextClosestSquare :: (Ord a, Num a) => a -> a
nextClosestSquare n =
  head $ dropWhile (< n) [2 ^ x | x <- ([0 ..] :: [Int])]

-- newUpperRightTriangularMatrix :: Show m => Int -> Matrix n m
-- newUpperRightTriangularMatrix n
--   | nsq < 2 = Empty 0
--   | nsq == 2 = UpperRightTriangularMatrix (Empty smsq) (Empty smsq) (Empty smsq)
--   | otherwise = UpperRightTriangularMatrix smallT smallSQ smallT
--   where
--     smallT = newUpperRightTriangularMatrix (float2Int (int2Float n / 2.0))
--     smallSQ = newSquareMatrix (float2Int (int2Float n / 2.0))
--     smsq = float2Int (int2Float nsq / 2.0)
--     nsq = nextClosestSquare n

u2s :: Matrix n m -> Matrix n m
u2s (UpperRightTriangularMatrix a b d) = SquareMatrix a b Empty d
u2s _ = undefined

-- u2s (UpperRightTriangularMatrix a b d) = SquareMatrix a b (Empty $ size a) d

-- newSquareMatrix :: Int -> Matrix n m
-- newSquareMatrix n
--   | nsq < 2 = Empty 0
--   | nsq == 2 = SquareMatrix (Empty smsq) (Empty smsq) (Empty smsq) (Empty smsq)
--   | otherwise = newSquareMatrix_ smallM
--   where
--     nsq = nextClosestSquare n
--     smsq = float2Int (int2Float nsq / 2.0)
--     smallM = newSquareMatrix smsq

newSquareMatrix_ :: Matrix ('S n) m -> Matrix ('S ('S n)) m
newSquareMatrix_ m = SquareMatrix m m m m

------------------------------- Algorithm --------------------------------------

v :: Matrix n a -> Matrix n b
v SquareMatrix {} = undefined
v UpperRightTriangularMatrix {} = undefined
v (UnitMatrix _) = undefined
v Empty = undefined
