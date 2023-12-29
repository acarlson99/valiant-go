{-# LANGUAGE InstanceSigs #-}

module SparseMatrix where

import GHC.Float

data Matrix m
  = SquareMatrix Int (Matrix m) (Matrix m) (Matrix m) (Matrix m)
  | UpperRightTriangularMatrix Int (Matrix m) (Matrix m) (Matrix m)
  | UnitMatrix m
  | Empty Int -- (Empty 2) is same size as (SquareMatrix 2 _ _ _ _); placeholder value
  deriving (Eq)

instance Foldable Matrix where
  foldr :: (a -> b -> b) -> b -> Matrix a -> b
  -- foldr f acc (SquareMatrix _ a b c d) = fn d . fn c . fn b $ fn a acc
  foldr f acc (SquareMatrix _ a b c d) = foldr (flip $ foldr f) acc [a, b, c, d]
  foldr f acc (UpperRightTriangularMatrix _ a b d) = foldr (flip $ foldr f) acc [a, b, d]
  foldr f acc (UnitMatrix a) = f a acc
  foldr _ acc (Empty _) = acc

instance Functor Matrix where
  fmap :: (a -> b) -> Matrix a -> Matrix b
  fmap f (SquareMatrix n a b c d) = SquareMatrix n (fmap f a) (fmap f b) (fmap f c) (fmap f d)
  fmap f (UpperRightTriangularMatrix n a b d) = UpperRightTriangularMatrix n (fmap f a) (fmap f b) (fmap f d)
  fmap f (UnitMatrix a) = UnitMatrix (f a)
  fmap _ (Empty size) = Empty size

instance Applicative Matrix where
  pure :: a -> Matrix a
  pure = UnitMatrix
  (<*>) :: Matrix (a -> b) -> Matrix a -> Matrix b
  (SquareMatrix n1 f1 f2 f3 f4) <*> (SquareMatrix n2 a b c d)
    | n1 /= n2 = error "Matrix size mismatch!"
    | otherwise = SquareMatrix n1 (f1 <*> a) (f2 <*> b) (f3 <*> c) (f4 <*> d)
  (UpperRightTriangularMatrix n1 f1 f2 f3) <*> (UpperRightTriangularMatrix n2 a b c)
    | n1 /= n2 = error "Matrix size mismatch!"
    | otherwise = UpperRightTriangularMatrix n1 (f1 <*> a) (f2 <*> b) (f3 <*> c)
  (UnitMatrix f) <*> (UnitMatrix x) = UnitMatrix (f x)
  (Empty n1) <*> (Empty n2)
    | n1 /= n2 = error "Matrix size mismatch!"
    | otherwise = Empty n2
  _ <*> _ = error "Matrix type mismatch!"

instance (Semigroup a) => Semigroup (Matrix a) where
  (<>) :: Matrix a -> Matrix a -> Matrix a
  (<>) = (<*>) . ((<>) <$>)

instance (Monoid a) => Monoid (Matrix a) where
  mempty = Empty 0

newUpperRightTriangularMatrix :: Show m => Int -> Matrix m
newUpperRightTriangularMatrix n
  | nsq < 2 = Empty 0
  | nsq == 2 = UpperRightTriangularMatrix nsq (Empty nsq) (Empty nsq) (Empty nsq)
  | otherwise = UpperRightTriangularMatrix nsq smallT smallSQ smallT
  where
    smallT = newUpperRightTriangularMatrix (float2Int (int2Float n / 2.0))
    smallSQ = newSquareMatrix (float2Int (int2Float n / 2.0))
    nsq = nextClosestSquare n

nextClosestSquare :: (Ord a, Num a) => a -> a
nextClosestSquare n =
  head $ dropWhile (< n) [2 ^ x | x <- ([0 ..] :: [Int])]

newSquareMatrix :: Int -> Matrix m
newSquareMatrix n
  | nsq < 2 = Empty 0
  | nsq == 2 = SquareMatrix nsq (Empty smsq) (Empty smsq) (Empty smsq) (Empty smsq)
  | otherwise = SquareMatrix nsq smallM smallM smallM smallM
  where
    nsq = nextClosestSquare n
    smsq = float2Int (int2Float nsq / 2.0)
    smallM = newSquareMatrix smsq

-- v :: SquareMatrix a -> SquareMatrix a -> SquareMatrix a -> SquareMatrix a
-- v a y b = undefined

instance Show m => Show (Matrix m) where
  show :: Show m => Matrix m -> String
  show mat = str
    where
      (topMax, str) = walk mat
      -- Richard Bird repmin-like function for printing a matrix
      walk :: (Show m) => Matrix m -> (Int, String)
      walk (Empty size) = (0, concat . replicate size $ replicate ((topMax + 1) * size) ' ' ++ "\n")
      walk (UnitMatrix m) = (length s, fixLength topMax s)
        where
          s = show m
          fixLength n x
            | len == 0 = replicate n '+'
            | otherwise = replicate md ' ' ++ x ++ replicate dv ' '
            where
              len = length x
              (dv, md) = n `divMod` len
      walk (SquareMatrix _ a b c d) = (foldr max 0 ns, concatQuads sa sb sc sd)
        where
          (ns, [sa, sb, sc, sd]) = unzip $ map walk [a, b, c, d]
      walk (UpperRightTriangularMatrix size a b d) = (foldr max 0 ns, concatQuads sa sb sc sd)
        where
          (ns, [sa, sb, sc, sd]) = unzip $ map walk [a, b, Empty (float2Int (int2Float size / 2.0)), d]

      concatQuads :: String -> String -> String -> String -> String
      concatQuads a b c d = concatMap pairConcat [(a, b), (c, d)]
        where
          lhf = lines
          rhf = map (++ "\n") . lines
          pairConcat (x, y) = concat $ zipWith (++) (lhf x) (rhf y)

mat1 :: Show a => a -> Matrix a
mat1 n = UpperRightTriangularMatrix 8 t2 as t2
  where
    a = SquareMatrix 2 b c d e
    b = UnitMatrix n -- :: Matrix a
    c = UnitMatrix n -- :: Matrix a
    d = UnitMatrix n -- :: Matrix a
    e = UnitMatrix n -- :: Matrix a
    as = SquareMatrix 4 a a a a
    t = UpperRightTriangularMatrix 2 (UnitMatrix n) (UnitMatrix n) (UnitMatrix n)
    t2 = UpperRightTriangularMatrix 4 t a t

sqm :: (Show a) => a -> Matrix a
sqm n = SquareMatrix 16 (mat1 n) (mat1 n) (mat1 n) (mat1 n)