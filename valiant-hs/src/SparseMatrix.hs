{-# LANGUAGE InstanceSigs #-}

module SparseMatrix where

import GHC.Float

data Matrix m
  = SquareMatrix Int (Matrix m) (Matrix m) (Matrix m) (Matrix m)
  | UpperRightTriangularMatrix Int (Matrix m) (Matrix m) (Matrix m)
  | UnitMatrix m
  | Empty Int -- (Empty 2) is same size as (SquareMatrix 2 _ _ _ _); placeholder value
  deriving (Eq)

instance Functor Matrix where
  fmap f (SquareMatrix s a b c d) = SquareMatrix s (f a) (f b) (f c) (f d)
  fmap f (UpperRightTriangularMatrix s a b d) = UpperRightTriangularMatrix s (f a) (f b) (f d)
  fmap f (UnitMatrix a) = UnitMatrix (f a)
  fmap _ (Empty size) = Empty size

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

newSquareMatrix :: Show m => Int -> Matrix m
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
  show mat = str
    where
      (topMax, str) = walk mat
      -- Richard Bird repmin-like function for printing a matrix
      walk :: Show m => Matrix m -> (Int, String)
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