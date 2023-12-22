module SparseMatrix where

import GHC.Float

data Matrix m
  = SquareMatrix Int (Matrix m) (Matrix m) (Matrix m) (Matrix m)
  | UpperRightTriangularMatrix Int (Matrix m) (Matrix m) (Matrix m)
  | UnitMatrix m
  | Empty
  deriving (Eq)

newUpperRightTriangularMatrix :: Int -> Matrix m
newUpperRightTriangularMatrix n
  | nsq < 2 = Empty
  | nsq == 2 = UpperRightTriangularMatrix nsq Empty Empty Empty
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
  | nsq < 2 = Empty
  | nsq == 2 = SquareMatrix nsq Empty Empty Empty Empty
  | otherwise = SquareMatrix nsq smallM smallM smallM smallM
  where
    smallM = newSquareMatrix (float2Int (int2Float n / 2.0))
    nsq = nextClosestSquare n

-- v :: SquareMatrix a -> SquareMatrix a -> SquareMatrix a -> SquareMatrix a
-- v a y b = undefined

-- printMatrix :: Matrix mx -> (String, Int)
-- printMatrix a n = undefined

data Tree = Leaf Int | Node Tree Tree

repmin :: Tree -> Tree
repmin t = tr where (mn, tr) = walk mn t

walk :: Int -> Tree -> (Int, Tree)
walk mn (Leaf n) = (n, Leaf mn)
walk mn (Node t1 t2) = (n1 `min` n2, Node tr1 tr2)
  where
    (n1, tr1) = walk mn t1
    (n2, tr2) = walk mn t2

fixLength :: Int -> [Char] -> [Char]
fixLength n s = replicate (n - length s) ' ' ++ s ++ " "

concatQuads :: String -> String -> String -> String -> String
concatQuads a b c d = upper ++ lower
  where
    lhf = map (++ " ") . lines
    rhf = map (++ "\n") . lines
    a_ = lhf a
    b_ = rhf b
    c_ = lhf c
    d_ = rhf d

    upper = concat $ zipWith (++) a_ b_
    lower = concat $ zipWith (++) c_ d_

applyF :: [t -> c] -> [t] -> [c]
applyF = zipWith (\a b -> a b)

showMx :: Show m => Matrix m -> String
showMx m = s where (mx, s) = walkMx mx m

walkMx :: Show m => Int -> Matrix m -> (Int, String)
walkMx mx Empty = (0, fixLength mx "")
walkMx mx (UnitMatrix m) = (length s, fixLength mx s)
  where
    s = show m
walkMx mx (SquareMatrix _ a b c d) = (foldr max 0 ns, concatQuads (head strs) (strs !! 1) (strs !! 2) (strs !! 3))
  where
    (ns, strs) = unzip $ map (mxMaybe (0, "") (walkMx mx)) [a, b, c, d]
walkMx _ _ = undefined

mxMaybe :: p -> (Matrix m -> p) -> Matrix m -> p
mxMaybe b _ Empty = b
mxMaybe _ f m = f m

mat :: Matrix Int
mat = SquareMatrix 4 a a a a
  where
    a = SquareMatrix 2 b c d e
    b = UnitMatrix 4 :: Matrix Int
    c = UnitMatrix 6 :: Matrix Int
    d = UnitMatrix 9 :: Matrix Int
    e = UnitMatrix 200 :: Matrix Int