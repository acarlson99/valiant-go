module SparseMatrix where

import GHC.Float

data Matrix m
  = SquareMatrix Int (Matrix m) (Matrix m) (Matrix m) (Matrix m)
  | UpperRightTriangularMatrix Int (Matrix m) (Matrix m) (Matrix m)
  | UnitMatrix m
  | Empty Int -- (Empty 2) is same size as (SquareMatrix 2 _ _ _ _); placeholder value
  deriving (Eq)

newUpperRightTriangularMatrix :: Int -> Matrix m
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
  | nsq == 2 = SquareMatrix nsq (Empty nsq) (Empty nsq) (Empty nsq) (Empty nsq)
  | otherwise = SquareMatrix nsq smallM smallM smallM smallM
  where
    smallM = newSquareMatrix (float2Int (int2Float n / 2.0))
    nsq = nextClosestSquare n

-- v :: SquareMatrix a -> SquareMatrix a -> SquareMatrix a -> SquareMatrix a
-- v a y b = undefined

fixLength :: Int -> [Char] -> [Char]
fixLength n s = replicate md ' ' ++ s ++ replicate dv ' '
  where
    (dv, md) = n `divMod` length s

concatQuads :: String -> String -> String -> String -> String
concatQuads a b c d = upper ++ lower
  where
    lhf = lines
    rhf = map (++ "\n") . lines
    a_ = lhf a
    b_ = rhf b
    c_ = lhf c
    d_ = rhf d

    upper = concat $ zipWith (++) a_ b_
    lower = concat $ zipWith (++) c_ d_

applyF :: (String -> String -> a) -> [String] -> [String] -> [a]
applyF f (a : as) (b : bs) = f a b : applyF f as bs
applyF f [] (b : bs) = f (replicate (length b) ' ') b : applyF f [] bs
applyF f (a : as) [] = f a (replicate (length a) ' ') : applyF f as []
applyF _ [] [] = []

showMx :: Show m => Matrix m -> String
showMx m = s where (mx, s) = walkMx mx m

walkMx :: Show m => Int -> Matrix m -> (Int, String)
walkMx mx (Empty _) = (0, fixLength mx "")
walkMx mx (UnitMatrix m) = (length s, fixLength mx s)
  where
    s = show m
walkMx mx (SquareMatrix _ a b c d) = (foldr max 0 ns, concatQuads sa sb sc sd)
  where
    (ns, [sa, sb, sc, sd]) = unzip $ map (walkMx mx) [a, b, c, d]
walkMx mx (UpperRightTriangularMatrix size a b d) = (foldr max 0 ns, concatQuads sa sb (replicate size '\n') sd)
  where
    (ns, [sa, sb, sd]) = unzip $ map (walkMx mx) [a, b, d]

-- (ns, [sa, sb, sc, sd]) = unzip $ map (walkMx mx) [a, b, c, SquareMatrix 0 (Empty (float2Int (int2Float size / 2.0))) (Empty (float2Int (int2Float size / 2.0))) (Empty (float2Int (int2Float size / 2.0))) (Empty (float2Int (int2Float size / 2.0)))]

-- walkMx mx (UpperRightTriangularMatrix size a b c) = undefined
-- walkMx mx (UpperRightTriangularMatrix _ a b c) = concatQuads ()
--   where
--     (ns, strs) = unzip $ map (mxMaybe (0, "") (walkMx mx)) [a, b, c]

mxMaybe :: p -> (Matrix m -> p) -> Matrix m -> p
mxMaybe b _ (Empty _) = b
mxMaybe _ f m = f m

-- mat = SquareMatrix 4 a a a a
mat :: Matrix Int
mat = UpperRightTriangularMatrix 4 t a t
  where
    a = SquareMatrix 2 b c d e
    b = UnitMatrix 400 :: Matrix Int
    c = UnitMatrix 6 :: Matrix Int
    d = UnitMatrix 9 :: Matrix Int
    e = UnitMatrix 2 :: Matrix Int
    t = UpperRightTriangularMatrix 2 (UnitMatrix 1) (UnitMatrix 2) (UnitMatrix 3)
