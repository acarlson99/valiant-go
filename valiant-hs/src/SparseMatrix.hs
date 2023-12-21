module SparseMatrix where

import           GHC.Float

data SquareMatrix a = M
  { matrixSize :: Int
  , ul         :: Maybe a
  , ur         :: Maybe a
  , bl         :: Maybe a
  , br         :: Maybe a
  }
  deriving Eq

instance Show a => Show (SquareMatrix a) where
  show _ = undefined

nextClosestSquare :: Int -> Int
nextClosestSquare n =
  head $ dropWhile (< n) [ float2Int (2 ** x) | x <- [1 ..] ]

newSquareMatrix :: Int -> SquareMatrix a
newSquareMatrix n = M (nextClosestSquare n) Nothing Nothing Nothing Nothing
