{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Matrix.N where

import Data.Data
import Matrix.Sparse
import Nat
import Vec
import VecN

-- MatrixN data type is meant to provide a simpler interface for `Matrix n a`
data MatrixN a where
  MatrixN :: SNat n -> Matrix n a -> MatrixN a

instance Functor MatrixN where
  fmap f (MatrixN n m) = MatrixN n $ fmap f m

vecNToValiantMatrixN :: a -> VecN a -> MatrixN a
vecNToValiantMatrixN _ (VecN SZero VNil) = MatrixN SZero Empty
vecNToValiantMatrixN e (VecN l xs) =
  let h = snatHalf l
      (as, rest) = vecNSplitAt h e (VecN l xs)
      (b, cs') = vecNSplitFirst e rest
      (cs, _) = vecNSplitAt h e cs'
      ul = vecNToValiantMatrixN e as
      br = vecNToValiantMatrixN e cs
   in case ul of
        (MatrixN n ulm) -> case br of
          (MatrixN m brm) ->
            case n =?= m of
              Just Refl ->
                MatrixN (SSucc n) $ UpperRightTriangularMatrix ulm (sqMatWithValInBottomLeft b n) brm
              Nothing -> error "Recursing did not go well-- this should never happen"

matrixNTopRightMost :: MatrixN a -> Maybe a
matrixNTopRightMost (MatrixN (SSucc n) m) = case m of
  UpperRightTriangularMatrix _ a _ -> matrixNTopRightMost $ MatrixN n a
  SquareMatrix _ a _ _ -> matrixNTopRightMost $ MatrixN n a
  Empty -> Nothing
matrixNTopRightMost (MatrixN SZero m) = case m of
  UnitMatrix a -> Just a
  Empty -> Nothing

liftMatF :: (forall n. Matrix n a -> b) -> MatrixN a -> b
liftMatF f (MatrixN _ m) = f m

instance (Show a) => Show (MatrixN a) where
  show (MatrixN n matrix) = "Matrix of size n=" ++ show n ++ "\n" ++ indentOnParens (show matrix)
    where
      indentOnParens str = go str 0 ""

      go :: String -> Int -> String -> String
      go [] _ indented = reverse indented
      go (x : xs) openCount indented
        | x == '(' = go xs (openCount + 1) (x : indented)
        | x == ')' = go xs (max 0 (openCount - 1)) (x : indented)
        | x == '\n' = go xs openCount (replicate openCount ' ' ++ x : indented)
        | otherwise = go xs openCount (x : indented)

instance {-# OVERLAPPABLE #-} (Show a) => Show (Matrix n a) where
  show (SquareMatrix a b c d) = "(SquareMatrix\n" ++ show a ++ show b ++ show c ++ (reverse . drop 1 . reverse . show) d ++ ")\n"
  show (UpperRightTriangularMatrix a b c) = "(UpperRightTriangularMatrix\n" ++ show a ++ show b ++ "(Empty)\n" ++ (reverse . drop 1 . reverse . show) c ++ ")\n"
  show (UnitMatrix a) = "(UnitMatrix " ++ show a ++ ")\n"
  show Empty = "(Empty)\n"

matI :: Int -> MatrixN a -> Maybe (MatrixN a)
matI i (MatrixN n m) = case n of
  SZero -> Nothing
  SSucc n2 ->
    Just $
      MatrixN
        n2
        ( ( case m of
              UpperRightTriangularMatrix a b d -> [a, b, Empty, d]
              SquareMatrix a b c d -> [a, b, c, d]
              Empty -> replicate 4 Empty
          )
            !! i
        )
