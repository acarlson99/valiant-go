{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MatrixN where

import Data.Data
import Nat
import SparseMatrix
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
  show (SquareMatrix a b c d) = "(SquareMatrix\n" ++ show a ++ "\n" ++ show b ++ "\n" ++ show c ++ "\n" ++ show d ++ ")"
  show (UpperRightTriangularMatrix a b c) = "(UpperRightTriangularMatrix\n" ++ show a ++ "\n" ++ show b ++ "\n" ++ "(Empty)" ++ show c ++ ")"
  show (UnitMatrix a) = "(UnitMatrix " ++ show a ++ ")"
  show Empty = "(Empty)"
