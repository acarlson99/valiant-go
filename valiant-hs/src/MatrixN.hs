{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module MatrixN where

import Data.Data
import qualified Data.Set as S
import Nat
import Ring
import RingParse
import SparseMatrix
import Vec
import VecN

-- MatrixN data type is meant to provide a simpler interface for `Matrix n a`
data MatrixN a where
  MatrixN :: SNat n -> Matrix n a -> MatrixN a

instance Functor MatrixN where
  fmap f (MatrixN n m) = MatrixN n $ fmap f m

-- instance Show a => Show (MatrixN a) where
--   show mn = case mn of (MatrixN n m) -> show m

-- TODO: this should split a matrix like so:
-- because `n` is always odd
-- n = (length(vec)-1) / 2
-- (as,rest) = splitN n vec
-- (x,bs) = splitN One rest
-- if bs =?= as then UpperRightTriangular (recurse as) (squareMatWithElemInBottomLeftCorner x) (recurse bs)
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
