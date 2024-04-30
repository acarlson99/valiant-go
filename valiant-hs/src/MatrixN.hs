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

topRightMost :: MatrixN a -> Maybe a
topRightMost (MatrixN (SSucc n) m) = case m of
  UpperRightTriangularMatrix _ a _ -> topRightMost $ MatrixN n a
  SquareMatrix _ a _ _ -> topRightMost $ MatrixN n a
  Empty -> Nothing
topRightMost (MatrixN SZero m) = case m of
  UnitMatrix a -> Just a
  Empty -> Nothing

liftV :: Ring a => MatrixN a -> MatrixN a
liftV (MatrixN n m) = MatrixN n $ runV m

topRightMost' :: Matrix n a -> Maybe a
topRightMost' m = case m of
  UpperRightTriangularMatrix _ a _ -> topRightMost' a
  SquareMatrix _ a _ _ -> topRightMost' a
  UnitMatrix a -> Just a
  Empty -> Nothing

liftMatF :: (forall n. Matrix n a -> b) -> MatrixN a -> b
liftMatF f (MatrixN _ m) = f m

-- (liftMatF topRightMost') . liftV $ vecNToValiantMatrixN $ listToVecN opRing

runThingy :: (Ord a, Ring (b -> RingParse a)) => b -> [RingParse a] -> Maybe (RingParse a)
runThingy productionRules syms =
  let ls = listToVecN $ map const syms
      m' = vecNToValiantMatrixN (const (RingParse (S.fromList []))) ls
   in case m' of MatrixN _ m -> topRightMost' (runV m) <*> pure productionRules
