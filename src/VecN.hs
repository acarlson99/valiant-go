{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module VecN where

import Nat
import Vec

data VecN a where
  VecN :: (SNatl n) => SNat n -> Vec n a -> VecN a

instance (Show n) => Show (VecN n) where
  show (VecN sn v) = show v ++ " l=" ++ show sn

vecNAppend :: a -> VecN a -> VecN a
vecNAppend a (VecN n v) = case n of
  SZero -> VecN (SSucc n) $ VCons a VNil
  SSucc _ ->
    case v of
      (VCons b bs) -> case vecNAppend a (VecN snat bs) of
        (VecN n2 xs) -> VecN (SSucc n2) $ VCons b xs

listToVecN :: [a] -> VecN a
listToVecN [] = VecN SZero VNil
listToVecN (x : xs) = case listToVecN xs of VecN sn vs -> VecN (SSucc sn) (VCons x vs)

vecNCons :: a -> VecN a -> VecN a
vecNCons x (VecN _ xs) = VecN snat $ VCons x xs

vecSplitFirst :: a -> Vec n a -> (a, Vec (n - One) a)
vecSplitFirst e VNil = (e, VNil)
vecSplitFirst _ (VCons x xs) = (x, xs)

vecNSplitFirst :: a -> VecN a -> (a, VecN a)
vecNSplitFirst _ (VecN _ (VCons x xs)) = (x, VecN snat xs)
vecNSplitFirst e vs = (e, vs)

vecNSplitAt :: SNat n -> a -> VecN a -> (VecN a, VecN a)
vecNSplitAt SZero _ v = (VecN snat VNil, v)
vecNSplitAt (SSucc n) df (VecN _ (VCons x xs)) =
  let (a, rest) = vecNSplitAt n df $ VecN snat xs
   in (vecNCons x a, rest)
vecNSplitAt (SSucc n) df (VecN SZero VNil) =
  let (a, rest) = vecNSplitAt n df $ VecN snat VNil
   in (vecNCons df a, rest)
