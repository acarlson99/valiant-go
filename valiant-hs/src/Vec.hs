{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Vec where

import Nat

data Vec :: Nat -> * -> * where
  VNil :: Vec 'Zero a
  VCons :: a -> Vec n a -> Vec ('Succ n) a

instance Show a => (Show (Vec n a)) where
  show (VCons x xs) = "(cons " ++ show x ++ " " ++ show xs ++ ")"
  show VNil = "'()"

class ListToVec n where
  listToVec :: [a] -> Maybe (Vec n a)

instance ListToVec 'Zero where
  listToVec (_ : _) = Nothing
  listToVec _ = Just VNil

instance ListToVec n => ListToVec ('Succ n) where
  listToVec (x : xs) = VCons x <$> listToVec xs
  listToVec _ = Nothing

instance Foldable (Vec n) where
  foldr f acc (VCons a bs) = f a $ foldr f acc bs
  foldr _ acc VNil = acc

instance NatTypeToVal (Vec 'Zero a) where
  natTypeToVal = const Zero

instance NatTypeToVal (Vec n a) => NatTypeToVal (Vec ('Succ n) a) where
  natTypeToVal :: Vec ('Succ n) a -> Nat
  natTypeToVal (VCons _ xs) = Succ $ natTypeToVal xs

data VecN a where
  VecN :: SNat n -> Vec n a -> VecN a

listToVecN :: [a] -> VecN a
listToVecN [] = VecN SZero VNil
listToVecN (x : xs) = case listToVecN xs of VecN sn vs -> VecN (SSucc sn) (VCons x vs)

instance Show n => Show (VecN n) where
  show (VecN sn v) = show v ++ " l=" ++ show sn

-- TODO: use VecN type
-- TODO: use vecsplitat to construct 4x4 mat
class VecSplitAt (n :: Nat) where
  vecSplitAt :: SNat n -> a -> Vec (m :: Nat) a -> (Vec n a, Vec (m - n) a)

instance VecSplitAt n where
  vecSplitAt :: SNat n -> a -> Vec m a -> (Vec n a, Vec (m - n) a)
  vecSplitAt SZero _ v = (VNil, v)
  vecSplitAt (SSucc n) df (VCons v vs) = (VCons v a, rest)
    where
      (a, rest) = vecSplitAt n df vs
  vecSplitAt (SSucc n) df VNil = (VCons df a, rest)
    where
      (a, rest) = vecSplitAt n df VNil
