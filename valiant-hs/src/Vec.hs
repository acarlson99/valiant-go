{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Vec where

import Nat

-- TODO: add SNatl constraint??
data Vec :: Nat -> * -> * where
  VNil :: Vec 'Zero a
  VCons :: SNatl n => a -> Vec n a -> Vec ('Succ n) a

instance Functor (Vec n) where
  fmap f VNil = VNil
  fmap f (VCons x xs) = VCons (f x) $ fmap f xs

instance Applicative (Vec 'Zero) where
  pure a = VNil
  (<*>) _ _ = VNil

instance (SNatl n, Applicative (Vec n)) => Applicative (Vec ('Succ n)) where
  pure a = VCons a $ pure a
  (<*>) (VCons f fs) (VCons v vs) = VCons (f v) $ fs <*> vs

instance Show a => (Show (Vec n a)) where
  show (VCons x xs) = "(cons " ++ show x ++ " " ++ show xs ++ ")"
  show VNil = "'()"

class SNatl n => ListToVec n where
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
  VecN :: SNatl n => SNat n -> Vec n a -> VecN a

-- instance Functor VecN where
--   fmap f (VecN n v) = VecN n $ fmap f v

-- instance Applicative VecN where
--   -- pure a = VecN snat $ pure a
--   pure a = undefined
--   (<*>) (VecN nf fs) (VecN nx xs) = VecN snat $ fs <*> xs

listToVecN :: [a] -> VecN a
listToVecN [] = VecN SZero VNil
listToVecN (x : xs) = case listToVecN xs of VecN sn vs -> VecN (SSucc sn) (VCons x vs)

instance Show n => Show (VecN n) where
  show (VecN sn v) = show v ++ " l=" ++ show sn

-- -- TODO: use VecN type
-- -- TODO: use vecsplitat to construct 4x4 mat
-- class VecSplitAt (n :: Nat) where
--   vecSplitAt :: (SNatl n, SNatl m) => SNat n -> a -> Vec (m :: Nat) a -> (Vec n a, Vec (m - n) a)

-- instance SNatl n => VecSplitAt n where
--   vecSplitAt SZero _ v = (VNil, v)
--   vecSplitAt (SSucc n) df (VCons v vs) = (VCons v a, rest)
--     where
--       (a, rest) = vecSplitAt n df vs
--   vecSplitAt (SSucc n) df VNil = (VCons df a, rest)
--     where
--       (a, rest) = vecSplitAt n df VNil

-- vecSplitAt :: (SNatl n, SNatl m, SNatl (m - n)) => SNat n -> a -> Vec (m :: Nat) a -> (Vec n a, Vec (m - n) a)
-- vecSplitAt SZero _ v = (VNil, v)
-- vecSplitAt (SSucc n) df VNil = (VCons df a, rest)
--   where
--     (a, rest) = vecSplitAt n df VNil
-- vecSplitAt _ _ _ = undefined
