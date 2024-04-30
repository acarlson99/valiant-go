{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Vec where

import Data.Data
import Nat

-- TODO: add SNatl constraint??
data Vec :: Nat -> * -> * where
  VNil :: Vec 'Zero a
  VCons :: SNatl n => a -> Vec n a -> Vec ('Succ n) a

instance Functor (Vec n) where
  fmap _ VNil = VNil
  fmap f (VCons x xs) = VCons (f x) $ fmap f xs

instance Applicative (Vec 'Zero) where
  pure _ = VNil
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

class VecAppend n where
  vecAppend :: a -> Vec n a -> Vec ('Succ n) a

instance VecAppend n where
  vecAppend a VNil = VCons a VNil
  vecAppend a (VCons b bs) = case vecAppend a bs of (VCons c cs) -> VCons b (VCons c cs)

instance NatTypeToVal (Vec 'Zero a) where
  natTypeToVal = const Zero

instance NatTypeToVal (Vec n a) => NatTypeToVal (Vec ('Succ n) a) where
  natTypeToVal :: Vec ('Succ n) a -> Nat
  natTypeToVal (VCons _ xs) = Succ $ natTypeToVal xs

class VecSplitAt (n :: Nat) where
  vecSplitAt :: (SNatl n, SNatl m) => SNat n -> a -> Vec (m :: Nat) a -> (Vec n a, Vec (m - n) a)

instance VecSplitAt 'Zero where
  vecSplitAt SZero _ v = (VNil, v)

instance (VecSplitAt n, SNatl n) => VecSplitAt ('Succ n) where
  vecSplitAt (SSucc n) df (VCons v vs) = (VCons v a, rest)
    where
      (a, rest) = vecSplitAt n df vs
  vecSplitAt (SSucc n) df VNil = (VCons df a, rest)
    where
      (a, rest) = vecSplitAt n df VNil

instance SNatEq (Vec n a) (Vec m a) where
  VNil =?= VNil = Just Refl
  (VCons _ as) =?= (VCons _ bs) = case as =?= bs of
    Nothing -> Nothing
    Just Refl -> Just Refl
  _ =?= _ = Nothing

-- -- TODO: this-- prove to the compiler that n+m is good
-- vconcat :: Vec n a -> Vec m a -> Vec (m + n) a
-- vconcat VNil ys = ys
-- vconcat (VCons x xs) ys = VCons x $ vecAppend xs ys
