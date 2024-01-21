{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

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
  natTypeToVal (VCons x xs) = Succ $ natTypeToVal xs