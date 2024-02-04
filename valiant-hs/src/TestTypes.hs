{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TestTypes where

import Data.Type.Equality
import Nat
import SparseMatrix
import Vec

data Natty :: Nat -> * where
  Zy :: Natty 'Zero
  Sy :: Natty n -> Natty ('Succ n)

class NATTY (n :: Nat) where
  natty :: Natty n

instance NATTY 'Zero where
  natty = Zy

instance NATTY n => NATTY ('Succ n) where
  natty = Sy natty

data LenList :: * -> * where
  LenList :: NATTY n => Vec n a -> LenList a

instance (Show a) => Show (LenList a) where
  show (LenList a) = show a

lenList :: [a] -> LenList a
lenList [] = LenList VNil
lenList (x : xs) = case lenList xs of LenList ys -> LenList (VCons x ys)

-- TODO: skolem??
-- lltl a = case a of LenList ys -> ys

-- listToVec :: [a] -> Vec n a
-- listToVec (x : xs) = VCons x $ listToVec xs
-- listToVec [] = VNil

-- vec :: Vec n Char
-- (Just vec) = listToVec "abcdef"

-- class ConstructVMatrix n where
--   constructVMatrix :: Vec (ExpTwo n) a -> Matrix n a

-- instance ConstructVMatrix 'Zero where
--   constructVMatrix (VCons x VNil) = pure x

-- instance (ConstructVMatrix n) => ConstructVMatrix ('Succ n) where
--   constructVMatrix _ = undefined

class ConstructVMatrix n where
  constructVMatrix :: Applicative (Matrix n) => Vec (ExpTwo n) Char -> Matrix n Char

instance ConstructVMatrix 'Zero where
  constructVMatrix (VCons s VNil) = UnitMatrix s

instance (Size n, ConstructMatrix n) => ConstructVMatrix ('Succ n) where
  constructVMatrix (VCons a _) = pure a
  constructVMatrix VNil = Empty

type family AreEqual a b where
  AreEqual a a = ()

class AreNotEqual a b

instance {-# OVERLAPPABLE #-} (a ~ b) => AreNotEqual a b

instance {-# OVERLAPPING #-} AreNotEqual a a

-- constructVMatrix l = constructMatrix $ foldr (:) [] l

vecStr :: Vec (Add Three One) Char
(Just vecStr) = listToVec "abcd"

-- data Equal a b where
--   Refl :: Equal a a

(=?=) :: forall k (a :: k). Nat -> Nat -> Maybe (a :~: a)
Zero =?= Zero = Just Refl
(Succ n) =?= (Succ m) = n =?= m
(Succ _) =?= Zero = Nothing
Zero =?= (Succ _) = Nothing

data (:?@?:) (a :: k) (b :: k) where
  X2 :: (:?@?:) a b

-- instance (:?@?:) (Succ (Succ n)) (Succ n) where
--   X2 = undefined

x2 :: Nat -> Nat -> Maybe (a :?@?: a)
x2 (Succ (Succ n)) (Succ m) = n `x2` m
x2 Zero Zero = Just X2
x2 _ _ = Nothing

data LenPair (n :: Nat) (m :: Nat) where
  WrapPair :: LenPair n m -> GradeUpPair n m
  ZeroPair :: LenPair 'Zero 'Zero

type GradeUpPair (n :: Nat) (m :: Nat) = LenPair ('Succ ('Succ n)) ('Succ m)

-- type GradeDownPair (n :: Nat) (m :: Nat) = LenPair n m
gradeDownPair :: GradeUpPair n m -> LenPair n m
gradeDownPair (WrapPair a) = a -- LenPair n m

listPairs :: LenPair n m -> Vec n a -> Vec m b -> Vec m (a, a, b)
listPairs p (VCons a (VCons x xs)) (VCons y ys) =
  case listPairs (gradeDownPair p) xs ys of
    zs -> VCons (a, x, y) zs
listPairs _ VNil VNil = VNil
listPairs _ _ _ = undefined

-- type family Length (xs :: [a]) :: Nat where
--   Length '[] = 'Zero
--   Length (x : xs) = 'Succ (Length xs)

-- data SameLength a b where
--   SameLength :: Length a :~: Length b -> SameLength a b

-- proof a b = SameLength a b
data Sphere

data Fragment

class Renderable a where
  boundingSphere :: a -> Sphere
  hit :: a -> [Fragment] -- returns the "fragments" of all hits with ray

data AnyRenderable = forall a. Renderable a => AnyRenderable a

instance Renderable AnyRenderable where
  boundingSphere (AnyRenderable a) = boundingSphere a
  hit (AnyRenderable a) = hit a

class TimesTwo a b where
  ttProof :: a -> b -> Bool

class Thingy (n :: Nat) where
  doThingy :: a -> Vec n a
