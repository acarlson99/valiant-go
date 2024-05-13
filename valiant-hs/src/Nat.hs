{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nat where

import Data.Kind (Type)
import Data.Type.Equality

-- Ideas from https://stackoverflow.com/questions/20558648/what-is-the-datakinds-extension-of-haskell
--            https://stackoverflow.com/questions/43156781/haskell-gadts-making-a-type-safe-tensor-types-for-riemannian-geometry
--            https://alexpeits.github.io/posts/2018-09-27-haskell-proofs.html#intro
--            https://richarde.dev/papers/2018/stitch/stitch.pdf
data Nat = Zero | Succ Nat deriving (Eq)

instance Show Nat where
  show nat = show $ f nat
    where
      f :: Nat -> Int
      f (Succ n) = 1 + f n
      f Zero = 0

type family a + b where
  m + 'Zero = m
  n + ('Succ m) = 'Succ (n + m)

type family (a :: Nat) - (b :: Nat) where
  m - 'Zero = m
  ('Succ n) - ('Succ m) = (n - m)
  'Zero - m = 'Zero -- no negatives

type family Min a b where
  Min m 'Zero = 'Zero
  Min ('Succ n) ('Succ m) = 'Succ (Min n m)

instance TestEquality SNat where
  testEquality (SSucc a) (SSucc b) = case a `testEquality` b of
    Just Refl -> Just Refl
    Nothing -> Nothing
  testEquality SZero SZero = Just Refl
  testEquality _ _ = Nothing

type family Max a b where
  Max 'Zero m = m
  Max m 'Zero = m
  Max ('Succ n) ('Succ m) = 'Succ (Max n m)

type family Twice a where
  Twice 'Zero = 'Zero
  Twice ('Succ n) = 'Succ ('Succ (Twice n))

type family ExpTwo (n :: Nat) :: Nat where
  ExpTwo 'Zero = 'Succ 'Zero
  ExpTwo ('Succ n) = Mul Two (ExpTwo n)

type family ExpFour (n :: Nat) :: Nat where
  ExpFour 'Zero = 'Succ 'Zero
  ExpFour ('Succ n) = Mul (Add Two Two) (ExpFour n)

type family Mul (m :: Nat) (n :: Nat) :: Nat where
  Mul m 'Zero = 'Zero
  Mul m ('Succ n) = Add m (Mul m n)

type family Add (m :: Nat) (n :: Nat) :: Nat where
  Add m n = m + n

type family Sub m n where
  Sub m n = m - n

type family Half (n :: Nat) :: Nat where
  Half 'Zero = 'Zero
  Half ('Succ 'Zero) = 'Zero
  Half ('Succ ('Succ n)) = 'Succ (Half n)

data Fin :: Nat -> Type where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)

data SNat :: Nat -> Type where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

instance Show (SNat n) where
  show nat = show $ f nat
    where
      f :: forall n. SNat n -> Int
      f (SSucc n) = 1 + f n
      f SZero = 0

class SNatl (n :: Nat) where
  snat :: SNat n

instance SNatl 'Zero where
  snat = SZero

instance (SNatl n) => SNatl ('Succ n) where
  snat = SSucc snat

data SNatWrap where
  SNatWrap :: (SNatl a) => SNat a -> SNatWrap

snatHalf :: SNat a -> SNat (Half a)
snatHalf (SSucc (SSucc n)) = SSucc (snatHalf n)
snatHalf (SSucc SZero) = SZero
snatHalf SZero = SZero

snatSub :: SNat a -> SNat b -> SNat (a - b)
snatSub SZero _ = SZero
snatSub a SZero = a
snatSub (SSucc a) (SSucc b) = snatSub a b

type One = 'Succ 'Zero

type Two = One + One

type Three = Two + One

type Four = Three + One

type Five = Four + One

type Six = Five + One

type Seven = Six + One

type Eight = Seven + One

type Nine = Eight + One

type Ten = Nine + One

class NatTypeToVal a where
  natTypeToVal :: a -> Nat

class SNatEq s t where
  (=?=) :: s -> t -> Maybe (s :~: t)

instance SNatEq (SNat s) (SNat t) where
  SZero =?= SZero = Just Refl
  SSucc a =?= SSucc b =
    case a =?= b of
      Nothing -> Nothing
      Just Refl -> Just Refl
  _ =?= _ = Nothing
