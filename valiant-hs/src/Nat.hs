{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nat where

import Data.Type.Equality

-- Ideas from https://stackoverflow.com/questions/20558648/what-is-the-datakinds-extension-of-haskell
--            https://stackoverflow.com/questions/43156781/haskell-gadts-making-a-type-safe-tensor-types-for-riemannian-geometry
--            https://alexpeits.github.io/posts/2018-09-27-haskell-proofs.html#intro
--            https://richarde.dev/papers/2018/stitch/stitch.pdf
data Nat = Zero | Succ Nat deriving (Eq)

proof :: Succ Zero + Succ Zero :~: Succ (Succ Zero + Zero)
proof = Refl

plusLeftId :: SNat a -> (Zero + a) :~: a
plusLeftId SZero = Refl
plusLeftId (SSucc n) = gcastWith (plusLeftId n) Refl

plusRightId :: SNat a -> (a + Zero) :~: a
plusRightId SZero = Refl
plusRightId (SSucc n) = gcastWith (plusRightId n) Refl

given1 :: SNat a -> (a + Zero) :~: a
given1 _ = Refl

given2 :: SNat a -> SNat b -> (a + Succ b) :~: Succ (a + b)
given2 _ _ = Refl

-- given2Refl :: SNat a -> SNat b -> Succ (a + b) :~: (Succ a + b)
-- given2Refl a SZero = Refl
-- given2Refl a (Succ b) =
--   let step1 :: SNat a -> SNat b -> Succ (a + b) :~: (Succ a + b)
--       step1 a b = given2Refl a b
--    in gcastWith (given2Refl a b) Refl

(!+) :: SNat n -> SNat m -> SNat (n + m)
n !+ SZero = n
n !+ (SSucc m) = SSucc (n !+ m)

(==>) :: a :~: b -> b :~: c -> a :~: c
Refl ==> Refl = Refl

plusAssoc :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
plusAssoc a b SZero = Refl
plusAssoc a b (SSucc c) = gcastWith (plusAssoc a b c) Refl

-- plusComm :: SNat a -> SNat b -> (a + b) :~: (b + a)
-- plusComm a SZero =
--   let step1 :: SNat a -> (a + Zero) :~: (Zero + a)
--       step1 a = gcastWith (plusLeftId a) Refl
--    in gcastWith (step1 a) Refl
-- plusComm a (SSucc b) =
--   let step1 :: SNat a -> SNat b -> Succ (a + b) :~: (a + Succ b)
--       step1 a b = gcastWith (plusAssoc a b (SSucc SZero)) Refl
--       step2 :: SNat a -> SNat b -> 'Succ (a + b) :~: 'Succ (b + a)
--       step2 a b = gcastWith (step1 a b) Refl -- this is no good
--    in gcastWith (step1 a b) Refl

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
  ('Succ n) - (Succ m) = (n - m)
  'Zero - m = Zero -- no negatives

type family Min a b where
  Min m 'Zero = 'Zero
  Min ('Succ n) ('Succ m) = 'Succ (Min n m)

instance TestEquality SNat where
  testEquality (SSucc a) (SSucc b) = case a `testEquality` b of
    Just Refl -> Just Refl
    Nothing -> Nothing
  testEquality SZero SZero = Just Refl
  testEquality _ _ = Nothing

-- Refl <- snat @(Half (Half (ExpFour n))) :~: snat @n

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

-- helper :: SNat a :~: SNat a
-- helper = Refl

-- proof :: Half (ExpFour n) :~: ExpTwo n
-- proof = gcastWith Refl Refl

-- type family Sqrt (n :: Nat) :: Nat where
--   ExpTwo ('Succ 'Zero) = 'Zero
--   ExpTwo ('Succ n) = Mul Two (ExpTwo n) -- TODO: un-this

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

data Fin :: Nat -> * where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)

data SNat :: Nat -> * where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

instance Show (SNat n) where
  show nat = show $ f nat
    where
      f :: SNat n -> Int
      f (SSucc n) = 1 + f n
      f SZero = 0

class SNatl (n :: Nat) where
  snat :: SNat n

instance SNatl 'Zero where
  snat = SZero

instance SNatl n => SNatl ('Succ n) where
  snat = SSucc snat

data SNatWrap where
  SNatWrap :: SNatl a => SNat a -> SNatWrap

snatHalf :: SNat a -> SNat (Half a)
snatHalf (SSucc (SSucc n)) = SSucc (snatHalf n)
snatHalf (SSucc SZero) = SZero
snatHalf SZero = SZero

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
    -- (a =?= b) >>>=== (\c -> return Refl) -- this does not work
    case a =?= b of
      Nothing -> Nothing
      Just Refl -> Just Refl
  _ =?= _ = Nothing
