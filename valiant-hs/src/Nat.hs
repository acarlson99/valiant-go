{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nat where

-- Ideas from https://stackoverflow.com/questions/20558648/what-is-the-datakinds-extension-of-haskell
--            https://stackoverflow.com/questions/43156781/haskell-gadts-making-a-type-safe-tensor-types-for-riemannian-geometry
--            https://alexpeits.github.io/posts/2018-09-27-haskell-proofs.html#intro
--            https://richarde.dev/papers/2018/stitch/stitch.pdf
data Nat = Zero | Succ Nat deriving (Eq)

data VNat :: Nat -> * where
  Zv :: VNat 'Zero
  Sv :: VNat n -> VNat ('Succ n)

class Natural n where
  natural :: VNat n

instance Natural 'Zero where
  natural = Zv

instance Natural n => Natural ('Succ n) where
  natural = Sv natural

instance Show Nat where
  show nat = show $ f nat
    where
      f :: Nat -> Int
      f (Succ n) = 1 + f n
      f Zero = 0

type family a + b where
  'Zero + m = m
  ('Succ n) + m = 'Succ (n + m)

type family Min a b where
  Min 'Zero m = 'Zero
  Min m 'Zero = 'Zero
  Min ('Succ n) ('Succ m) = 'Succ (Min n m)

type family Max a b where
  Max 'Zero m = m
  Max m 'Zero = m
  Max ('Succ n) ('Succ m) = 'Succ (Max n m)

type family IsTwice a where
  IsTwice 'Zero = 'Zero
  IsTwice ('Succ n) = 'Succ ('Succ (IsTwice n))

type family ExpTwo (n :: Nat) :: Nat where
  ExpTwo 'Zero = 'Succ 'Zero
  ExpTwo ('Succ n) = Mul Two (ExpTwo n)

-- type family Sqrt (n :: Nat) :: Nat where
--   ExpTwo ('Succ 'Zero) = 'Zero
--   ExpTwo ('Succ n) = Mul Two (ExpTwo n) -- TODO: un-this

type family Mul (m :: Nat) (n :: Nat) :: Nat where
  Mul m 'Zero = 'Zero
  Mul m ('Succ n) = Add m (Mul m n)

type family Add (m :: Nat) (n :: Nat) :: Nat where
  Add m n = m + n

type family Half (n :: Nat) :: Nat where
  Half 'Zero = 'Zero
  Half ('Succ 'Zero) = 'Succ 'Zero
  Half ('Succ ('Succ n)) = 'Succ (Half n)

data Fin :: Nat -> * where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)

data SNat :: Nat -> * where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

class SNatl (n :: Nat) where
  snat :: SNat n

instance SNatl 'Zero where
  snat = SZero

instance SNatl n => SNatl ('Succ n) where
  snat = SSucc snat

type One = 'Succ 'Zero

type Two = One + One

type Three = One + Two

class NatTypeToVal a where
  natTypeToVal :: a -> Nat
