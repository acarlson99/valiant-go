{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SparseMatrix where

import Ring

-------------------------------- Data ------------------------------------------

-- Ideas from https://stackoverflow.com/questions/20558648/what-is-the-datakinds-extension-of-haskell
--            https://stackoverflow.com/questions/43156781/haskell-gadts-making-a-type-safe-tensor-types-for-riemannian-geometry
data N = Z | S N deriving (Eq, Show)

{- ORMOLU_DISABLE -}

type SqShape s   = ((s,s),
                    (s,s))
type UTrShape s u = (s,u
                      ,s)

{- ORMOLU_ENABLE -}

-- | Matrix n a
data Matrix (n :: N) a where
  SquareMatrix :: Matrix n a -> Matrix n a -> Matrix n a -> Matrix n a -> NonZeroMatrix n a
  UpperRightTriangularMatrix :: Matrix n a -> Matrix n a -> Matrix n a -> NonZeroMatrix n a
  UnitMatrix :: a -> ZeroMatrix a
  Empty :: Matrix n a

type ZeroMatrix = Matrix 'Z

type NonZeroMatrix (n :: N) = Matrix ('S n)

class IsZeroType n where
  isZeroType :: Matrix n a -> Bool

instance IsZeroType 'Z where
  isZeroType _ = True

instance IsZeroType n => IsZeroType ('S n) where
  isZeroType _ = False

-- Test stuff

type One = 'S 'Z

type Two = 'S One

type Three = 'S Two

{- ORMOLU_DISABLE -}
sq1, ut1 :: Num a => a -> Matrix One a
sq1 n = a
  where
    a = SquareMatrix  b c
                      d e
    b = (+ 1) <$> UnitMatrix n
    c =           UnitMatrix n
    d =           UnitMatrix n
    e =           UnitMatrix n
ut1 n = UpperRightTriangularMatrix u u u where u = UnitMatrix n
{- ORMOLU_ENABLE -}

sq2, ut2 :: Num a => a -> Matrix Two a
sq2 n = SquareMatrix a (subtract 1 <$> a) a (subtract 1 <$> a) where a = sq1 n
ut2 n = UpperRightTriangularMatrix t a t where t = ut1 n; a = sq1 n

sq3, ut3 :: Num a => a -> Matrix Three a
sq3 n = SquareMatrix s Empty s s where s = sq2 n
ut3 n = UpperRightTriangularMatrix t2 s t2 where t2 = ut2 n; s = sq2 n

------------------------ Functors and things -----------------------------------

instance Foldable (Matrix n) where
  foldr :: (a -> b -> b) -> b -> Matrix n a -> b
  foldr f acc (SquareMatrix a b c d) = foldr (flip $ foldr f) acc [a, b, c, d]
  foldr f acc (UpperRightTriangularMatrix a b d) = foldr (flip $ foldr f) acc [a, b, d]
  foldr f acc (UnitMatrix a) = f a acc
  foldr _ acc Empty = acc

instance Functor (Matrix n) where
  fmap f (SquareMatrix a b c d) = SquareMatrix (fmap f a) (fmap f b) (fmap f c) (fmap f d)
  fmap f (UpperRightTriangularMatrix a b d) = UpperRightTriangularMatrix (fmap f a) (fmap f b) (fmap f d)
  fmap f (UnitMatrix a) = UnitMatrix (f a)
  fmap _ Empty = Empty

instance Ring a => Ring (ZeroMatrix a) where
  zero = Empty
  add = addZM
  mul = mulZM

addZM :: Ring a => ZeroMatrix a -> ZeroMatrix a -> ZeroMatrix a
addZM Empty y = y
addZM x Empty = x
addZM (UnitMatrix a) (UnitMatrix b) = UnitMatrix (add a b)

mulZM :: Ring a => ZeroMatrix a -> ZeroMatrix a -> ZeroMatrix a
mulZM Empty _y = Empty
mulZM _x Empty = Empty
mulZM (UnitMatrix a) (UnitMatrix b) = UnitMatrix (mul a b)

instance (Ring a, Ring (Matrix n a), Applicative (Matrix n), Applicative (NonZeroMatrix n)) => Ring (NonZeroMatrix n a) where
  zero = Empty
  add = addSM
  mul = mulSM

addSM :: (Ring a, Applicative (NonZeroMatrix n)) => NonZeroMatrix n a -> NonZeroMatrix n a -> NonZeroMatrix n a
addSM Empty y = y
addSM x Empty = x
addSM x y = add <$> x <*> y

-- see Bernardy and Claessen, “Efficient Divide-and-Conquer Parsing of Practical Context-Free Languages.”
mulSM :: (Ring (Matrix n a), Applicative (Matrix n)) => NonZeroMatrix n a -> NonZeroMatrix n a -> NonZeroMatrix n a
mulSM Empty _y = Empty
mulSM _x Empty = Empty
{- ORMOLU_DISABLE -}
mulSM (SquareMatrix a11 a12
                    a21 a22)
      (SquareMatrix b11 b12
                    b21 b22) =
      SquareMatrix c11 c12
                    c21 c22
  where
    (+) = add; (*) = mul
    c11 = (a11*b11) + (a12*b21);   c12 = (a11*b12) + (a12*b22)
    c21 = (a21*b11) + (a22*b21);   c22 = (a21*b21) + (a22*b22)
{- ORMOLU_ENABLE -}
mulSM _ _ = error "Illegal type combination for mulSM"

instance Applicative ZeroMatrix where
  pure = UnitMatrix
  (UnitMatrix a) <*> (UnitMatrix b) = UnitMatrix $ a b
  Empty <*> _ = Empty
  _ <*> Empty = Empty

instance Applicative (Matrix n) => Applicative (NonZeroMatrix n) where
  pure m = SquareMatrix (pure m) (pure m) (pure m) (pure m)
  (SquareMatrix a b c d) <*> (SquareMatrix e f g h) = SquareMatrix (a <*> e) (b <*> f) (c <*> g) (d <*> h)
  (UpperRightTriangularMatrix a b d) <*> (UpperRightTriangularMatrix e f h) = UpperRightTriangularMatrix (a <*> e) (b <*> f) (d <*> h)
  (UpperRightTriangularMatrix a b d) <*> (SquareMatrix e f _ h) = UpperRightTriangularMatrix (a <*> e) (b <*> f) (d <*> h)
  (SquareMatrix a b _ d) <*> (UpperRightTriangularMatrix e f h) = UpperRightTriangularMatrix (a <*> e) (b <*> f) (d <*> h)
  Empty <*> _ = error "Warning: should fill in zeroes on the left" -- but we don't have a "zero" around
  _ <*> Empty = error "Warning: should fill in zeroes on the right" -- but we don't have a "zero" around

instance (Applicative (Matrix n), Semigroup a) => Semigroup (Matrix n a) where
  (<>) = (<*>) . ((<>) <$>)

instance (Applicative (Matrix n), Semigroup a) => Monoid (Matrix n a) where
  mempty = Empty

---------------------------------- Show ----------------------------------------

-- Richard Bird repmin
class (Show a) => BirdWalk a where
  walk :: Int -> a -> (Int, String)

instance (Show m) => Show (ZeroMatrix m) where
  show m = s
    where
      (topMax, s) = walk topMax m

instance (Show m) => BirdWalk (ZeroMatrix m) where
  walk topMax Empty = (0, concat . replicate len $ replicate ((topMax + 1) * len) ' ' ++ "\n")
    where
      len = 1
  walk topMax (UnitMatrix m) = (length s, fixLength topMax s)
    where
      s = show m
      fixLength n x
        | len == 0 = replicate n '+'
        | otherwise = replicate md ' ' ++ x ++ replicate dv ' '
        where
          len = length x
          (dv, md) = n `divMod` len

instance
  (Show m, Size n, BirdWalk (Matrix n m)) =>
  BirdWalk (NonZeroMatrix n m)
  where
  walk topMax mat = case mat of
    (SquareMatrix a b c d) -> (foldr max 0 ns, concatQuads sa sb sc sd)
      where
        (ns, [sa, sb, sc, sd]) = unzip $ map (walk topMax) [a, b, c, d]
    (UpperRightTriangularMatrix a b d) -> (foldr max 0 ns, concatQuads sa sb sc sd)
      where
        (ns, [sa, sb, sc, sd]) = unzip $ map (walk topMax) [a, b, Empty, d]
    Empty -> (0, concat . replicate (size mat) $ replicate ((topMax + 1) * size mat) ' ' ++ "\n")

concatQuads :: String -> String -> String -> String -> String
concatQuads a b c d = concatMap pairConcat [(a, b), (c, d)]
  where
    lhf = lines
    rhf = map (++ "\n") . lines
    pairConcat (x, y) = concat $ zipWith (++) (lhf x) (rhf y)

instance
  (Show m, BirdWalk (Matrix n m), Size n) =>
  Show (NonZeroMatrix n m)
  where
  show mat = s
    where
      (topMax, s) = walk topMax mat

class Size n where
  size :: Matrix n a -> Int

instance Size 'Z where
  size _ = 1

instance (Size n) => Size ('S n) where
  size m = 2 * size (helper m)
    where
      helper :: NonZeroMatrix n m -> Matrix n m
      helper _ = Empty

---------------------------- Constructors --------------------------------------

nextClosestSquare :: (Ord a, Num a) => a -> a
nextClosestSquare n =
  head $ dropWhile (< n) [2 ^ x | x <- ([0 ..] :: [Int])]

lastClosestSquare :: (Ord a, Num a) => a -> a
lastClosestSquare n =
  last . (0 :) $ takeWhile (< n) [2 ^ x | x <- ([0 ..] :: [Int])]

-- newUpperRightTriangularMatrix :: Show m => Int -> Matrix n m
-- newUpperRightTriangularMatrix n
--   | nsq < 2 = Empty 0
--   | nsq == 2 = UpperRightTriangularMatrix (Empty smsq) (Empty smsq) (Empty smsq)
--   | otherwise = UpperRightTriangularMatrix smallT smallSQ smallT
--   where
--     smallT = newUpperRightTriangularMatrix (float2Int (int2Float n / 2.0))
--     smallSQ = newSquareMatrix (float2Int (int2Float n / 2.0))
--     smsq = float2Int (int2Float nsq / 2.0)
--     nsq = nextClosestSquare n

u2s :: Matrix n m -> Matrix n m
u2s (UpperRightTriangularMatrix a b d) = SquareMatrix a b Empty d
u2s _ = undefined

-- u2s (UpperRightTriangularMatrix a b d) = SquareMatrix a b (Empty $ size a) d

-- newSquareMatrix :: Int -> Matrix n m
-- newSquareMatrix n
--   | nsq < 2 = Empty 0
--   | nsq == 2 = SquareMatrix (Empty smsq) (Empty smsq) (Empty smsq) (Empty smsq)
--   | otherwise = newSquareMatrix_ smallM
--   where
--     nsq = nextClosestSquare n
--     smsq = float2Int (int2Float nsq / 2.0)
--     smallM = newSquareMatrix smsq

newSquareMatrix_ :: Matrix n m -> NonZeroMatrix n m
newSquareMatrix_ m = SquareMatrix m m m m

------------------------------- Algorithm --------------------------------------

class ConstructMatrix n where
  constructMatrix :: String -> Matrix n String

instance ConstructMatrix 'Z where
  constructMatrix s = UnitMatrix s

instance (Size n, ConstructMatrix n) => ConstructMatrix ('S n) where
  constructMatrix s = UpperRightTriangularMatrix a x b
    where
      n = size b
      (as, bs) = splitAt n s
      a = constructMatrix as
      x = Empty
      b = constructMatrix bs

-- TODO: This type signature changes the value of `sq`
--       I think I need to lift `strlen` to the type level and somehow
--       at compile time infer
--       `n = (/2) Len s => typeOf (constructMatrix s) == (n*'S `Cons` 'Z)`
sqa :: Matrix ('S 'Z) String
sqb :: Matrix ('S ('S ('S 'Z))) String
sqc :: Matrix ('S ('S ('S ('S 'Z)))) String
(sqa, sqb, sqc) = (sqa_, sqb_, sqc_)
  where
    sqa_ = constructMatrix "abcdef"
    sqb_ = constructMatrix "abcdef"
    sqc_ = constructMatrix "abcdef"

-- see Bernardy and Claessen, “Efficient Divide-and-Conquer Parsing of Practical Context-Free Languages.”
class Valiant a where
  v :: a -> a -> a -> a

instance Ring a => Valiant (ZeroMatrix a) where
  v Empty (UnitMatrix x) Empty = UnitMatrix x
  v _ x _ = x

instance (Ring (Matrix n a), Valiant (Matrix n a)) => Valiant (NonZeroMatrix n a) where
  v :: (Ring (Matrix n a), Valiant (Matrix n a)) => NonZeroMatrix n a -> NonZeroMatrix n a -> NonZeroMatrix n a -> NonZeroMatrix n a
  v _ Empty _ = Empty
  v (UpperRightTriangularMatrix a11 a12 a22) (SquareMatrix x11 x12 x21 x22) (UpperRightTriangularMatrix b11 b12 b22) = SquareMatrix y11 y12 y21 y22
    where
      y21 = v a22 x21 b11 :: Matrix n a
      y11 = v a11 ((x11 `add` a12) `mul` y21) b11
      y22 = v a22 ((x22 `add` y21) `mul` b12) b22
      y12 = v a11 ((x12 `add` a12) `mul` (y22 `add` y11) `mul` b12) b22
  v _ _ _ = undefined
