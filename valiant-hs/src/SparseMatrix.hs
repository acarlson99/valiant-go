{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SparseMatrix where

import Data.Data
import Data.Set qualified as S
import Nat
import Ring
import RingParse
import Vec

-------------------------------- Data ------------------------------------------

data Matrix (n :: Nat) a where
  SquareMatrix :: Matrix n a -> Matrix n a -> Matrix n a -> Matrix n a -> Matrix (n + One) a
  UpperRightTriangularMatrix :: Matrix n a -> Matrix n a -> Matrix n a -> Matrix (n + One) a
  UnitMatrix :: a -> Matrix 'Zero a
  Empty :: Matrix n a

instance Foldable (Matrix n) where
  foldr :: (a -> b -> b) -> b -> Matrix n a -> b
  foldr f acc (SquareMatrix a b c d) = foldr (flip $ foldr f) acc [a, b, c, d]
  foldr f acc (UpperRightTriangularMatrix a b d) = foldr (flip $ foldr f) acc [a, b, d]
  foldr f acc (UnitMatrix a) = f a acc
  foldr _ acc Empty = acc

foldrExpandEmpty :: (Monoid a) => (a -> b -> b) -> b -> Matrix n a -> b
foldrExpandEmpty f acc (SquareMatrix a b c d) = foldr (flip $ foldrExpandEmpty f) acc [a, b, c, d]
foldrExpandEmpty f acc (UpperRightTriangularMatrix a b d) = foldrExpandEmpty f acc (SquareMatrix a b Empty d)
foldrExpandEmpty f acc (UnitMatrix a) = f a acc
foldrExpandEmpty f acc Empty = f mempty acc

instance Functor (Matrix n) where
  fmap f (SquareMatrix a b c d) = SquareMatrix (fmap f a) (fmap f b) (fmap f c) (fmap f d)
  fmap f (UpperRightTriangularMatrix a b d) = UpperRightTriangularMatrix (fmap f a) (fmap f b) (fmap f d)
  fmap f (UnitMatrix a) = UnitMatrix (f a)
  fmap _ Empty = Empty

instance (Ring a) => Ring (Matrix n a) where
  zero = Empty
  add Empty y = y
  add x Empty = x
  add (UnitMatrix a) (UnitMatrix b) = UnitMatrix (add a b)
  add (SquareMatrix a b c d) (SquareMatrix e f g h) = SquareMatrix (add a e) (add b f) (add c g) (add d h)
  add (UpperRightTriangularMatrix a b d) (UpperRightTriangularMatrix e f h) = UpperRightTriangularMatrix (add a e) (add b f) (add d h)
  add (SquareMatrix a b c d) (UpperRightTriangularMatrix e f h) = add (SquareMatrix a b c d) (SquareMatrix e f Empty h)
  add (UpperRightTriangularMatrix a b d) (SquareMatrix e f g h) = add (SquareMatrix a b Empty d) (SquareMatrix e f g h)
  mul Empty _ = Empty
  mul _ Empty = Empty
  mul (UnitMatrix a) (UnitMatrix b) = UnitMatrix (mul a b)
  mul x y = mulSM x y

-- see Bernardy and Claessen, “Efficient Divide-and-Conquer Parsing of Practical Context-Free Languages.”
{- ORMOLU_DISABLE -}
mulSM :: (Ring a) => Matrix n a -> Matrix n a -> Matrix n a
mulSM Empty _y = Empty
mulSM _x Empty = Empty
mulSM (SquareMatrix a11 a12
                    a21 a22)
      (SquareMatrix b11 b12
                    b21 b22) =
      SquareMatrix c11 c12
                    c21 c22
  where
    (+) = add; (*) = mul
    c11 = (a11*b11) + (a12*b21);   c12 = (a11*b12) + (a12*b22)
    c21 = (a21*b11) + (a22*b21);   c22 = (a21*b12) + (a22*b22)
mulSM _ _ = error "Illegal type combination for mulSM"
{- ORMOLU_ENABLE -}

instance Applicative (Matrix 'Zero) where
  pure = UnitMatrix
  (UnitMatrix a) <*> (UnitMatrix b) = UnitMatrix $ a b
  Empty <*> _ = Empty
  _ <*> Empty = Empty

instance (Applicative (Matrix n)) => Applicative (Matrix ('Succ n)) where
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

instance (Eq a) => Eq (Matrix n a) where
  (SquareMatrix a b c d) == (SquareMatrix a' b' c' d') = a == a' && b == b' && c == c' && d == d'
  (UpperRightTriangularMatrix a b c) == (UpperRightTriangularMatrix a' b' c') = SquareMatrix a b Empty c == SquareMatrix a' b' Empty c'
  (UnitMatrix a) == (UnitMatrix b) = a == b
  Empty == Empty = True
  _ == _ = False

---------------------------------- Show ----------------------------------------

-- Richard Bird repmin
class (Show a, Show b) => BirdWalk a b where
  walk :: Int -> a -> b -> (Int, String)

instance (Show m, Ring m, Ring m) => Show (Matrix 'Zero m) where
  show m = s
    where
      (topMax, s) = walk topMax m (zero @m)

instance
  (Show m, BirdWalk (Matrix n m) m, Ring m) =>
  Show (Matrix ('Succ n) m)
  where
  show mat = s
    where
      (topMax, s) = walk topMax mat (zero @m)

instance (Show m, Ring m) => BirdWalk (Matrix 'Zero m) m where
  walk topMax mat zv = (length s, fixLength topMax s)
    where
      s = show $ case mat of
        UnitMatrix m -> m
        Empty -> zv
      fixLength n x
        | len == 0 = replicate n ' '
        | otherwise = replicate (n - len) ' ' ++ x
        where
          len = length x

instance
  (Show m, BirdWalk (Matrix n m) m, Ring m) =>
  BirdWalk (Matrix ('Succ n) m) m
  where
  walk topMax mat zv = case mat of
    (SquareMatrix a b c d) -> (foldr max 0 ns, concatQuads sa sb sc sd)
      where
        (ns, [sa, sb, sc, sd]) = unzip $ map (\m -> walk topMax m zv) [a, b, c, d]
    (UpperRightTriangularMatrix a b d) -> (foldr max 0 ns, concatQuads sa sb sc sd)
      where
        (ns, [sa, sb, sc, sd]) = unzip $ map (\m -> walk topMax m zv) [a, b, Empty, d]
    Empty -> walk topMax (SquareMatrix Empty Empty Empty Empty :: Matrix ('Succ n) m) zv

concatQuads :: String -> String -> String -> String -> String
concatQuads a b c d = dropLast $ concatMap pairConcat [(a, b), (c, d)]
  where
    lhf = lines
    rhf = map (++ "\n") . lines
    pairConcat (x, y) = concat $ zipWith (++) (lhf x) (rhf y)
    dropLast = reverse . drop 1 . reverse

------------------------------------ Util -------------------------------------

sqMatWithValInBottomLeft :: a -> SNat n -> Matrix n a
sqMatWithValInBottomLeft a SZero = UnitMatrix a
sqMatWithValInBottomLeft a (SSucc n) =
  let m = sqMatWithValInBottomLeft a n
   in SquareMatrix Empty Empty m Empty

matrixTopRightMost :: Matrix n a -> Maybe a
matrixTopRightMost m = case m of
  UpperRightTriangularMatrix _ a _ -> matrixTopRightMost a
  SquareMatrix _ a _ _ -> matrixTopRightMost a
  UnitMatrix a -> Just a
  Empty -> Nothing
