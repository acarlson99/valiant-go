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

module TestTypes where

import Control.Applicative
import Control.Arrow
import qualified Data.Bifunctor
import Data.Type.Equality
import Nat
import RingParse
import SparseMatrix
import Vec

-- data Natty :: Nat -> * where
--   Zy :: Natty 'Zero
--   Sy :: Natty n -> Natty ('Succ n)

-- class NATTY (n :: Nat) where
--   natty :: Natty n

-- instance NATTY 'Zero where
--   natty = Zy

-- instance NATTY n => NATTY ('Succ n) where
--   natty = Sy natty

data LenList :: * -> * where
  LenList :: SNatl n => SNat n -> Vec n a -> LenList a

-- LenList :: SNatl n => Vec n a -> LenList a

instance (Show a) => Show (LenList a) where
  show (LenList _ a) = show a

lenList :: [a] -> LenList a
lenList [] = LenList snat VNil
lenList (x : xs) = case lenList xs of LenList n ys -> LenList (SSucc n) (VCons x ys)

data LengthyListy :: * -> * where
  LengthyListy :: SNatl m => Vec (ExpFour m) a -> Matrix m a -> LengthyListy a

-- a = LengthyListy (lenList [1, 2, 3]) (return 1)

fnuc (LengthyListy v n) = undefined

shift :: (SNatl m, SNatl n) => Vec (Succ m) a -> Vec n a -> (Vec m a, Vec (Succ n) a)
shift (VCons a as) bs = (as, VCons a bs)

class ConstructShape a where
  constructShape :: Vec (ExpFour (SqDepth b)) a -> b

class TakeN (n :: Nat) where
  takeN :: [a] -> Maybe (Vec n a)

instance TakeN 'Zero where
  takeN _ = Just VNil

instance (SNatl n, TakeN n) => TakeN ('Succ n) where
  takeN [] = Nothing
  takeN (x : xs) =
    case takeN @n xs of
      Just vec -> Just (VCons x vec)
      Nothing -> Nothing

-- instance TakeN ('Succ n) where
--   takeN :: [a] -> Maybe (Vec ('Succ n) a)
--   takeN (x : xs) = case takeN xs :: Maybe (Vec n a) of
--     Just a -> undefined
--     Nothing -> Nothing
--   takeN _ = Nothing

-- data Equal a b where
--   Refl :: Equal a a

-- newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

data TypedMat where
  (:::) :: Matrix t a -> SNat t -> TypedMat

data TypedVec where
  (:::-) :: Vec n a -> SNat n -> TypedVec

-- (=?=) :: SNat s -> SNat t -> Maybe (s :~: t)
-- SZero =?= SZero = Just Refl
-- SSucc a =?= SSucc b =
--   -- (a =?= b) >>>=== (\c -> return Refl) -- this does not work
--   case a =?= b of
--     Nothing -> Nothing
--     Just Refl -> Just Refl
-- _ =?= _ = Nothing

class SNatEq s t where
  (=?=) :: s -> t -> Maybe (s :~: t)

instance SNatEq (Vec n a) (Vec m a) where
  VNil =?= VNil = Just Refl
  (VCons a as) =?= (VCons b bs) = case as =?= bs of
    Nothing -> Nothing
    Just Refl -> Just Refl
  _ =?= _ = Nothing

instance SNatEq (SNat s) (SNat t) where
  SZero =?= SZero = Just Refl
  SSucc a =?= SSucc b =
    -- (a =?= b) >>>=== (\c -> return Refl) -- this does not work
    case a =?= b of
      Nothing -> Nothing
      Just Refl -> Just Refl
  _ =?= _ = Nothing

instance SNatEq TypedMat TypedMat where
  (=?=) (a ::: at) (b ::: bt) =
    case at =?= bt of
      Nothing -> Nothing
      Just Refl -> Just Refl

instance SNatEq TypedVec TypedVec where
  (=?=) (a :::- at) (b :::- bt) =
    case at =?= bt of
      Nothing -> Nothing
      Just Refl -> Just Refl

-- f :: TypedVec -> TypedMat -> Maybe (SNat n)
-- f (a :::- at) (b ::: bt) =
--   case at =?= bt of
--     Nothing -> Nothing
--     Just m -> Just $ f_ at bt
--   where
--     f_ :: SNat n -> SNat n -> SNat n
--     f_ _ b = b

type family Flip f a b where
  Flip f a b = f b a

-- instance SNatEq TypedVec TypedMat where
--   (=?=) (a :::- at) (b ::: bt) =
--     case at =?= bt of
--       Nothing -> Nothing
--       Just Refl -> Just Refl

data LengthlessMat a where
  LengthlessMat :: Applicative (Matrix n) => Matrix (n :: Nat) a -> LengthlessMat a

instance Functor LengthlessMat where
  fmap f (LengthlessMat m) = LengthlessMat $ fmap f m

-- instance Applicative LengthlessMat where
--   pure a = LengthlessMat $ pure a
--   liftA2 f (LengthlessMat a) (LengthlessMat b) = undefined

-- LengthlessMat a -> __f f a
--   where
--     __f :: a -> b -> (a :~: b)
--     __f _ _ = Refl

-- instance Monad LengthlessMat where
--   (>>=) _ _ = undefined

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

-- class ConstructVMatrix n where
--   constructVMatrix :: Applicative (Matrix n) => Vec (ExpTwo n) Char -> Matrix n Char

-- instance ConstructVMatrix 'Zero where
--   constructVMatrix (VCons s VNil) = UnitMatrix s

-- instance (Size n, ConstructMatrix n) => ConstructVMatrix ('Succ n) where
--   constructVMatrix (VCons a _) = pure a
--   constructVMatrix VNil = Empty

class ConstructVMatrix n where
  constructVMatrix :: Applicative (Matrix n) => Vec (ExpTwo n) Char -> Matrix (Succ n) Char

instance ConstructVMatrix 'Zero where
  constructVMatrix (VCons s VNil) = UpperRightTriangularMatrix Empty (UnitMatrix s) Empty

-- TODO: this should split a matrix like so:
-- because `n` is always odd
-- n = (length(vec)-1) / 2
-- (as,rest) = splitN n vec
-- (x,bs) = splitN One rest
-- if bs =?= as then UpperRightTriangular (recurse as) (squareMatWithElemInBottomLeftCorner x) (recurse bs)
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

-- (=?=) :: forall k (a :: k). Nat -> Nat -> Maybe (a :~: a)
-- Zero =?= Zero = Just Refl
-- (Succ n) =?= (Succ m) = n =?= m
-- (Succ _) =?= Zero = Nothing
-- Zero =?= (Succ _) = Nothing

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
  doThingy :: [a] -> Vec n a

-- instance Thingy 'Zero where
--   doThingy xs = case listToVec @Zero xs of
--     Nothing -> doThingy @(Succ Zero) xs
--     Just v -> v

-- iterate on type until we get non-Nothing
it :: (Show (Vec n a), ListToVec n, SNatl n) => [a] -> Vec n a
it ls = helper ls SZero
  where
    helper :: forall n m a. (SNatl m, SNatl n) => (ListToVec m, ListToVec (Succ m)) => [a] -> SNat m -> Vec n a
    helper ls n = case listToVec @m ls of
      Just l -> case (snat @m) =?= (snat @n) of
        Nothing -> error "this should not happen"
        Just Refl -> l
      Nothing -> helper ls (SSucc n)

data MatrixN a where
  MatrixN :: SNat n -> Matrix n a -> MatrixN a

-- instance Show a => Show (MatrixN a) where
--   show mn = case mn of (MatrixN n m) -> show m

sqMatWithValInBottomLeft :: a -> SNat n -> Matrix n a
sqMatWithValInBottomLeft a SZero = UnitMatrix a
sqMatWithValInBottomLeft a (SSucc n) =
  let m = sqMatWithValInBottomLeft a n
   in SquareMatrix Empty Empty m Empty

-- TODO: this should split a matrix like so:
-- because `n` is always odd
-- n = (length(vec)-1) / 2
-- (as,rest) = splitN n vec
-- (x,bs) = splitN One rest
-- if bs =?= as then UpperRightTriangular (recurse as) (squareMatWithElemInBottomLeftCorner x) (recurse bs)
vecNToValiantMatrixN :: Monoid a => VecN a -> MatrixN a
vecNToValiantMatrixN (VecN SZero VNil) = MatrixN SZero Empty
vecNToValiantMatrixN (VecN l xs) =
  let h = snatHalf l
      (as, rest) = vecNSplitAt h mempty (VecN l xs)
      (b, cs') = vecNSplitFirst rest
      (cs, _) = vecNSplitAt h mempty cs'
      ul = vecNToValiantMatrixN as
      br = vecNToValiantMatrixN cs
      ur = sqMatWithValInBottomLeft b $ snat @One
   in case ul of
        (MatrixN n ulm) -> case br of
          (MatrixN m brm) ->
            case n =?= m of
              Just Refl ->
                MatrixN (SSucc n) $ UpperRightTriangularMatrix ulm Empty brm
              Nothing -> error "Recursing did not go well-- this should never happen"

-- -- TODO: quarter
-- -- TODO: shuffle into SqShape
-- vecHalve ::
--   ( Monoid a,
--     SNatl (Half n),
--     VecAppend (n - Half n),
--     SNatl n
--   ) =>
--   Vec n a ->
--   (Vec (Half n) a, Vec (Half n) a)
-- vecHalve (v :: Vec n a) = case as =?= bs of
--   Just Refl -> (as, bs)
--   Nothing -> case as =?= cs of
--     Nothing -> (as, undefined)
--     Just Refl -> (as, cs)
--   where
--     n = snat @n
--     hn = snatHalf n
--     (as, bs) = vecSplitAt hn mempty v
--     cs = vecAppend mempty bs

-- vecQuarter ::
--   ( Monoid a,
--     VecAppend (n - Half n),
--     VecAppend (Half n - Half (Half n)),
--     SNatl n,
--     SNatl (Half (Half n)),
--     SNatl (Half n),
--     ExpFour (Half (Half n)) ~ n
--   ) =>
--   Vec n a ->
--   ( (Vec (Half (Half n)) a, Vec (Half (Half n)) a),
--     (Vec (Half (Half n)) a, Vec (Half (Half n)) a)
--   )
-- vecQuarter v = Data.Bifunctor.bimap vecHalve vecHalve $ vecHalve v

-- vecQ ::
--   forall n a.
--   ( Monoid a,
--     SNatl n,
--     SNatl (Half n),
--     SNatl (Half (Half n)),
--     SNatl (ExpFour (Half (Half n))),
--     VecAppend (n - Half n),
--     VecAppend (Half n - Half (Half n))
--   ) =>
--   Vec n a ->
--   Maybe
--     ( (Vec (Half (Half n)) a, Vec (Half (Half n)) a),
--       (Vec (Half (Half n)) a, Vec (Half (Half n)) a)
--     )
-- vecQ v =
--   let an = snat @(ExpFour (Half (Half n)))
--       bn = snat @n
--    in case an =?= bn of
--         Just Refl -> Just $ vecQuarter v
--         Nothing -> Nothing

-- -- TODO: rewrite using TypedVec
-- vecQuarterSafe ::
--   ( Monoid a,
--     SNatl (Half (Half n)),
--     SNatl (Half n),
--     SNatl n,
--     SNatl (ExpFour (Half (Half n))),
--     SNatl (Half (Half ('Succ n))),
--     SNatl (Half ('Succ n)),
--     VecAppend (n - Half n),
--     VecAppend (Half n - Half (Half n)),
--     VecAppend ('Succ n - Half ('Succ n)),
--     VecAppend (Half ('Succ n) - Half (Half ('Succ n))),
--     VecAppend n,
--     ExpFour (Half (Half ('Succ n))) ~ 'Succ n,
--     Half (Half ('Succ n)) ~ Half (Half n),
--     SNatl (Half ('Succ (Half n))),
--     VecAppend ('Succ n - Half n),
--     SNatl (Half ('Succ (Half ('Succ n)))),
--     VecAppend ('Succ (Half n) - Half ('Succ (Half n))),
--     VecAppend ('Succ ('Succ n) - Half ('Succ n)),
--     VecAppend ('Succ (Half ('Succ n)) - Half ('Succ (Half ('Succ n)))),
--     VecAppend ('Succ ('Succ n) - Half n),
--     VecAppend ('Succ (Half n) - Half (Half n)),
--     VecAppend ('Succ ('Succ ('Succ n)) - Half ('Succ n)),
--     VecAppend ('Succ (Half ('Succ n)) - Half (Half n))
--   ) =>
--   Vec n a ->
--   ( (Vec (Half (Half n)) a, Vec (Half (Half n)) a),
--     (Vec (Half (Half n)) a, Vec (Half (Half n)) a)
--   )
-- vecQuarterSafe v = case vecQ v of
--   Just a -> a
--   Nothing -> vecQuarterSafe $ vecAppend mempty v

class VecToShape n where
  vecToShape :: Vec n a -> NDepthSq n a

vtll :: SNatl n => Vec n a -> LenList a
vtll = LenList snat

-- lenListQuarterSafe (LenList n v) =
--   case v of
--     (v_ :: (SNatl n, SNatl (Half n)) => Vec n a) ->
--       case vecQ v_ of
--         Just _ -> undefined
--         Nothing -> undefined
