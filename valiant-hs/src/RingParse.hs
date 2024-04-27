{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RingParse where

import Control.Applicative
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Ring
import SparseMatrix
import Vec

data Symbol nt t = Nonterminal nt | Terminal t
  deriving (Ord, Eq)

instance (Show t, Show nt) => Show (Symbol nt t) where
  show (Nonterminal n) = '/' : show n ++ "/"
  show (Terminal a) = '\'' : show a

data ProductionRule nt t where
  Binary :: nt -> Symbol nt t -> Symbol nt t -> ProductionRule t nt
  Unary :: nt -> Symbol nt t -> ProductionRule t nt

type ProductionRules t nt = [ProductionRule t nt]

instance (Show t, Show nt) => Show (ProductionRule t nt) where
  show rule = show n ++ " -> " ++ concatMap show s
    where
      s :: [Symbol nt t]
      n :: Symbol nt t
      (n, s) = case rule of
        (Unary n a) -> (Nonterminal n, [a])
        (Binary n a b) -> (Nonterminal n, [a, b])

binApp (Binary n a b) x y = if x == a && b == y then return $ Nonterminal n else Nothing
binApp Unary {} _ _ = Nothing

unaryApp (Unary n a) x = if x == a then return $ Nonterminal n else Nothing
unaryApp Binary {} _ = Nothing

newtype RingParse f a = RingParse {getSyms :: f a}

instance Eq (f a) => Eq (RingParse f a) where
  a == b = getSyms a == getSyms b

instance Functor f => Functor (RingParse f) where
  fmap f = RingParse . fmap f . getSyms

instance Applicative f => Applicative (RingParse f) where
  pure = RingParse . pure
  (<*>) (RingParse fs) (RingParse v) = RingParse $ fs <*> v

liftRPF :: (ma a -> mb b -> mc c) -> RingParse ma a -> RingParse mb b -> RingParse mc c
liftRPF f (RingParse as) (RingParse bs) = RingParse $ f as bs

instance (Applicative f, Semigroup a) => Semigroup (RingParse f a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Ord a, Applicative f, Monoid (f a)) => Monoid (RingParse f a) where
  mempty = RingParse mempty

instance (Show a, Foldable f) => Show (RingParse f a) where
  show (RingParse s) = show $ foldr (:) [] s

productionRules :: ProductionRules String String
productionRules =
  [ Binary "S" (Nonterminal "NP") (Nonterminal "VP"),
    Binary "VP" (Nonterminal "VP") (Nonterminal "PP"),
    Binary "VP" (Nonterminal "V") (Nonterminal "NP"),
    Unary "VP" (Terminal "eats"),
    Binary "PP" (Nonterminal "P") (Nonterminal "NP"),
    Binary "NP" (Nonterminal "Det") (Nonterminal "N"),
    Unary "NP" (Terminal "she"),
    Unary "V" (Terminal "eats"),
    Unary "P" (Terminal "with"),
    Unary "N" (Terminal "fish"),
    Unary "N" (Terminal "fork"),
    Unary "Det" (Terminal "a")
  ]

-- TODO: change this def to foldable+monoid
-- instance (Foldable f, Monoid (f a)) => Ring (RingParse f a) where
instance (f ~ S.Set, a ~ Symbol String String) => Ring (RingParse f a) where
  zero = RingParse mempty
  add (RingParse sa) (RingParse sb) = RingParse $ (<>) sa sb
  mul (RingParse x) (RingParse y) = RingParse sc
    where
      s = catMaybes [binApp a a_0 a_1 | a_0 <- S.toList x, a_1 <- S.toList y, a <- productionRules]
      sc = S.fromList s

syms = map Terminal $ words "she eats a fish with a fork"

applyUnaryOp = catMaybes . (<$> productionRules) . flip unaryApp

unaryOps = applyUnaryOp <$> syms

opRing :: [RingParse S.Set (Symbol String String)]
opRing = map (RingParse . S.fromList) unaryOps

__f = a `mul` b
  where
    (a : b : _) = opRing

-- [[a   ], [b], [c ], [d]]
-- [[ab  ], [ ], [cd]]
-- [[    ], [ ]]
-- [[abcd]]
applyBinOp :: [[Symbol String String]] -> [[Symbol String String]]
applyBinOp syms =
  zipWith (\a b -> catMaybes $ binApp <$> productionRules <*> a <*> b) syms $ tail syms

-- TODO: check patterns (n-1) e.g. CYK algo
-- zipWith mul opRing (tail opRing)

-- TODO: better way to how to construct a matrix
__a =
  UpperRightTriangularMatrix
    ( UpperRightTriangularMatrix
        (UpperRightTriangularMatrix Empty (UnitMatrix a) Empty)
        (SquareMatrix Empty Empty (UnitMatrix b) Empty)
        (UpperRightTriangularMatrix Empty (UnitMatrix c) Empty)
    )
    ( SquareMatrix
        Empty
        Empty
        (SquareMatrix Empty Empty (UnitMatrix d) Empty)
        Empty
    )
    ( UpperRightTriangularMatrix
        (UpperRightTriangularMatrix Empty (UnitMatrix e) Empty)
        (SquareMatrix Empty Empty (UnitMatrix f) Empty)
        (UpperRightTriangularMatrix Empty (UnitMatrix g) Empty)
    )
  where
    [a, b, c, d, e, f, g] = opRing

-- [a, b, c, d, e, f, g] = [1 .. 7]
-- a `mul` ((b `mul` (c `mul` d)) `mul` (e `mul` (f `mul` g)))
