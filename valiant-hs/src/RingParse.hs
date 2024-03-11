{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RingParse where

import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Ring
import SparseMatrix
import Vec

type Name = String

data Symbol a = Nonterminal Name | Terminal a
  deriving (Ord, Eq)

instance Show a => Show (Symbol a) where
  show (Nonterminal n) = '/' : n ++ "/"
  show (Terminal a) = '\'' : show a

data ProductionRule a where
  Binary :: Name -> Symbol a -> Symbol a -> ProductionRule a
  Unary :: Name -> Symbol a -> ProductionRule a

type ProductionRules a = [ProductionRule a]

instance (Show a) => Show (ProductionRule a) where
  show rule = show n ++ " -> " ++ concatMap show s
    where
      s :: [Symbol a]
      n :: Symbol a
      (s, n) = case rule of
        (Unary n a) -> ([a], Nonterminal n)
        (Binary n a b) -> ([a, b], Nonterminal n)

binApp (Binary n a b) x y = if x == a && b == y then return $ Nonterminal n else Nothing
binApp Unary {} _ _ = Nothing

unaryApp (Unary n a) x = if x == a then return $ Nonterminal n else Nothing
unaryApp Binary {} _ = Nothing

newtype RingParse a = RingParse {getSyms :: S.Set (Symbol a)}

instance Show a => Show (RingParse a) where
  show (RingParse s) = show s

productionRules :: ProductionRules String
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

instance a ~ String => Ring (RingParse a) where
  zero = RingParse mempty
  add (RingParse sa) (RingParse sb) = RingParse $ S.union sa sb
  mul (RingParse x) (RingParse y) = RingParse sc
    where
      s = catMaybes [binApp a a_0 a_1 | a_0 <- S.toList x, a_1 <- S.toList y, a <- productionRules]
      sc = S.fromList s

syms = map Terminal $ words "she eats a fish with a fork"

applyUnaryOp = catMaybes . (<$> productionRules) . flip unaryApp

unaryOps = applyUnaryOp <$> syms

opRing :: [RingParse String]
opRing = map (RingParse . S.fromList) unaryOps

__f = a `mul` b
  where
    (a : b : _) = opRing

-- [[a   ], [b], [c ], [d]]
-- [[ab  ], [ ], [cd]]
-- [[    ], [ ]]
-- [[abcd]]
applyBinOp :: [[Symbol String]] -> [[Symbol String]]
applyBinOp syms =
  zipWith (\a b -> catMaybes $ binApp <$> productionRules <*> a <*> b) syms $ tail syms

-- TODO: check patterns (n-1) e.g. CYK algo

-- TODO: this is NOT how to construct a matrix
__a =
  UpperRightTriangularMatrix
    ( UpperRightTriangularMatrix
        (UpperRightTriangularMatrix (UnitMatrix a) Empty (UnitMatrix b))
        Empty
        (UpperRightTriangularMatrix (UnitMatrix c) Empty (UnitMatrix d))
    )
    Empty
    ( UpperRightTriangularMatrix
        (UpperRightTriangularMatrix (UnitMatrix e) Empty (UnitMatrix f))
        Empty
        (UpperRightTriangularMatrix (UnitMatrix g) Empty (UnitMatrix h))
    )
  where
    [a, b, c, d, e, f, g, h] = opRing ++ [RingParse $ S.fromList []]
