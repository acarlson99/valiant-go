{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RingParse where

import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Ring

--                             A -> BC          | "fish"
data Symbol nt t = Nonterminal nt [Symbol nt t] | Terminal t
  deriving (Eq, Ord)

symNameEq (Nonterminal a _) (Nonterminal b _) = a == b
symNameEq (Terminal a) (Terminal b) = a == b
symNameEq _ _ = False

symChildren (Nonterminal _ xs) = xs
symChildren _ = []

instance (Show t, Show nt) => Show (Symbol nt t) where
  show (Nonterminal n _) = '/' : show n ++ "/"
  show (Terminal a) = '\'' : show a

data ProductionRule nt t where
  Binary :: nt -> Symbol nt t -> Symbol nt t -> ProductionRule nt t
  Unary :: nt -> Symbol nt t -> ProductionRule nt t

type ProductionRules nt t = [ProductionRule nt t]

instance (Show t, Show nt) => Show (ProductionRule nt t) where
  show rule = show s ++ " -> " ++ concatMap show ss
    where
      ss :: [Symbol nt t]
      s :: Symbol nt t
      (s, ss) = case rule of
        (Unary n a) -> (Nonterminal n [], [a])
        (Binary n a b) -> (Nonterminal n [], [a, b])

binApp :: (Eq nt, Eq t) => ProductionRule nt t -> Symbol nt t -> Symbol nt t -> Maybe (Symbol nt t)
binApp (Binary n a b) x y = if x `symNameEq` a && b `symNameEq` y then return $ Nonterminal n [x, y] else Nothing
binApp Unary {} _ _ = Nothing

unaryApp :: (Eq nt, Eq t) => ProductionRule nt t -> Symbol nt t -> Maybe (Symbol nt t)
unaryApp (Unary n a) x = if x `symNameEq` a then return $ Nonterminal n [x] else Nothing
unaryApp Binary {} _ = Nothing

newtype RingParse a = RingParse {getSyms :: S.Set a}

instance (Eq a) => Eq (RingParse a) where
  a == b = getSyms a == getSyms b

liftRPF :: (S.Set a -> S.Set b -> S.Set c) -> RingParse a -> RingParse b -> RingParse c
liftRPF f (RingParse as) (RingParse bs) = RingParse $ f as bs

instance (Ord a) => Semigroup (RingParse a) where
  (<>) (RingParse as) (RingParse bs) = RingParse $ as <> bs

instance (Ord a) => Monoid (RingParse a) where
  mempty = RingParse mempty

instance (Show a) => Show (RingParse a) where
  show (RingParse s) = show $ foldr (:) [] s

productionRules :: ProductionRules String String
productionRules =
  [ Binary "S" (Nonterminal "NP" []) (Nonterminal "VP" []),
    Binary "VP" (Nonterminal "VP" []) (Nonterminal "PP" []),
    Binary "VP" (Nonterminal "V" []) (Nonterminal "NP" []),
    Unary "VP" (Terminal "eats"),
    Binary "PP" (Nonterminal "P" []) (Nonterminal "NP" []),
    Binary "NP" (Nonterminal "Det" []) (Nonterminal "N" []),
    Unary "NP" (Terminal "she"),
    Unary "V" (Terminal "eats"),
    Unary "P" (Terminal "with"),
    Unary "N" (Terminal "fish"),
    Unary "N" (Terminal "fork"),
    Unary "Det" (Terminal "a")
  ]

instance (a ~ Symbol String String) => Ring (RingParse a) where
  zero = RingParse mempty
  add (RingParse sa) (RingParse sb) = RingParse $ (<>) sa sb
  mul (RingParse x) (RingParse y) = RingParse $ foldr S.insert S.empty s
    where
      toL xs = foldr (:) [] xs
      s = catMaybes [binApp a a_0 a_1 | a_0 <- toL x, a_1 <- toL y, a <- productionRules]

instance (a ~ Symbol String String) => Ring (ProductionRules String String -> RingParse a) where
  zero = const $ RingParse mempty
  add fa fb = \pr ->
    let (RingParse sa) = fa pr
        (RingParse sb) = fb pr
     in RingParse $ (<>) sa sb
  mul fx fy = \pr ->
    let (RingParse x) = fx pr
        (RingParse y) = fy pr
        toL xs = S.toList xs
        s = catMaybes [binApp a a_0 a_1 | a_0 <- toL x, a_1 <- toL y, a <- pr]
     in RingParse $ foldr S.insert S.empty s

syms = map Terminal $ words "she eats a fish with a fork"

applyUnaryOp = catMaybes . (<$> productionRules) . flip unaryApp

unaryOps = applyUnaryOp <$> syms

opRing :: [RingParse (Symbol String String)]
opRing = map (RingParse . S.fromList) unaryOps

__f :: ProductionRules String String -> RingParse (Symbol String String)
__f = const a `mul` const b
  where
    (a : b : _) = opRing

-- [[a   ], [b], [c ], [d]]
-- [[ab  ], [ ], [cd]]
-- [[    ], [ ]]
-- [[abcd]]
applyBinOp :: (a ~ String) => [[Symbol a a]] -> ProductionRules a a -> [[Symbol a a]]
applyBinOp syms productionRules =
  zipWith (\a b -> catMaybes $ binApp <$> productionRules <*> a <*> b) syms $ tail syms
