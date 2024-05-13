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

symNameEq :: (Eq nt, Eq t) => Symbol nt t -> Symbol nt t -> Bool
symNameEq (Nonterminal a _) (Nonterminal b _) = a == b
symNameEq (Terminal a) (Terminal b) = a == b
symNameEq _ _ = False

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

instance (Ord a) => Semigroup (RingParse a) where
  (<>) (RingParse as) (RingParse bs) = RingParse $ as <> bs

instance (Ord a) => Monoid (RingParse a) where
  mempty = RingParse mempty

instance (Show a) => Show (RingParse a) where
  show (RingParse s) = show $ foldr (:) [] s

instance (Ord b, Ord a) => Ring (ProductionRules a b -> RingParse (Symbol a b)) where
  zero = const $ RingParse mempty
  add fa fb prods =
    let (RingParse sa) = fa prods
        (RingParse sb) = fb prods
     in RingParse $ (<>) sa sb
  mul fx fy prods =
    let (RingParse x) = fx prods
        (RingParse y) = fy prods
        toL = foldr (:) []
        s = catMaybes [binApp a a_0 a_1 | a_0 <- toL x, a_1 <- toL y, a <- prods]
     in RingParse $ foldr S.insert S.empty s
