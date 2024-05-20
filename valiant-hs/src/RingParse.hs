{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RingParse where

import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Grammar.Chomsky
import Ring

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
