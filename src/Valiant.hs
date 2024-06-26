{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Valiant where

import Data.Maybe
import Data.Set qualified as S
import Grammar.Chomsky
import Grammar.ContextFree (bnfCFG)
import Grammar.Convert (bnfGram, convert)
import Matrix.N
import Matrix.Sparse
import Nat
import Ring
import RingParse
import Tree qualified as T
import VecN

-- see Bernardy and Claessen, “Efficient Divide-and-Conquer Parsing of Practical Context-Free Languages.”
class Valiant a where
  v :: a -> a -> a -> a

instance (Ring a) => Valiant (Matrix n a) where
  v _ Empty _ = Empty
  v _ (UnitMatrix a) _ = UnitMatrix a
  v (UpperRightTriangularMatrix a11 a12 a22) (SquareMatrix x11 x12 x21 x22) (UpperRightTriangularMatrix b11 b12 b22) = SquareMatrix y11 y12 y21 y22
    where
      y21 = v a22 x21 b11
      y11 = v a11 (x11 `add` (a12 `mul` y21)) b11
      y22 = v a22 (x22 `add` (y21 `mul` b12)) b22
      y12 = v a11 (x12 `add` (a12 `mul` y22) `add` (y11 `mul` b12)) b22
  v _ _ _ = undefined

bin :: (Valiant (Matrix n a)) => Matrix ('Succ n) a -> Matrix ('Succ n) a
bin (UpperRightTriangularMatrix a t b) = UpperRightTriangularMatrix a (v a t b) b
bin _ = undefined

-- used for recursively calculating `v` for submatrices
class (Valiant a) => RunV a where
  runV :: a -> a

instance (Ring a) => RunV (Matrix n a) where
  runV (UpperRightTriangularMatrix a t b) =
    let a' = runV a
        t' = runV $ v a' t b'
        b' = runV b
     in UpperRightTriangularMatrix a' t' b'
  runV x = x

liftV :: (Ring a) => MatrixN a -> MatrixN a
liftV (MatrixN n m) = MatrixN n $ runV m

runThingy :: (Ord a, Ring (b -> RingParse a)) => b -> [RingParse a] -> Maybe (RingParse a)
runThingy productionRules syms =
  let ls = listToVecN $ map const syms
      m' = vecNToValiantMatrixN mempty ls
   in case m' of MatrixN _ m -> matrixTopRightMost (runV m) <*> pure productionRules

-- sequenceTreeMat :: M.Matrix n (T.Tree (a -> b)) -> a -> M.Matrix n (T.Tree b)
sequenceTreeMat :: (Functor f1, Functor f2) => f1 (f2 (a -> b)) -> a -> f1 (f2 b)
sequenceTreeMat b a = fmap (fmap ($ a)) b

symToTree :: Symbol nt t -> T.Tree (Either nt t)
symToTree (Nonterminal x' xs) =
  let x = Left x'
   in case xs of
        [a, b] -> T.Node x (symToTree a) (symToTree b)
        [a] -> T.Node x (symToTree a) T.Empty
        _ -> T.Node x T.Empty T.Empty
symToTree (Terminal x) = T.Node (Right x) T.Empty T.Empty

-- Parse a list of `tokens` using `productionRules` into parse trees
-- TODO: sequence Maybe [T.Tree a] -> Maybe T.Tree [a]
valiantParse :: (nt ~ String, t ~ String) => ProductionRules nt t -> [String] -> [T.Tree (Either nt t)]
valiantParse productionRules tokens =
  let strToRingParse = RingParse . S.fromList . catMaybes . (<$> productionRules) . flip unaryApp . Terminal
      opRing = map strToRingParse tokens
      -- TODO: v this hack (making all rules a tuple of rules,RingParse) is unacceptable-- please fix
      mat = (,) productionRules <$> vecNToValiantMatrixN mempty (listToVecN opRing)
   in maybe
        []
        (map symToTree . S.toList . getSyms)
        ( liftMatF
            (fmap snd . matrixTopRightMost)
            (liftV mat)
        )

-- test

-- tokens = ["<", "i", "d", "e", "n", "t", ">", ":", ":", "=", " ", "\"", "a", "\""]
tokens = ["<", "P", ">", " ", ":", ":", "=", " ", "<", "P", ">", "<", "P", ">", "\n"] --
-- tokens = ["<", "P", ">", " ", ":", ":", "=", " ", "<", "P", ">", "\n"] --

productionRules = bnfGram

strToRingParse = RingParse . S.fromList . catMaybes . (<$> productionRules) . flip unaryApp . Terminal

opRing' ts = map strToRingParse ts

mat ts = (,) productionRules <$> vecNToValiantMatrixN mempty (listToVecN $ opRing' ts)

showM m = fmap snd m

-- [a,b,c,d,e,f,g,h,i,j,k,l] = map const $ opRing' tokens
-- ($ productionRules) $ (a `mul` (b `mul` (c `mul` (d `mul` (e `mul` (f `mul` (g `mul` (h `mul` ((i `mul` (j `mul` k)) `mul` l)))))))))

-- EXAMPLE

_productionRules :: ProductionRules String String
_productionRules =
  [ Binary "S" (newNonTerm "NP") (newNonTerm "VP"),
    Binary "VP" (newNonTerm "VP") (newNonTerm "PP"),
    Binary "VP" (newNonTerm "V") (newNonTerm "NP"),
    Unary "VP" (newTerm "eats"),
    Binary "PP" (newNonTerm "P") (newNonTerm "NP"),
    Binary "NP" (newNonTerm "Det") (newNonTerm "N"),
    Unary "NP" (newTerm "she"),
    Unary "V" (newTerm "eats"),
    Unary "P" (newTerm "with"),
    Unary "N" (newTerm "fish"),
    Unary "N" (newTerm "fork"),
    Unary "Det" (newTerm "a")
  ]

toks :: [String]
toks = words "she eats a fish with a fork"

res :: [T.Tree (Either String String)]
res = valiantParse _productionRules toks

-- instance (a ~ Symbol String String) => Ring (RingParse a) where
--   zero = RingParse mempty
--   add (RingParse sa) (RingParse sb) = RingParse $ (<>) sa sb
--   mul (RingParse x) (RingParse y) = RingParse $ foldr S.insert S.empty s
--     where
--       toL = foldr (:) []
--       s = catMaybes [binApp a a_0 a_1 | a_0 <- toL x, a_1 <- toL y, a <- _productionRules]

-- syms = map Terminal toks

-- applyUnaryOp = catMaybes . (<$> _productionRules) . flip unaryApp

-- unaryOps = applyUnaryOp <$> syms

-- opRing :: [RingParse (Symbol String String)]
-- opRing = map (RingParse . S.fromList) unaryOps

-- __a :: Matrix ('Succ ('Succ ('Succ 'Zero))) (RingParse (Symbol String String))
-- __a =
--   ($ _productionRules) . pure
--     <$> UpperRightTriangularMatrix
--       ( UpperRightTriangularMatrix
--           (UpperRightTriangularMatrix Empty (UnitMatrix a) Empty)
--           (SquareMatrix Empty Empty (UnitMatrix b) Empty)
--           (UpperRightTriangularMatrix Empty (UnitMatrix c) Empty)
--       )
--       ( SquareMatrix
--           Empty
--           Empty
--           (SquareMatrix Empty Empty (UnitMatrix d) Empty)
--           Empty
--       )
--       ( UpperRightTriangularMatrix
--           (UpperRightTriangularMatrix Empty (UnitMatrix e) Empty)
--           (SquareMatrix Empty Empty (UnitMatrix f) Empty)
--           (UpperRightTriangularMatrix Empty (UnitMatrix g) Empty)
--       )
--   where
--     [a, b, c, d, e, f, g] = opRing

-- [a, b, c, d, e, f, g] = [1 .. 7]
-- a `mul` ((b `mul` (c `mul` d)) `mul` (e `mul` (f `mul` g)))
__a :: [String] -> Matrix (Succ (Succ (Succ (Succ Zero)))) (ProductionRules String String, RingParse (Symbol String String))
__a ts =
  (,) productionRules
    <$> UpperRightTriangularMatrix
      ( UpperRightTriangularMatrix
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
      )
      ( SquareMatrix
          Empty
          Empty
          ( SquareMatrix
              Empty
              Empty
              (SquareMatrix Empty Empty (UnitMatrix h) Empty)
              Empty
          )
          Empty
      )
      ( UpperRightTriangularMatrix
          ( UpperRightTriangularMatrix
              (UpperRightTriangularMatrix Empty (UnitMatrix i) Empty)
              (SquareMatrix Empty Empty (UnitMatrix j) Empty)
              (UpperRightTriangularMatrix Empty (UnitMatrix k) Empty)
          )
          ( SquareMatrix
              Empty
              Empty
              (SquareMatrix Empty Empty (UnitMatrix l) Empty)
              Empty
          )
          ( UpperRightTriangularMatrix
              (UpperRightTriangularMatrix Empty Empty Empty)
              (SquareMatrix Empty Empty Empty Empty)
              (UpperRightTriangularMatrix Empty Empty Empty)
          )
      )
  where
    (a : b : c : d : e : f : g : h : i : j : k : l : []) = opRing' ts
