module Ring where

import Data.Set (Set, fromList, toList, union)
import SparseMatrix

class Ring a where
  add :: a -> a -> a
  mul :: a -> a -> a

instance Ring a => Ring (Matrix a) where
  add (SquareMatrix size aa ab ac ad) (SquareMatrix _ ba bb bc bd) =
    SquareMatrix size (aa `add` ba) (ab `add` bb) (ac `add` bc) (ad `add` bd)
  add (UpperRightTriangularMatrix size aa ab ad) (UpperRightTriangularMatrix _ ba bb bd) =
    UpperRightTriangularMatrix size (aa `add` ba) (ab `add` bb) (ad `add` bd)
  add (UnitMatrix a) (UnitMatrix b) = UnitMatrix $ a `add` b
  add (Empty size) Empty {} = Empty size
  add _ _ = undefined

  mul (SquareMatrix size aa ab ac ad) (SquareMatrix _ ba bb bc bd) =
    SquareMatrix size (aa `mul` ba) (ab `mul` bb) (ac `mul` bc) (ad `mul` bd)
  mul (UpperRightTriangularMatrix size aa ab ad) (UpperRightTriangularMatrix _ ba bb bd) =
    UpperRightTriangularMatrix size (aa `mul` ba) (ab `mul` bb) (ad `mul` bd)
  mul (UnitMatrix a) (UnitMatrix b) = UnitMatrix $ a `mul` b
  mul (Empty size) Empty {} = Empty size
  mul _ _ = undefined

-- instance Ring a => Ring (SquareMatrix a) where
--   add a b =
--     M
--       (sqMatrixSize a)
--       (sqUl a `add` sqUl b)
--       (sqUr a `add` sqUr b)
--       (sqBl a `add` sqBl b)
--       (sqBr a `add` sqBr b)
--   mul a b =
--     M
--       (urtMatrixSize a)
--       (urtUl a `mul` urtUl b)
--       (urtUr a `mul` urtUr b)
--       (urtBl a `mul` urtBl b)
--       (urtBr a `mul` urtBr b)

instance Ring a => Ring (Maybe a) where
  add (Just a) (Just b) = Just (a `add` b)
  add a Nothing = a
  add Nothing b = b
  mul (Just a) (Just b) = Just (a `mul` b)
  mul a Nothing = a
  mul Nothing b = b

instance Ring Int where
  add = (+)
  mul = (*)

instance (Ring a, Ord a) => Ring (Set a) where
  add a b = fromList $ add <$> toList a <*> toList b
  mul = union

instance (Ring a, Ring b) => Ring (a, b) where
  add (aa, ab) (ba, bb) = (aa `add` ba, ab `add` bb)
  mul (aa, ab) (ba, bb) = (aa `mul` ba, ab `mul` bb)
