module Ring where

import Data.Set
import SparseMatrix (Matrix)

class Ring a where
  add :: a -> a -> a
  mul :: a -> a -> a

instance Ring a => Ring (Matrix a) where
  add a b = undefined
  mul a b = undefined

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
