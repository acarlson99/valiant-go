module Ring where

import Data.Set (Set, fromList, toList, union)
import SparseMatrix

class Ring a where
  add :: a -> a -> a
  mul :: a -> a -> a

instance Ring a => Ring (Matrix a) where
  add = (<*>) . (add <$>)
  mul = (<*>) . (mul <$>)

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
