module Tree where

import Ring

data Tree a = Node {val :: a, lhs :: Tree a, rhs :: Tree a} | Empty

instance (Show a) => Show (Tree a) where
  show (Node a l r) = "(" ++ show a ++ " [" ++ show l ++ "] " ++ " [" ++ show r ++ "] )"
  show Empty = ""

instance Functor Tree where
  fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)
  fmap _ Empty = Empty

instance Applicative Tree where
  pure a = Node a Empty Empty
  (<*>) ft (Node a l r) = Node (val ft a) (ft <*> l) (ft <*> r)
  (<*>) Empty _ = Empty
  (<*>) _ Empty = Empty

instance Foldable Tree where
  foldr f acc (Node a l r) =
    let acc' = foldr f acc l
     in foldr f (f a acc') r
  foldr _ acc Empty = acc

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Node a l r) = Node <$> f a <*> traverse f l <*> traverse f r

instance (Ring a) => Ring (Tree a) where
  zero = Node zero Empty Empty
  add ta tb = Node (val ta `add` val tb) ta tb
  mul ta tb = Node (val ta `mul` val tb) ta tb
