module SparseMatrix where

import           Data.Set
import           GHC.Float

data SquareMatrix a = M
  { matrixSize :: Int
  , ul         :: Maybe a
  , ur         :: Maybe a
  , bl         :: Maybe a
  , br         :: Maybe a
  } deriving (Eq)

instance Show a => Show (SquareMatrix a) where
  show mx = undefined

nextClosestSquare :: Int -> Int
nextClosestSquare n =
  head $ dropWhile (< n) [ float2Int (2 ** x) | x <- [1 ..] ]

newSquareMatrix :: Int -> SquareMatrix a
newSquareMatrix n = M (nextClosestSquare n) Nothing Nothing Nothing Nothing

class Ring a where
  add :: a -> a -> a
  mul :: a -> a -> a

instance Ring a => Ring (SquareMatrix a) where
  add a b = M (matrixSize a)
              (ul a `add` ul b)
              (ur a `add` ur b)
              (bl a `add` bl b)
              (br a `add` br b)
  mul a b = M (matrixSize a)
              (ul a `mul` ul b)
              (ur a `mul` ur b)
              (bl a `mul` bl b)
              (br a `mul` br b)

instance Ring a => Ring (Maybe a) where
  add (Just a) (Just b) = Just (a `add` b)
  add a        Nothing  = a
  add Nothing  b        = b
  mul (Just a) (Just b) = Just (a `mul` b)
  mul a        Nothing  = a
  mul Nothing  b        = b

instance Ring Int where
  add = (+)
  mul = (*)

instance (Ring a,Ord a) => Ring (Set a) where
  add a b = fromList $ add <$> toList a <*> toList b
  mul = union

instance (Ring a, Ring b)=> Ring (a,b) where
  add (aa, ab) (ba, bb) = (aa `add` ba, ab `add` bb)
  mul (aa, ab) (ba, bb) = (aa `mul` ba, ab `mul` bb)
