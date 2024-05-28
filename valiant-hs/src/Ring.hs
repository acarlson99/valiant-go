module Ring where

import Data.Set (Set, fromList, toList, union)

-- | The 'Ring' class is used for ring-like datatypes.
--
-- The following operator interactions are expected to hold:
--
-- 1. @x + 0@ = @x@
-- 2. @0 + x@ = @x@
-- 3. @(x + y) + z@ = @x + (y + z)@
-- 4. @x · (y + z)@ = @(x · y) + (x · z)@
-- 5. @x · 0@ = @0@
-- 6. @0 · x@ = @0@
class Ring a where
  zero :: a
  add :: a -> a -> a
  mul :: a -> a -> a

verifyRing :: (Show a, Eq a, Ring a) => a -> a
verifyRing one =
  if and
    [ x /= y,
      x /= z,
      y /= z,
      (x `add` zero) == x,
      x == (x `add` zero),
      (x `add` y) `add` z == x `add` (y `add` z),
      x `mul` (y `add` z) == (x `mul` y) `add` (x `mul` z),
      x `mul` zero == zero,
      zero `mul` x == zero
    ]
    then one
    else zero
  where
    x = one
    y = one `add` one
    z = y `add` one

instance (Ring a) => Ring (Maybe a) where
  zero = Nothing
  add (Just a) (Just b) = Just (a `add` b)
  add a Nothing = a
  add Nothing b = b
  mul (Just a) (Just b) = Just (a `mul` b)
  mul _ Nothing = Nothing
  mul Nothing _ = Nothing

instance Ring Int where
  zero = 0
  add = (+)
  mul = (*)

newtype RingInteger = RingInteger Integer
  deriving (Num, Show)

instance Ring RingInteger where
  zero = 0
  add = (+)
  mul = (*)

instance Semigroup RingInteger where
  (<>) = (+)

instance Monoid RingInteger where
  mempty = 0

instance (Ring a, Ord a) => Ring (Set a) where
  zero = mempty
  add = union
  mul a b = fromList $ add <$> toList a <*> toList b

instance (Ring a, Ring b) => Ring (a, b) where
  zero = (zero, zero)
  add (aa, ab) (ba, bb) = (aa `add` ba, ab `add` bb)
  mul (aa, ab) (ba, bb) = (aa `mul` ba, ab `mul` bb)
