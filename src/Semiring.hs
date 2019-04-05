module Semiring where

class Semiring a where
  zero, one :: a
  (<.>) :: a -> a -> a
  (<+>) :: a -> a -> a

infixl 7 <.>

infixl 6 <+>

instance Semiring Bool where
  zero = False
  one = True
  (<+>) = (||)
  (<.>) = (&&)

instance Semiring Int where
  zero = 0
  one = 1
  (<+>) = (+)
  (<.>) = (*)
