module Semiring where

class Semiring a where
  zero, one :: a
  (<.>) :: a -> a -> a
  (<+>) :: a -> a -> a

infixl 7 <.>

infixl 6 <+>
