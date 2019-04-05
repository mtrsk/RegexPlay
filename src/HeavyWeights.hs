module HeavyWeights where

import           EffMatchingW
import           Semiring

submatchW :: Semiring s => RegW (Int, c) s -> [c] -> s
submatchW r s = matchW (seqW arb (seqW r arb)) (zip [0 ..] s)
  where
    arb = repW (symW (const one))

symI :: SemiringI s => Char -> RegW (Int, Char) s
symI c = symW weight
  where
    weight (pos, x)
      | x == c = index pos
      | otherwise = zero

data Leftmost
  = NoLeft
  | Leftmost Start
  deriving (Show)

data Start
  = NoStart
  | Start Int
  deriving (Show)

instance Semiring Leftmost where
  zero = NoLeft
  one = Leftmost NoStart
  -- Addition
  NoLeft <+> x = x
  x <+> NoLeft = x
  Leftmost x <+> Leftmost y = Leftmost (leftmost x y)
    where
      leftmost NoStart NoStart     = NoStart
      leftmost NoStart (Start i)   = Start i
      leftmost (Start i) NoStart   = Start i
      leftmost (Start i) (Start j) = Start (min i j)
  -- Multiplication
  NoLeft <.> _ = NoLeft
  _ <.> NoLeft = NoLeft
  Leftmost x <.> Leftmost y = Leftmost (start x y)
    where
      start NoStart s = s
      start s _       = s

instance Semiring.SemiringI Leftmost where
  index = Leftmost . Start

data LeftLong
  = NoLeftLong
  | LeftLong Range
  deriving (Show)

data Range
  = NoRange
  | Range Int
          Int
  deriving (Show)

instance Semiring LeftLong where
  zero = NoLeftLong
  one = LeftLong NoRange
  -- Addition
  NoLeftLong <+> x = x
  x <+> NoLeftLong = x
  LeftLong x <+> LeftLong y = LeftLong (leftlong x y)
    where
      leftlong (Range i j) (Range k l)
        | i < k || i == k && j >= l = Range i j
        | otherwise = Range k l
  -- Multiplication
  NoLeftLong <.> _ = NoLeftLong
  _ <.> NoLeftLong = NoLeftLong
  LeftLong x <.> LeftLong y = LeftLong (range x y)
    where
      range NoRange r               = r
      range r NoRange               = r
      range (Range i _) (Range _ j) = Range i j

instance SemiringI LeftLong where
  index i = LeftLong (Range i i)
