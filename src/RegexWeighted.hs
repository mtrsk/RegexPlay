module RegexWeighted where

import           RegexSimple
import           Semiring
import           Utils

data RegW c s
  = EpsW
  | SymW (c -> s)
  | AltW (RegW c s)
         (RegW c s)
  | SeqW (RegW c s)
         (RegW c s)
  | RepW (RegW c s)

sym :: Semiring s => Char -> RegW Char s
sym c =
  SymW
    (\x ->
       if x == c
         then one
         else zero)

weighted :: Semiring s => Reg -> RegW Char s
weighted Eps       = EpsW
weighted (Sym c)   = sym c
weighted (Alt p q) = AltW (weighted p) (weighted p)
weighted (Seq p q) = SeqW (weighted p) (weighted q)
weighted (Rep r)   = RepW (weighted r)

acceptW :: Semiring s => RegW c s -> [c] -> s
acceptW EpsW u =
  if null u
    then one
    else zero
acceptW (SymW f) u =
  case u of
    [c] -> f c
    _   -> zero
acceptW (AltW p q) u = acceptW p u <+> acceptW q u
acceptW (SeqW p q) u =
  sum' [acceptW p u1 <.> acceptW q u2 | (u1, u2) <- split u]
acceptW (RepW r) u = sum' [prod [acceptW r ui | ui <- ps] | ps <- parts u]

sum' :: Semiring s => [s] -> s
sum' = foldr (<+>) zero

prod :: Semiring s => [s] -> s
prod = foldr (<.>) one
