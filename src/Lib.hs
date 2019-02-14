module Lib where

import           Semiring

instance Semiring Bool where
  zero = False
  one = True
  (<+>) = (||)
  (<.>) = (&&)

data Reg
  = Eps
  | Sym Char
  | Alt Reg
        Reg
  | Seq Reg
        Reg
  | Rep Reg
  deriving (Show)

data RegW c s
  = EpsW
  | SymW (c -> s)
  | AltW (RegW c s)
         (RegW c s)
  | SeqW (RegW c s)
         (RegW c s)
  | RepW (RegW c s) --deriving (Show)

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

accept :: Reg -> String -> Bool
accept Eps u       = null u
accept (Sym c) u   = u == [c]
accept (Alt p q) u = accept p u || accept q u
accept (Seq p q) u = or [accept p u1 && accept q u2 | (u1, u2) <- split u]
accept (Rep r) u   = or [and [accept r u_i | u_i <- ps] | ps <- parts u]

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

-- Decomposes a list into pairs
split :: [a] -> [([a], [a])]
split []     = [([], [])]
split (c:cs) = ([], c : cs) : [(c : s1, s2) | (s1, s2) <- split cs]

-- Partitions a given list
parts :: [a] -> [[[a]]]
parts []     = [[]]
parts [c]    = [[[c]]]
parts (c:cs) = concat [[(c : p) : ps, [c] : p : ps] | p:ps <- parts cs]
