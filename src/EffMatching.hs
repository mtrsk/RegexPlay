module EffMatching where

import           Semiring

data Reg
  = Eps
  | Sym Bool
        Char
  | Alt Reg
        Reg
  | Seq Reg
        Reg
  | Rep Reg
  deriving (Show)

empty :: Reg -> Bool
empty Eps       = True
empty (Sym _ _) = False
empty (Alt p q) = empty p || empty q
empty (Seq p q) = empty p && empty q
empty (Rep r)   = True

final :: Reg -> Bool
final Eps       = False
final (Sym b _) = b
final (Alt p q) = final p || final q
final (Seq p q) = (final p && empty q) || final q
final (Rep r)   = final r

shift :: Bool -> Reg -> Char -> Reg
shift _ Eps _       = Eps
shift m (Sym _ x) c = Sym (m && x == c) x
shift m (Alt p q) c = Alt (shift m p c) (shift m q c)
shift m (Seq p q) c = Seq (shift m p c) (shift (m && empty p || final p) q c)
shift m (Rep r) c   = Rep (shift (m || final r) r c)

match :: Reg -> String -> Bool
match r []     = empty r
match r (c:cs) = final (foldl (shift False) (shift True r c) cs)
