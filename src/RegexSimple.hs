module RegexSimple where

import           Utils

data Reg
  = Eps
  | Sym Char
  | Alt Reg
        Reg
  | Seq Reg
        Reg
  | Rep Reg

accept :: Reg -> String -> Bool
accept Eps u       = null u
accept (Sym c) u   = u == [c]
accept (Alt p q) u = accept p u || accept q u
accept (Seq p q) u = or [accept p u1 && accept q u2 | (u1, u2) <- split u]
accept (Rep r) u   = or [and [accept r u_i | u_i <- ps] | ps <- parts u]
