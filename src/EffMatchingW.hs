module EffMatchingW where

import           Semiring

data RegW c s = RegW
  { emptyW :: s
  , finalW :: s
  , regW   :: ReW c s
  }

data ReW c s
  = EpsW
  | SymW (c -> s)
  | AltW (RegW c s)
         (RegW c s)
  | SeqW (RegW c s)
         (RegW c s)
  | RepW (RegW c s)

epsW :: Semiring s => RegW c s
epsW = RegW {emptyW = one, finalW = zero, regW = EpsW}

symW :: Semiring s => (c -> s) -> RegW c s
symW f = RegW {emptyW = zero, finalW = zero, regW = SymW f}

altW :: Semiring s => RegW c s -> RegW c s -> RegW c s
altW p q =
  RegW
    { emptyW = emptyW p <+> emptyW q
    , finalW = finalW p <+> finalW q
    , regW = AltW p q
    }

seqW :: Semiring s => RegW c s -> RegW c s -> RegW c s
seqW p q =
  RegW
    { emptyW = emptyW p <.> emptyW q
    , finalW = finalW p <.> emptyW q <+> finalW q
    , regW = SeqW p q
    }

repW :: Semiring s => RegW c s -> RegW c s
repW r = RegW {emptyW = one, finalW = finalW r, regW = RepW r}

shiftW :: Semiring s => s -> ReW c s -> c -> RegW c s
shiftW _ EpsW _ = epsW
shiftW m (SymW f) c = (symW f) {finalW = m <.> f c}
shiftW m (AltW p q) c = altW (shiftW m (regW p) c) (shiftW m (regW q) c)
shiftW m (SeqW p q) c =
  seqW (shiftW m (regW p) c) (shiftW (m <.> emptyW p <+> finalW p) (regW q) c)
shiftW m (RepW r) c = repW (shiftW (m <+> finalW r) (regW r) c)

matchW :: Semiring s => RegW c s -> [c] -> s
matchW r [] = emptyW r
matchW r (c:cs) = finalW (foldl (shiftW zero . regW) (shiftW one (regW r) c) cs)
