\begin{code}
{-# LANGUAGE  FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE  TypeSynonymInstances #-}
module A2_Skeleton where
import Prelude hiding (  (+), (-), (*), (/), negate, recip, (^),
                         pi, sin, cos, exp, fromInteger, fromRational)
import DSLsofMath.Algebra
import DSLsofMath.FunExp

type Tri a     = (a, a, a)
type TriFun a  = Tri (a->a)   -- = |(a->a, a->a, a->a)|
type FunTri a  = a -> Tri a   -- = |a -> (a, a, a)|

instance Additive a => Additive (Tri a) where
  (+)  = addTri;   zero  = zeroTri
instance (Additive a, Multiplicative a) => Multiplicative (Tri a) where
  (*)  = mulTri;   one   = oneTri

instance AddGroup a => AddGroup (Tri a) where
  negate  = negateTri
instance (AddGroup a, MulGroup a) => MulGroup (Tri a) where
  recip   = recipTri

(addTri, zeroTri, mulTri, oneTri, negateTri, recipTri) = undefined

instance Transcendental a  => Transcendental (Tri a)  where
  pi = piTri;   sin = sinTri;   cos = cosTri;   exp = expTri

(piTri, sinTri, cosTri, expTri) = undefined
\end{code}
