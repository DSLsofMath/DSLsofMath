\begin{code}
-- May need some of these
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Live_7_1_2021 where
import DSLsofMath.Algebra
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..),sum)
import Data.Maybe(fromJust)
type REAL = Double
\end{code}

* New algebraic structure: vector space.
  Two sets involved:    s for scalars, v for vectors and
  Two main operations:  (+) and (*^) = scale

\begin{code}
class (Field s, AddGroup v) => VectorSpace v s where
  (*^) :: s -> v -> v

scale :: VectorSpace v s => s -> v -> v
scale = (*^)

instance Field s => VectorSpace s s where (*^) = (*)

type OneD = REAL

vR1, vR2 :: OneD
vR1 = 1 + 3
vR2 = scale half vR1

half :: REAL
half = 0.5
\end{code}

Note that |(*)    :: a -> a -> a|  -- symmetric type
and       |scale  :: s -> v -> v|  -- asymmetric type
but in this instance |s=v=a=REAL|.

\begin{code}
instance VectorSpace v s => VectorSpace (g->v) s where (*^) = scaleF

scaleF :: VectorSpace v s => s -> (g->v) -> (g->v)
scaleF s f = \i -> scale s (f i)
\end{code}
The functions instances for |zero|, |(+)|, |negate| are as before.

Example of 2D-vectors
\begin{code}
data Two = X | Y            -- index type
  deriving (Eq, Enum, Bounded, Show)

type TwoD s = Two -> s      -- vectors in the X-Y-plane

vT1, vT2, vT3, vT4 :: TwoD REAL
vT1 X = 4; vT1 Y = 1
vT2 X = 1; vT2 Y = 3
vT3 = vT1 + vT2
vT4 = scale half vT3
\end{code}


\begin{code}

\end{code}

\begin{code}
\end{code}
\begin{code}
\end{code}
\begin{code}
type Finite g = (Bounded g, Enum g, Eq g)
finiteDomain :: Finite a => [a]
finiteDomain = [minBound..maxBound]

toF :: Eq k => [(k,v)] -> (k->v)
toF tab key = fromJust (lookup key tab)

instance (Finite g, Show g, Show s) => Show (g->s) where  show = showFun

showFun :: (Finite a, Show a, Show b) => (a->b) -> String
showFun f = "toF "++show (map (\i->(i, f i)) finiteDomain)
\end{code}


toL :: Finite g => Vector s g -> [s]
toL (V v) = map v finiteDomain
instance (Finite g, Show s) => Show (Vector s g)  where  show = showVector

showVector :: (Finite g, Show s) => Vector s g -> String
showVector (V v) = showFun v
