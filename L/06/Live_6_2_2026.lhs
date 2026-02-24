\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
module Live_6_2 where
import DSLsofMath.Algebra as Algebra
import qualified Prelude
import Prelude (Eq, Ord, Show, Int, Double, Rational, id, const, (.), error, map)
import DSLsofMath.PSDS (
  Poly(P), PS, evalPS, scaleP, derP, integP, takeP -- Polynomials and Power Series
  )
\end{code}

----------------
L6.2 live coding on solving ODEs

Solve   f'' + 2*f' + f = sin,  f 0 = 2, f' 0 = 1

Step 0: transform to power series with this specification:
     f = eval as; f' = eval as'; f'' = eval as''
-- introduce instead of f, f', f'' the series as, as', as''
     sin = eval sinP
Step 1: compute terms for the rhs
Step 2: fill in "integ-equations" for as and as'
Step 3: solve for the highest derivative:
  f''  = sin  - f  -        2*f'
  as'' = sinP - as - scaleP 2 as'
Step 4: If you do this by hand: fill in the coefficient lists step by step.
\begin{code}
sinP, cosP :: Field a => PS a
sinP = error "TODO"
cosP = error "TODO"

as, as', as'' :: Field a => PS a
as   = error "TODO"
as'  = error "TODO"
as'' = error "TODO"
\end{code}

sinP =      :      :
cosP =      :      :
as   =      :      :
as'  =      :      :
as'' =      :      :

----------------
Step 5: Checking

Don't forget to check the original equation (often catches simple mistakes).

\begin{code}
lhs :: Field a => Int -> (a -> a)

lhs n =  f'' + 2*f' + f
  where  f   = evalPS n as
         f'  = evalPS n as'
         f'' = evalPS n as''

rhs :: Transcendental a => Int -> a -> a
rhs n =  evalPS n sinP

testEq n x = lhs n x - rhs n x -- should be close to zero
\end{code}


























-------------- Later -------------------

Implement pi, exp, sin, cos as *functions on power series*.

\begin{code}
type PowerSeries = PS

instance (Eq a, Transcendental a) => Transcendental (PowerSeries a) where
   pi   =  piPS
   exp  =  expPS
   sin  =  sinPS
   cos  =  cosPS

val0 ::  Additive a => PS a  ->  a
val0 = error "val0"

piPS :: Transcendental a => PS a
piPS = error "TODO piPS"

expPS, sinPS, cosPS :: (Eq a, Transcendental a) => PS a -> PS a
expPS  as  = error "expPS"
sinPS  as  = error "sinPS"
cosPS  as  = error "cosPS"
\end{code}

Specification:
+ Semantic side:
  exp :: a -> a, where a is (or represents) a function type
  exp :: (R->R) -> (R->R)

    (expFun f) x = expReal (f x)
  thus
    expFun f = expReal . f
+ Syntactic side:
  expPS :: PS a -> PS a
    H1(eval, expPS, expFun)
  === {def. of H1}
    Forall as. eval (expPS as) == expFun (eval as)
  === {def. of expFun}
    Forall as. eval (expPS as) == expReal . (eval as)

To implement expPS using integP we need to know its derivative and its
value at 0. We calculate:

  eval (derP (expPS as))
=== {H1(eval, derP, D)}
  D (eval (expPS as))
=== {H1(eval, expPS, expFun)}
  D (expReal . (eval as))
=== {Chain rule: D (f . g) = (f . g) * D g}
  (expReal . (eval as)) * D (eval as)
=== {H1(eval, expPS, expFun) and H1(eval, derP, D) "backwards"}
  (eval (expPS as)) * eval (derP as)
=== {H2(eval,(*),(*))
  eval (expPS as  *  derP as)

Thus, we conclude
  derP (expPS as) == expPS as  *  derP as

Then we need the value at zero:
  eval (expPS as) 0
=== {H1(eval, expPS, expFun)}
  (expReal . (eval as)) 0
=== {Def. of (.)}
  expReal (eval as 0)
=== {Def. new helper function val0 which is almost "head".}
  expReal (val0 as)




