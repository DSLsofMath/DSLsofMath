\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
module Live_6_2 where
import DSLsofMath.Algebra as Algebra
import qualified Prelude
import Prelude (Eq, Ord, Show, Int, Double, Rational, fst, snd, id, const, (.), error, map)
import DSLsofMath.PSDS (
  Poly(P), PS, evalPS, scaleP, derP, integP, takeP -- Polynomials and Power Series
  )
\end{code}

----------------
L6.2 live coding on solving ODEs

Solve   f'' + 2*f' + f = sin,  f 0 = 2, f' 0 = 1
  - note +, sin, f, etc. all work on functions (point-free version)
  -- 2*f means scale 2 f
  -- scale c f  =  \x -> c * (f x)
Step 0: transform to power series with this specification:
     f = eval as; f' = eval as'; f'' = eval as''
-- Note that the prime in as' is just part of the name, we have to supply the meaning.
-- introduce instead of f, f', f'' the series as, as', as''
     sin = eval sinP
Step 1: express the terms for the rhs (computed later)
Step 2: fill in "integ-equations" for as and as'
Step 3: solve for the highest derivative:
  f''  = sin  - f  -        2*f'
  as'' = sinP - as - scaleP 2 as'
Step 4: If you do this by hand: fill in the coefficient lists step by step.
\begin{code}
sinP, cosP :: Field a => PS a
sinP = integP 0 cosP
cosP = integP 1 (-sinP)

as, as', as'' :: Field a => PS a
as   = integP 2 as'
as'  = integP 1 as''
as'' = sinP - as - scaleP 2 as'
\end{code}

--             /1     /2     /3 ....
sinP =  0   :  1   :   0
cosP =  1   :  0   : -1/2
as   =  2   :  1   :  -2  : 4/3 : ...
as'  =  1   : -4   :   4  :
as'' =  a   :  b   :   c
where a = (sinP - as - scaleP 2 as')!!0
        =     0 -  2 -        2*1
        = -4
      b = (sinP - as - scaleP 2 as')!!1
        =     1 -  1 -        2*(-4)
        = 8
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

testEq n m x = lhs n x - rhs m x -- should be close to zero
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

xP :: Field a => PS a
xP = integP 0 one

val0 ::  Additive a => PS a  ->  a
val0 (P (a0:_))  = a0
val0 (P [])      = zero

piPS :: Transcendental a => PS a
piPS = integP pi 0

expPS, sinPS, cosPS :: (Eq a, Transcendental a) => PS a -> PS a
expPS  as  = g
  where  g   = integP v0 (g  *  derP as)
         v0  = exp (val0 as)

sinPS  as  = fst (sincosPS as)
cosPS  as  = snd (sincosPS as)

sincosPS :: Transcendental a => PS a -> (PS a, PS a)
sincosPS as = (s, c)
  where  as'  = derP as
         a0   = val0 as
         s0   = sin a0
         c0   = cos a0
         s    = integP s0 ( c  *  as')
         c    = integP c0 (-s  *  as')
\end{code}

Specification:
+ Semantic side:
  exp :: a -> a, where a is (or represents) a function type
  type R = Double
  expReal :: R -> R
  expFun  :: (R->R) -> (R->R)
  expPS   ::   PS R -> PS R
  -- but not exp :: PS R directly

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
