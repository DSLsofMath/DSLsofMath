\begin{code}
{-# LANGUAGE RebindableSyntax #-}
module P2_Laplace where
import qualified Prelude
import Prelude (Rational, (==), take, error, Double)
import DSLsofMath.Algebra
import DSLsofMath.PSDS
\end{code}

(a) Power Series transform

\begin{code}
as, as', as'', rhs :: Field a => PS a
as    = integP (1/2) as'
as'   = integP (-1/2) as''
as''  = scaleP (1/2) (rhs - scaleP 5 as' - scaleP 2 as)
rhs   = integP 1 (scaleP (-1) rhs)
\end{code}

\begin{code}
bs, bs', bs'', rHs :: Field a => [a]
bs   = ( 1/2) : (-1/2) :    5/8   : (-25/48) : error "TODO"
bs'  = (-1/2) :   5/4  : (-25/16) : error "TODO"
bs'' = ( 5/4) : (-25/8): error "TODO"
  --bs''!!0 = (1/2)* ( 1 -5*(-1/2) - 2*(1/2))  =   5/4
  --bs''!!1 = (1/2)* (-1 -5*( 5/4) - 2*(-1/2)) = -25/8

rHs  = 1 : (-1) : (1/2) : (-1/6) : error "TODO"
\end{code}

(b) Laplace-transform

2*f'' t + 5*f' t + 2*f t = exp(-t), f 0 = 1/2, f' 0 = -1/2

-- Apply the Laplace transform and simplify:

2*L f'' s + 5*L f' s + 2*L f s = 1/(s+1)

-- Compute Laplace-D-laws for our initital conditions:

  L f'  s = -f 0 + s*L f s = -1/2 + s*F s
  L f'' s = -f' 0 + s*L f' s = 1/2 + s*(-1/2 + s*F s) = 1/2*(1-s+2s^2*F s)

-- Use Laplace-D-laws on the transformed equation
(1-s+2s^2*F s) + 5*(-1/2 + s*F s) + 2*F s = 1/(s+1)
-- collect terms
2s^2*F s + 5*s*F s + 2*F s = 1/(s+1) - 1 + s + 5/2
-- simplify and multiply by (s+1)
(s+1)*(2*s^2 + 5*s + 2)*F s = 1 + (s + 3/2)*(s+1)
-- Factor the quadratic term: (2*s^2 + 5*s + 2) = 2*(s+2)*(s+1/2)
2*(s+1)*(s+2)*(s+1/2)*F s = 1 + (s + 3/2)*(s+1)
-- Ansatz: F s = A/(s+1) + B/(s+2) + C/(s+1/2)
2*(s+2)*(s+1/2)*A + 2*(s+1)*(s+1/2)*B + 2*(s+1)*(s+2)*C = 1 + (s + 3/2)*(s+1)
-- This polynomial eq. should hold for all s, instantiate for s=-1, s=-2, s=-1/2:
2*(-1+2)*(-1+1/2)*A = 1
2*(-2+1)*(-2+1/2)*B = 1 + (-2 + 3/2)*(-2+1)
2*(-1/2+1)*(-1/2+2)*C = 1 + (-1/2 + 3/2)*(-1/2+1)
-- simplify
-A = 1
-2*(-3/2)*B = 1 + (-1/2)*(-1)
2*(1/2)*(3/2)*C = 1 + (1)*(1/2)
-- simplify
A = -1
3*B = 3/2
(3/2)*C = 3/2
-- simplify
A = -1; B = 1/2; C = 1
-- Thus we have
F s = -1/(s+1) + (1/2)/(s+2) + 1/(s+1/2)
-- and by "pattern-matching" we can transform back
f t   = -exp(-t) + (1/2)*exp(-2*t) + exp(-t/2)
-- Now compute f' and f'' to check the conditions:
f'  t =  exp(-t) -   exp(-2*t) - (1/2)*exp(-t/2)
f'' t = -exp(-t) + 2*exp(-2*t) + (1/4)*exp(-t/2)
-- Check f 0 = 1/2
f 0   = -1 + (1/2) + 1 = 1/2 -- OK!
-- Check f' 0 = -1/2
f'  t = 1 - 1 - (1/2) = -1/2 -- OK!
-- Finally check that LHS = RHS
LHS = 2*f'' t + 5*f' t + 2*f t
= -- expand
  2*(-exp(-t) +     2*exp(-2*t) + (1/4)*exp(-t/2))
  5*( exp(-t) -       exp(-2*t) - (1/2)*exp(-t/2))
  2*(-exp(-t) + (1/2)*exp(-2*t) +       exp(-t/2))
= -- distribute
  -2*exp(-t) +     4*exp(-2*t) + (1/2)*exp(-t/2)
   5*exp(-t) -     5*exp(-2*t) - (5/2)*exp(-t/2)
  -2*exp(-t) +     1*exp(-2*t) +     2*exp(-t/2)
= -- collect terms
  (-2+5-2)*exp(-t) + (4-5+1)*exp(-2*t) + ((1/2)-(5/2)+2)*exp(-t/2)
= -- arithmetics
  exp(-t)
= RHS -- OK!
