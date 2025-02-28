\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
module Live_6_2 where
import DSLsofMath.Algebra as Algebra
import qualified Prelude
import Prelude (Eq, Ord, Show, Int, Double, Rational, id, const, (.), error)
import DSLsofMath.Simplify
import DSLsofMath.PSDS -- Polynomials and Power Series
import DSLsofMath.FunExp
\end{code}

----------------
L6.2 live coding on solving ODEs

Solve   f'' + 2*f' + f = sin,  f 0 = 2, f' 0 = 1

Step 0: transform to power series with this specification:
     f = eval as; f' = eval as'; f'' = eval as''
-- introduce instead of f, f', f'' the series as, as', as''
     sin = eval sinP
Step 1: compute terms for the rhs
Step 2: solve for the highest derivative:
  f''  = sin  - f  -        2*f'
  as'' = sinP - as - scaleP 2 as'
Step 3: fill in "integ-equations" for as' and as
Step 4: If you do this by hand: fill in the coefficient lists step by step.
\begin{code}
sinP, cosP :: Field a => PS a
sinP = integP 0 cosP
cosP = integP 1 (-sinP)
-- P [2,1,-2,4 % 3,-1/2, 1/8, -1/40, 11 % 2520,(-13) % 20160,29 % 362880]

as, as', as'' :: Field a => PS a
as'' = sinP - as - scaleP 2 as'
as'  = integP 1 as''
as   = integP 2 as'
\end{code}

sinP =      :      :
cosP =      :      :
as'' =      :      :
as'  =      :      :
as   =      :      :

----------------
Step 5: Checking

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

Don't forget to check the original equation (often catches simple
 mistakes).

f' + f = 1

notRhs = 1:1:1:1.... -- not correct!
  eval notRhs x = 1+x+x^2+x^3...
rhs = 1:0:0:0:0: ...
