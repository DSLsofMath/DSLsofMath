%if False
\begin{code}
{-# LANGUAGE RebindableSyntax #-}
module Solution_202603_Laplace where
import Prelude (Bool, Int, Rational, Eq((==)), (!!), otherwise, error, Ord((<), (>=)), length)
import DSLsofMath.Algebra
import DSLsofMath.PSDS
(!) :: Additive a => Poly a -> Int -> a
P as ! i | i >= length as = zero
         | i < 0  = error "(!) out of bound (negative)"
         | otherwise = as !! i
\end{code}
%endif
\paragraph{Solution}
% f''(t) + 5f'(t) + 6f(t) = 12e^{-t},\quad f(0) = 0,\quad  f'(0) = 0
\begin{enumerate}
\item Power series version:
%
let |fs, fs', fs'', expm1 :: PS Rational| be such that
\begin{spec}
  map eval [fs, fs', fs'', expm1] == [f, f', f'', exp.((-1)*)]
\end{spec}

Then we have
\begin{spec}
  fs'' + scaleP 5 fs' + scaleP 6 fs = scaleP 12 expm1
\end{spec}
which can be rearranged to
\begin{code}
fs, fs', fs'', expm1 :: Field a => PS a
fs''   = scaleP 12 expm1 - scaleP 5 fs' - scaleP 6 fs
\end{code}

We also have the integrations and the base exponential series:
\begin{code}
fs     = integP 0  fs'
fs'    = integP 0  fs''
expm1  = integP 1 (scaleP (-1) expm1)
\end{code}

Now we can start filling in the coefficients:
\begin{enumerate}
\item Step 0:
\begin{spec}
  fs     !0 =  0
  fs'    !0 =  0
  expm1  !0 =  1
  fs''   !0 = 12*1 - 5*0 - 6*0 = 12
\end{spec}

\item Step 1:
\begin{spec}
  fs     !1 =  0/1 = 0
  fs'    !1 =  12/1 = 12
  expm1  !1 = -1/1 = -1
  fs''   !1 = 12*(-1) - 5*(12) - 6*(0) = -12 - 60 = -72
\end{spec}

\item Step 2:
\begin{spec}
  fs     !2 =  12/2 = 6
  fs'    !2 = -72/2 = -36
\end{spec}

\item Step 3:
\begin{spec}
  fs     !3 = -36/3 = -12
\end{spec}
\end{enumerate}

Thus
\begin{code}
test :: Bool
test = takeP 4 fs == P [0, 0, 6, -12 :: Rational]
\end{code}
\newpage
\item Analytic solution with Laplace transform

\begin{enumerate}
  \item Step 0: Let |F = L f| below:

\begin{spec}
  LHS t = f''(t) + 5f'(t) + 6f(t)
  RHS t = 12*exp(-t)
\end{spec}

and |f 0  = 0|,  |f' 0 = 0|.

\item Step 1: Use the Laplace-D-law to compute |L f'|

\begin{spec}
  L f' s = L (D f) s = -0 + s*L f s = s*F s
\end{spec}

\item Step 2: Use the Laplace-D-law to compute |L f''|

\begin{spec}
  L f'' s         = -- Def. of |D|
  L (D f') s      = -- |f' 0 = 0|
  -0 + s*L f' s   = -- comp. above
  s*(s*F s)       = -- simplify
  s^2*F s
\end{spec}

\item Then we apply L to LHS:

\begin{spec}
  L (\t -> f''(t) + 5*f'(t) + 6*f(t)) s = -- Linearity, def. of |F|
  L f'' s + 5*L f' s + 6*L f s          = -- L-D-law results from above
  s^2*F s + 5*s*F s + 6*F s             = -- Simplify
  (s^2 + 5*s + 6)*F s                   = -- Factor
  (s+2)*(s+3)*F s
\end{spec}

\item and we apply L to the RHS

\begin{spec}
  L (\t -> 12*exp(-t)) s
= -- Linearity and Laplace law for exponentials
  12/(s+1)
\end{spec}

By combining LHS=RHS and dividing both sides by |(s+2)*(s+3)| we get:
\begin{spec}
  F s = 12/((s+1)*(s+2)*(s+3))
\end{spec}

\item and can start with partial fraction decomposition. Ansatz:

\begin{spec}
  F s = A/(s+1) + B/(s+2) + C/(s+3)
\end{spec}

\item Multiply by |(s+1)*(s+2)*(s+3)| to get a polynomial equation:

\begin{spec}
  12 = A*(s+2)*(s+3) + B*(s+1)*(s+3) + C*(s+1)*(s+2)
\end{spec}

\item Solve the polynomial equation by specialising to three cases (s=-1,-2,-3):

\begin{spec}
s=-1:  12 = A*(1)*(2)    <=>  12 = 2A   <=>  A = 6
s=-2:  12 = B*(-1)*(1)   <=>  12 = -B   <=>  B = -12
s=-3:  12 = C*(-2)*(-1)  <=>  12 = 2C   <=>  C = 6 
\end{spec}

\item Thus
\begin{spec}
  F s   = 6/(s+1) - 12/(s+2) + 6/(s+3)
\end{spec}
\item which we can recognize as the transform of
\begin{spec}
  f t    =  6*exp(-t) - 12*exp(-2*t) + 6*exp(-3*t)
  f t    =  6*(e1 - 2*e2 + e3)
\end{spec}
To simplify the checking expressions I write:
  e1 = exp(-t)
  e2 = exp(-2*t)
  e3 = exp(-3*t)

\item Checking: 
First, compute the derivatives symbolically:
\begin{spec}
  f' t   = 6*(-e1  + 4*e2 - 3*e3)
  f'' t  =  6*(e1  - 8*e2 + 9*e3)
\end{spec}
Then, check initial conditions, and the main equation.
  
\begin{spec}
f   0 = 6*( 1 - 2 + 1) = 0 -- OK!
f'  0 = 6*(-1 + 4 - 3) = 0 -- OK!
LHS t  = 6*(    ( e1 - 8*e2 + 9*e3)+
              5*(-e1 + 4*e2 - 3*e3)+
              6*( e1 - 2*e2 +   e3)  )
       = 6*((1-5+6)*e1 + (-8+20-12)*e2 + (9-15+6)*e3)
       = 12*e1 + 0*e2 + 0*e3
       = RHS t    -- OK!
\end{spec}

\end{enumerate}
\end{enumerate}
