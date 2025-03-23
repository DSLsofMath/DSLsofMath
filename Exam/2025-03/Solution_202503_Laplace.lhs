%if False
\begin{code}
{-# LANGUAGE RebindableSyntax #-}
module Solution_202503_Laplace where
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
% f'' + 20f' + 99f = -99,\quad f(0) = 0,\quad  f'(0) = -1
\begin{enumerate}
\item Power series version:
%
let |fs, fs', fs'', rhs :: PS Rational| be such that
\begin{spec}
  map eval [fs, fs', fs'', rhs] == [f, f', f'', const (-99)]
\end{spec}

Then we have
\begin{spec}
  fs'' + scaleP 20 fs' + scaleP 99 fs = rhs
\end{spec}
which can be rearranged to
\begin{code}
fs, fs', fs'', rhs :: Field a => PS a
fs''   = rhs - scaleP 20 fs' - scaleP 99 fs
\end{code}

We also have
\begin{code}
fs     = integP 0  fs'
fs'    = integP (-1)  fs''
rhs    = integP (-99)  0
\end{code}

Now we can start filling in the coefficients
\begin{enumerate}
\item Step 0:
\begin{spec}
  fs     !0 =  0
  fs'    !0 = -1
  rhs    !0 = -99
  fs''   !0 = -99 - 20*(-1) - 99*0 = -79
\end{spec}

\item Step 1:
\begin{spec}
  fs     !1 = -1/1 = -1
  fs'    !1 = -79/1 = -79
\end{spec}

\item Step 2:
\begin{spec}
  fs     !2 = -79/2 = -39.5
\end{spec}
\end{enumerate}

Thus
\begin{code}
test :: Bool
test = takeP 3 fs == P [0,-1,-39.5::Rational]
\end{code}
\newpage
\item Analytic solution with Laplace transform

\begin{enumerate}
  \item Step 0: Let |F = L f| below:

\begin{spec}
  LHS = f'' + 20f' + 99f
  RHS t = -99
\end{spec}

and |f 0  = 0|,  |f' 0 = -1|.

\item Step 1: Use the Laplace-D-law to compute |L f'|

\begin{spec}
  L f' s = L (D f) s = -0 + s*L f s = s*F s
\end{spec}

\item Step 2: Use the Laplace-D-law to compute |L f''|

\begin{spec}
  L f'' s         = -- Def. of |D|
  L (D f') s      = -- |f' 0 = -1|
  1 + s*L f' s    = -- comp. above
  1 + s*(s*F s)   = -- simplify
  1 + s^2*F s     
\end{spec}

\item Then we apply L to LHS:

\begin{spec}
  L (f'' + 20*f' + 99*f) s               = -- Linearity, def. of |F|
  L f'' s + 20*L f' s + 99*L f s         = -- L-D-law results from above
  1 + s^2*F s  +  20*s*F s  +  99*F s    = -- Simplify
  (s^2+20*s+99)*F s + 1                  = -- Factor
  (s+9)*(s+11)*F s + 1
\end{spec}

\item and we apply to the RHS

\begin{spec}
  L (const (-99)) s
= -- exp 0 = 1
  L (\t -> -99*exp(0*t)) s
= -- Laplace law for exponentials
  -99/(s+0)
= -- simplify
  -99/s
\end{spec}

By combining LHS=RHS and moving |1| to the right we get:
\begin{spec}
  (s+9)*(s+11)*F s = -99/s - 1 
<=> -- divide both sides by (s+9)*(s+11)
  F s = -99/(s*(s+9)*(s+11)) - 1/((s+9)*(s+11))
\end{spec}
\item and can start with partial fraction decomposition: Ansatz:

\begin{spec}
  F s = A/s + B/(s+9) + C/(s+11)
\end{spec}

\item Multiply by |s*(s+9)*(s*11)| to get a polynomial equation:

\begin{spec}
  s*(s+9)*(s+11)*F s = -99 - s
\end{spec}

\item Substitute the ansatz and simplify:

\begin{spec}
  A*(s+9)*(s+11) + B*s*(s+11) + C*s*(s+9) = -99 - s
\end{spec}

\item Solve the polynomial equation by specialising to three cases (s=0,-9,-11):

\begin{spec}
s=0:   A  *( 0+9)  *  (0+11)  = -99 - 0      <=>  99A  = -99 <=>  A = -1
s=-9:  B  *   -9   * (-9+11)  = -99 - (-9)   <=> -18B  = -90 <=>  B =  5
s=-11: C  *  -11   * (-11+9)  = -99 - (-11)  <=>  22C  = -88 <=>  C = -4 
\end{spec}

\item Thus
\begin{spec}
  F s   = -1/s + 5/(s+9) - 4/(s+11)
\end{spec}
\item which we can recognize as the transform of
\begin{spec}
  f t    =  -1*exp(0*t)  +  5*exp(-9*t)  -     4*exp(-11*t)
  f t    =  -1 +      5*exp(-9*t)  -       4*exp(-11*t)
  f t    =  -1 + e9 + e11
\end{spec}
To simplify the expressions I write:
  e9 =   5*exp(-9*t)
  e11 = -4*exp(-11*t)

\item Checking: \textbf{Important!}
Fist, compute the derivatives symbolically
\begin{spec}
  f' t   =       (-9)*e9  + (-11)*e11
  f'' t  =        +81*e9  +   121*e11
\end{spec}
Then, check initial conditions, and the main equation.
  
\begin{spec}
f   0 = -1+5-4 = 0 -- OK!
f'  0 = -45+44= -1 -- OK!
LHS t  =    (      81*e9  +   121*e11)+
         20*(    (-9)*e9  + (-11)*e11)+
         99*(-1 +     e9  +       e11)
       = -99 +(81-180+99)*e9 + (121-220+99)*e11
       = -99 + 0*e9 + 0*e11
       = RHS t    -- OK!
\end{spec}

\end{enumerate}
\end{enumerate}
