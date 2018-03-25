3. Consider the following differential equation:

    f(t) = (f''(t) + f'(t))/2, f(0) = a, f'(0) = b

3a. Solve the equation assuming that |f| can be expressed by a power
    series |fs|, that is, use |integ| and the differential equation to
    express the relation between |fs|, |fs'|, and |fs''|.

    What are the first three coefficients of |fs| (expressed in terms
    of |a| and |b|)?

3b. Solve the equation using the Laplace transform.  You should need
    this formula (and the rules for linearity + derivative):

    ℒ (\t -> e^(α*t)) s  = 1 / (s - α)

    Show that your solution does indeed satisfy the three requirements.

----
Short answers:
a) fs = integ fs'  a; fs'  = integ fs'' b; fs'' = 2*fs-fs'
   fs = a : b : a-b/2 : ...
b) f t = ((2*a+b)/3)*exp(t)+((a-b)/3)*exp(-2*t)

----------------
More details:

3a.
\begin{code}
module P3 where
import Data.Ratio
import PS
fs   = integ fs'  a
fs'  = integ fs'' b
fs'' = 2*fs-fs'
\end{code}
Computing the first few coefficients (four here, three in the exam question):

fs   = a     :  b       :  a-b/2   : -a/3+b/2 : ...
-- move b up, move 2*a-b up, divide by 2, etc
fs'  = b     :  2*a-b   : -a+3*b/2 : ...
-- move 2*a-b up,
fs'' = 2*a-b : -2*a+3*b : ...

----------------
3b.

  f(t) = (f''(t) + f'(t))/2, f(0) = a, f'(0) = b

  2*f - f'' - f' = 0

  LHS
=
  L (2*f - f'' - f') s
= -- linearity
  2*L f s - L f'' s - L f' s
= -- L f' s = -f 0 + s*L f s = -a + s*L f s
  2*L f s - L f'' s + a - s*L f s
= -- L f'' s = -f' 0 - s*f 0 + s^2*L f s = -b - a*s + s^2*L f s
  2*L f s + b + a*s - s^2*L f s + a - s*L f s
= -- simplify
  (2-s-s^2)*L f s  + a + b + a*s
-- Note that s=1 and s=-2 are zeros of (2-s-s^2)=-(s-1)*(s+2)

Thus
  L f s = (a+b+a*s)/(s-1)/(s+2)

Ansatz: L f s = A/(s-1) + B/(s+2) and multiply both sides by (s-1)*(s+2)

  a+b+a*s == A*(s+2) + B*(s-1)
<= the same but with s=1 and with s=-2
  a+b+a == A*(1+2)  &&   a+b-2*a == B*(-2-1)
<=> simplify
  2*a+b == 3*A  &&  b-a == -3*B
<=>
  A == (2*a+b)/3  &&  B == (a-b)/3

Inverse transform by inspection:

  f t = A*exp(t)+B*exp(-2*t)   -- with A and B as above

-- Checking:

  f'  t = A*exp(t)-2*B*exp(-2*t)
  f'' t = A*exp(t)+4*B*exp(-2*t)

  Original RHS
= -- def.
  (f''(t) + f'(t))/2
= -- Def. of f' t and f'' t, then simplification
  A*exp(t)+B*exp(-2*t)
= -- Ansatz
  f t
= -- def.n
  Original LHS

f  0 = A*1+B*1 = (2*a+b)/3 + (a-b)/3 = a  -- OK
f' 0 = A-2*B = (2*a+b)/3 - 2*(a-b)/3 = b  -- OK




----------------------------------------------------------------
Not needed for the exam question:

\begin{code}
a = V "a"
b = V "b"
data E = A E E | M E E | N E | C Integer | R E | V String
  deriving (Eq, Show)
instance Num E where
  (+) = A
  (*) = M
  negate = N
  fromInteger = C
instance Fractional E where
  recip = R
  fromRational = f
f :: Rational -> E
f r = fromInteger (numerator r) / fromInteger (denominator r)
\end{code}
