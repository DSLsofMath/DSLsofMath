\begin{code}
{-# LANGUAGE RebindableSyntax #-}
module P4_Laplace where
import qualified Prelude
import Prelude (Rational, (==), take, error, Double)
import DSLsofMath.Algebra
import DSLsofMath.PSDS
import Data.Complex
\end{code}

Solve: 
  f''/10 + f'/5 + f = 0, f 0 = 2,  f' 0 = -2
simplified to
  f'' + 2*f' + 10*f = 0, f 0 = 2, f' 0 = -2

\begin{code}
as, as', as'' :: Field a => PS a
as'' = scaleP (-2) as' + scaleP (-10) as
as'  = integP (-2)  as''
as   = integP   2   as'
\end{code}
Hand-computing the first few terms
\begin{code}
bs, bs', bs'' :: Field a => [a]
bs'' = -- (-2)*(-2) + (-10)*2 == 4 - 20 == -16
       (-16) :  -- (-2)*(-16) + (-10)*(-2) == 32+20 == 52
              52 : error "TODO"
bs'  = (-2) :(-16): 26 : error "TODO"
bs   =   2  :(-2):(-8): 26/3 : error "TODO"

-- Just checking that the Haskell-computed and hand-computed
-- coefficients match.
check = takeP 4 as == P (take 4 bs :: [Rational])
\end{code}


----------------
Laplace transform version

Step 1: Use the Laplace-D-law to compute L f' and L f''

Laplace-D-law:  L (D f) s = -f 0 + s*L f s

Here we have f 0 = 1 =>
  L f' s = L (D f) = -1 + s*L f s
Similarly, for the second derivative we get
  L f'' s
= L (D f') s
= -f' 0 + s*L f' s
=  2    + s*(-2 + s*L f s)
=  2    - 2*s + s^2*L f s

Then we apply L to the LHS of the main equation:

  L (f'' + 2*f' + 10*f) s
= -- Linearity
  L f'' s + 2*L f' s + 10*L f s
= -- L-D-law results from above
 (2    - 2*s + s^2*L f s) + 2*(-2 + s*L f s) + 10*L f s
= -- Let F = L f and simplify
 2 - 2*s + s^2*F s - 4 + 2*s*F s + 10*F s
= -- collect terms
 -2 - 2*s + (s^2 + 2*s + 10)*F s

As this should equal zero we get

F s = 2*(s+1)/(s^2 + 2*s + 10)

Now, to do partial fraction decomposition we need the roots

  s1,2 = -1 +- sqrt (1-10) = -1 +- 3*sqrt (-1) = -1 +- 3*i

Thus  s^2 + 2*s + 10 = (s-s1)*(s-s2)
  {- check: s1*s2 = (-1)²-(3*i)² = 1+9 = 10 -- OK
           -s1-s2 = 2 -- OK
  Also useful later: s1-s2 = (-1+3*i)-(-1-3*i) = 6*i
  -}

Ansatz: F s = 2*(s+1)/((s-s1)*(s-s2)) = A/(s-s1) + B/(s-s2)
Multiply both sides with (s-s1)*(s-s2)
  2*(s+1) = A*(s-s2) + B*(s-s1)
This should hold for all s - let's specialise to s=s1 and s=s2
First s=s1:
    2*(s1+1) = A*(s1-s2) + B*(s1-s1) = A*6*i
  simplify:
    6*i = A*6*i
  => A = 1
Then s=s2
    2*(s2+1) = A*(s2-s2) + B*(s2-s1) = -B*(s1-s2) = -B*6*i
  simplify:
    -6*i = -B*6*i
  => B = 1

Thus F s = 1/(s-s1) + 1/(s-s2)

Now we can use the Laplace-exponential-law "backwards" to get

  f t = exp (s1*t) + exp (s2*t)

Time to check that this really is a solution:

  f   t =      exp (s1*t) +      exp (s2*t)
  f'  t = s1  *exp (s1*t) +   s2*exp (s2*t)
  f'' t = s1^2*exp (s1*t) + s2^2*exp (s2*t)

f  0 = (1+1) = 2 -- OK
f' 0 = (s1*1+s2*1) = -2 -- OK

lhs = f'' + 2*f' + 10*f
 =      (s1^2*exp (s1*t) + s2^2*exp (s2*t))
    + 2*(s1  *exp (s1*t) +   s2*exp (s2*t))
    +10*(     exp (s1*t) +      exp (s2*t))

 =  (s1^2+2*s1+10)*exp (s1*t) + (s2^2+2*s2+10)*exp (s2*t)
 -- both s1 and s2 are solutions to s^2+2*s+10=0
 =              0 *exp (s1*t) +             0 *exp (s2*t)
 =  0
 = rhs

While the form of the expression was convenient for checking that the
equation was satisfied, another form is perhaps more readable for
humans:

f t = exp (s1*t) + exp (s2*t)
    = exp ((-1 +3*i)*t) + exp ((-1 - 3*i)*t)
    = exp (-t  + 3*i*t) + exp ( -t -  3*i*t)
    = exp (-t)*exp(3*i*t) + exp (-t)*exp(-3*i*t)
    = exp (-t)*(exp(3*i*t) + exp(-3*i*t))
    = exp(-t)*2*cos(3*t)

f is thus an oscillation with exponentially decaying amplitude.

Just some checking code
\begin{code}
f   t =      exp (s1*t) +      exp (s2*t)
f'  t = s1  *exp (s1*t) +   s2*exp (s2*t)
f'' t = s1^2*exp (s1*t) + s2^2*exp (s2*t)

s1, s2 :: Complex Double
s1 = (-1) + 3*i
s2 = (-1) - 3*i

i :: Complex Double
i = 0:+1

lhs :: Complex Double -> Complex Double
lhs t = f'' t + 2*f' t + 10*f t

fAlt :: Transcendental a => a -> a
fAlt t = 2*exp(-t)*cos(3*t)
\end{code}
