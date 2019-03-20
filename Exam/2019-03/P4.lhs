P4: 2f(x) + 3f'(x) + f''(x) = 2*e^{-3x},    f(0) = -2,   f'(0) = 0

4a) power series

\begin{code}
fs   = integ (-2) fs'
fs'  = integ 0    fs''
fs'' = rhs - 2*fs - 3*fs'
rhs  = integ 2 (-3*rhs)

integ c fs = c : zipWith (/) fs [1..]
\end{code}

Hand-computation steps:
1. first term of fs, fs', rhs from initial conditions => -2, 0, 2
2. 2nd term of fs from fs' via integ => 0
3. compute 1st term of fs'' from the 1st terms of the other three => 6
4. 2nd term of fs' from fs'' via integ => 6
5. 3rd term of fs from 2nd term of fs' via integ => 3 = 6/2
6. compute 2nd term of fs'' as before => -24
7. compute 3rd term of fs' as before => -12 = -24/2
8. compute 4th term of fs as before => -4 = -12/3

\begin{code}
tests = [ take 4 fs   ==  -2 : 0  : 3   : -4 :   []
        , take 3 fs'  ==   0 : 6  : -12 :        []
        , take 2 fs'' ==   6 : -24:              []
        , take 2 rhs  ==   2 : -6 :              []
        ]
\end{code}

Helper functions - not needed on the exam:
\begin{code}
fs, fs', fs'', rhs :: [Rational]

instance (Eq a, Num a) => Num [a] where
  (+) = addL
  (*) = mulL
  negate = map negate
  fromInteger i = fromInteger i : repeat 0

addL :: Num r => [r] -> [r] -> [r]
addL = zipWithLonger (+)

mulL :: Num r => [r] -> [r] -> [r]
mulL [] bs = []
mulL as [] = []
mulL (a:as) bs = addL (scaleL a bs) (shiftL (mulL as bs))

scaleL :: Num r => r -> [r] -> [r]
scaleL a = map (a*)

shiftL :: Num r => [r] -> [r]
shiftL = (0:)

zipWithLonger :: (a->a->a) -> [a] -> [a] -> [a]
zipWithLonger op [] bs = bs
zipWithLonger op as [] = as
zipWithLonger op (a:as) (b:bs) = op a b : zipWithLonger op as bs

\end{code}

----------------

4b) Laplace

  2f(x) + 3f'(x) + f''(x) = 2*e^{-3x}
  f(0) = -2,
  f'(0) = 0

First apply Laplace to both sides and start applying laws

  2*L f s + 3*L f' s + L f'' s = 2*L (\x->e^{-3x}) s
= -- L f' s = -f 0 + s*L f s = 2+s*L f s
  -- L f'' s = -f' 0 + s*L f' s = s*(2+s*L f s) = 2*s+s^2*L f s
  -- L (\x->e^{a*x}) s = 1/(s-a)
  2*L f s + 3*(2+s*L f s) + 2*s+s^2*L f s = 2/(s+3)
= -- collect terms
  (s^2+3*s+2)*L f s = 2/(s+3) - 6 - 2*s
= -- Divide to get L f s by itself
  L f s = (2/(s+3) - 6 - 2*s)/(s^2+3*s+2)

Now we factorise (s^2+3*s+2) = (s+1)(s+2) and try to find A, B, C such that

  (2/(s+3) - 6 - 2*s)/(s^2+3*s+2) = A/(s+1) + B/(s+2) + C/(s+3)
= -- multiply by (s+1)(s+2)(s+3)
  2 - 6*(s+3) - 2*s*(s+3) = A*(s+2)*(s+3) + B*(s+1)*(s+3) + C*(s+1)*(s+2)

To solve this, find three simple equations by setting s=-1, s=-2, s=-3:

s=-1 =>
    2-6*(2)-2*(-1)*(2) = A*(1)*(2)
  = -- simplify
    -6 = 2*A
  =
    A = -3

s=-2 =>
    2-6*(1)-2*(-2)*(1) = B*(-1)*(1)
  = -- simplify
    2-6+2*2 = -B
  =
    B = 0

s=-3 =>
    2 = C*(-2)*(-1)
  = -- simplify
    C = 1

Thus

  L f s = -3/(s+1) + 1/(s+3)

which we get from

  f x = -3 exp(-x) + exp(-3*x)

Testing:
  compute LHS = 2f(x) + 3f'(x) + f''(x) and RHS = 2*e^{-3x}

  f x   = -3*exp(-x) +   exp(-3*x)
  f' x  =  3*exp(-x) - 3*exp(-3*x)
  f'' x = -3*exp(-x) + 9*exp(-3*x)
  2*f x = -6*exp(-x) + 2*exp(-3*x)
  3*f' x=  9*exp(-x) - 9*exp(-3*x)

  2f(x) + 3f'(x) + f''(x)
         = 0*exp(-x) + 2*exp(-3*x) = RHS  -- OK

  f(0) = -3+1 = -2  -- OK
  f'(0) = 3-3 = 0   -- OK

Thus the solution is

  f x = -3 exp(-x) + exp(-3*x)

----------------
Optional: check 4a) against 4b)
  f''' x = 3*exp(-x) -27*exp(-3*x)
Thus the 0th to 3rd derivatives at zero are
  -2, 0, 6, -24
which we can match to a polynomial
  -2 : 0/1 : 6/(1*2) : -24/(1*2*3) : ...
=
  -2 : 0 : 3 : -4

which matches 4a).
