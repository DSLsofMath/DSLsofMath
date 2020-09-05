P4: Laplace f''(t) + \sqrt{3}*f'(t) = 6*(f(t)-1),   f 0 = 4,    f' 0 = 0

a) [10p]

Let
\begin{code}
r = sqrt 3
six = 6 : repeat 0
\end{code}

Transform the diff. equation to power series:
  fs'' + r*fs' = 6*(fs-one) = 6*fs - six


Simplify
  fs'' = 6*fs - r*fs' - six
Add the "integ is inverse of deriv" instances for fs and fs'

\begin{code}
fs   = integ 4 fs'
fs'  = integ 0 fs''
fs'' = scale 6 fs + scale (-r) fs' + scale (-1) six
\end{code}

Start computing step by step

  fs   = 4:_
  fs'  = 0:_
  fs'' = (6*4-r*0-6):_

Simplify and move to the next step:

  fs   =  4: 0:_
  fs'  =  0:18:_
  fs'' = 18:(6*0-r*18-0):_

Simplify and move to the next step:

  fs   =  4:     0   : 9    :_
  fs'  =  0:    18   :(-9*r):_
  fs'' = 18:(-18*r)  :(6*9-r*(-9*r)-0):_

... and so on (more than needed for the exam question):

  fs   =  4:     0   :    9 :(-3*r): 27/4 : _
  fs'  =  0:    18   :(-9*r):  27  : _
  fs'' = 18:(-18*r)  :   81 : _

Thus: the first four coefficients of fs are [4, 0, 9, -3*sqrt 3]

----------------

b) [15p] Solve using the Laplace transform

-- Transform both side. Note that L (const 1) s = 1/s [common mistake to write = 1]
  L f'' s + r*(L f' s) = 6*(L f s - 1/s)
-- Use the derivative law (twice)
  -f' 0 + s*L f' s + r*(-f 0 + s*L f s) = 6*(L f s - 1/s)
-- f' 0 = 0, derivative law again, f 0 = 4
  s*(-4 + s*L f s) + r*(-4 + s*L f s) = 6*(L f s - 1/s)
-- distribute (*) over (+)
  -4*s + s^2*L f s - 4*r + r*s*L f s = 6*L f s - 6/s
-- Collect terms: L f s on the LHS, the rest on the RHS
  (s^2+r*s-6)*L f s =  4*s - 6/s + 4*r
-- Factor the quadratic in s: roots are s1=r, s2=-2*r. Multiply by s
  s*(s-r)*(s+2*r)*L f s =  4*s^2 - 6 + 4*r*s = 4*s*(s+r) - 6
-- Partial fraction decomposition: L f s = A/s + B/(s-r) + C/(s+2r)
  s*(s-r)*(s+2*r)*(A/s + B/(s-r) + C/(s+2r)) = 4*s*(s+r) - 6
-- Distribute (*) over (+)
  (s-r)*(s+2*r)*A + s*(s+2*r)*B + s*(s-r)*C = 4*s*(s+r) - 6
-- Specialise to three values of s to get three equations for A, B, C.
  s=0:   (-r)*(2*r)*A = -6
  s=r:   r*(3*r)*B = 4*r*2r - 6 = 4*6-6 = 18
  s=-2r: (-2r)*(-3r)*C = 4*(-2r)*(-r) - 6 = 8*3-6 = 18
-- Simplify
  s=0:     2*3*A =  6
  s=r:     3*3*B = 18
  s=-2r: 2*3*3*C = 18
-- Simplify
  s=0:         A = 1
  s=r:         B = 2
  s=-2r:       C = 1

Thus we get
  L f s = 1/s + 2/(s-r) + 1/(s+2r)
which we can see is the Laplace transform of
  f(t) = 1+2*e^(r*t)+e^(-2*r*t)
(using linarity and the transform of exponentials).

Now checking the three equations:
  f(0) = 1 + 2*1 + 1 = 4 -- OK!

  f'(t)  = 2*r*e^(r*t)-2*r*e^(-2*r*t)
  f''(t) =   6*e^(r*t)+ 12*e^(-2*r*t)

  f'(0) = 2*r*1-2*r*1 = 0 -- OK!

  LHS = f''(t) + r*f'(t)
      = 6*e^(r*t)+ 12*e^(-2*r*t) + r*(2*r*e^(r*t)-2*r*e^(-2*r*t))
      = (6+6)*e^(r*t) + (12-6)*e^(-2*r*t)
      = 6*(2*e^(r*t) + e^(-2*r*t))

  RHS = 6*(f(t)-1)
      = 6*(1+2*e^(r*t)+e^(-2*r*t) -1)
      = 6*(2*e^(r*t) + e^(-2*r*t))

Thus LHS = RHS
QED.

----------------------------------------------------------------

\begin{code}
scale = map . (*)
integ c cs = c : zipWith (/) cs [1..]
instance Num a => Num [a] where
  (+) = zipWithLonger (+)
  negate = map negate
  fromInteger = (:[]).fromInteger
  -- TODO fill in the rest (not needed on the exam)

zipWithLonger :: (a->a->a) -> [a] -> [a] -> [a]
zipWithLonger op [] bs = bs
zipWithLonger op as [] = as
zipWithLonger op (a:as) (b:bs) = op a b : zipWithLonger op as bs
\end{code}
