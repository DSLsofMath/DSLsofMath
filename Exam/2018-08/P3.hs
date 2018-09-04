-- Problem 3: -4*f'' + f == exp, f(0)=0, f'(0)=-1/2
module P3 where
import PS

-- 3a: first four coefficients

fs   = integ fs'  0
fs'  = integ fs'' (-1/2)
fs'' = (1/4)*(fs - exps)
exps = integ exps 1

{-
fs    =   0   : -1/2 : -1/8  : -1/16 : _
fs'   =  -1/2 : -1/4 : -3/16 : _
fs''  =  -1/4 : -3/8 : _
exps  =   1   :  1   :  1/2  : _
-}

-- 3b: Solve with Laplace

{-
First we use |L f' s = -f 0 + s*L f s| twice:

Lemma: (using also the initial conditions)
  L f'' s
= -f' 0 + s*L f' s
= -f' 0 + s*(-f 0 + s*L f s)
= -f' 0 - (f 0)*s + s²*L f s
= 1/2 + s²*L f s

Then we start calculating:

  LHS
= -- def.
  L (-4*f'' + f) s
= -- linearity
  -4*L f'' s + L f s
= -- Lemma
  -4*(1/2 + s²*L f s) + L f s
= -- collect terms
  -2 + (1-4*s²)*L f s
= -- factorisation
  -2 + (1-2*s)*(1+2*s)*L f s


  RHS
= -- def.
  L exp s
= -- law from exam question
  1/(s-1)

Thus we need to solve
  -2 + (1-2*s)*(1+2*s)*L f s == 1/(s-1)
<=>
  (1-2*s)*(1+2*s)*L f s == 1/(s-1) + 2
<=>
  L f s == (1/(s-1) + 2)/(1-2*s)/(1+2*s)

Ansatz: L f s = A/(s-1) + B/(1-2*s) + C/(1+2*s)

  A/(s-1) + B/(1-2*s) + C/(1+2*s) == (1/(s-1) + 2)/(1-2*s)/(1+2*s)
<= multiply by (s-1)*(1-2*s)*(1+2*s)
  A*(1-2*s)*(1+2*s) + B*(s-1)*(1+2*s) + C*(s-1)*(1-2*s) == 1 + 2*(s-1)
<= -- Solve by picking tree instances: s1=1, s2=1/2, s3=-1/2
  A*(-1)*3 == 1
  B*(-1/2)*2 == 1 + 2*(-1/2)
  C*(-3/2)*2 == 1 + 2*(-3/2)
<=>
  -3*A == 1
    -B == 0
  -3*C == -2
<=>
  A == -1/3
  B == 0
  C == 2/3

Thus
  L f s = (-1/3)/(s-1) + (2/3)/(1+2*s)
        = (-1/3)/(s-1) + (1/3)/(s+1/2)
        = (1/3)*(1/(s+1/2) - 1/(s-1))

Solution:  f x = (1/3)*(exp (-x/2) - exp x)

Check:
  f 0    = (1/3)*(1 - 1) = 0 -- OK

  f' x   = (1/3)*(-exp (-x/2)/2 - exp x)
  f' 0   = (1/3)*(-1/2 - 1) = (1/3)*(-3/2) = -1/2 -- OK

  f'' x  = (1/3)*(exp (-x/2)/4 - exp x)

  LHS
= -4*f'' x + f x
= -4*(1/3)*(exp (-x/2)/4 - exp x) + (1/3)*(exp (-x/2) - exp x)
= (-4/3)*exp (-x/2)/4 + (4/3)*exp x + (1/3)*exp (-x/2) - (1/3)*exp x
= (-1/3 + 1/3)*exp (-x/2) + (4/3-1/3)*exp x
= exp x
= RHS

OK - it all fits.
-}

-- Not part of the exam question, but useful for testing:
fs, fs', fs'', exps :: (Eq a, Fractional a) => PS a

main :: IO ()
main = print (take 4 (coeff fs)  ==  [0, -1/2, -1/8, -1/16 :: Rational])
