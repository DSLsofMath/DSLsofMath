Laplace: Consider the following differential equation:

  f'' = (3*f'-f)/2,  f(0) = 1,  f'(0) = 5/2

a) [10p] Solve the equation assuming that |f| can be expressed
    by a power series |fs|, that is, use |integ| and the differential
    equation to express the relation between |fs|, |fs'|, |fs''|.
    What are the first three coefficients of |fs|?
    Explain how you compute them.
\begin{code}
-- Transform the diff. equation to list form with f = eval fs, ...
fs'' = scale (3/2) fs' + scale (-1/2) fs
-- connect fs, fs', fs'' with integP and  f(0) = 1,  f'(0) = 5/2
fs' = integ  (5/2)  fs''
fs  = integ  1      fs'

-- Hand-computation - call them gs, gs', gs'' to avoid name-clash.
gs   =  1    : 5/2/1 : a0/1/2 : error "TODO"
gs'  = (5/2) : a0/1 : error "TODO"
gs'' =   a0  : error "TODO"
a0 = (3/2)*(5/2) + (-1/2)*1 -- = 15/4 - 2/4 = 13/4

-- Answer: [1, 5/2, 13/8]
-- Not part of the exam: helper definitions
check = take 3 fs == take 3 gs
main = print (take 3 fs)

type P = []
integ :: (Enum a, Fractional a) => a -> P a -> P a
integ c fs' = c : zipWith (/) fs' [1..]

instance Num a => Num (P a) where
  (+) = addP

scale :: Num a => a -> P a -> P a
scale c = map (c*)

addP :: Num a => P a -> P a -> P a
addP = zipWithLonger (+)

zipWithLonger :: (a->a->a) -> (P a -> P a -> P a)
zipWithLonger _op [] ys = ys
zipWithLonger _op xs [] = xs
zipWithLonger op (x:xs) (y:ys) = op x y : zipWithLonger op xs ys
\end{code}

b) [15p] Solve the equation using the Laplace transform.  You
    should need this formula (note that |a| can be a complex
    number) and the rules for linearity + derivative:

    L  (\t. exp (a*t)) s  =  1 / (s - a)

    Show that your solution does indeed satisfy the three
    requirements.

Use the L-D-law:
  L (D g) s = - g 0 + s*L g s
for g=f and for g=f'
  L f'  s = - f 0 + s*L f s = -1 + s*L f s
  L f'' s = - f' 0 + s*L f' s = -5/2 + s*(-1 + s*L f s)
          = -5/2 - s + s^2*L f s


-- Transform the equation
L f'' s = L ((3*f'-f)/2) s
-- Use linearity and the instances of the L-D-law from above.
-5/2 - s + s^2*L f s = (3/2)*(-1 + s*L f s) - (1/2)*L f s
-- let F = L f and simplify / collect terms
(s^2 -(3/2)*s + 1/2)*F s = -3/2 + s + 5/2
-- Simplify + factorise
F s = (s+1)/((s-1)*(s-1/2))
-- Partial fraction decomposition: let F s = A/(s-1) + B/(s-1/2)
(s+1)/((s-1)*(s-1/2)) = A/(s-1) + B/(s-1/2)
-- Multiply both sides with (s-1)*(s-1/2)
s+1 = A*(s-1/2) + B*(s-1)
-- Specialise to s=1/2 and to s=1
s=1/2: 3/2 = B*(-1/2) => B = -3
s=1:     2 = A*(1/2)  => A =  4
-- Thus we have found
F s = 4/(s-1) - 3/(s-1/2)
-- and can transform back by "pattern matching"
f   t = 4*exp t - 3*exp(t/2)
-- Check: first compute f' and f''
f'  t = 4*exp t - (3/2)*exp(t/2)
f'' t = 4*exp t - (3/4)*exp(t/2)
-- Then check the main equation

LHS = f'' t = 4*exp t - (3/4)*exp(t/2)
RHS = (3*f' t - f t)/2
    = (3*(4*exp t - (3/2)*exp(t/2))
       - (4*exp t - 3    *exp(t/2)))/2
    = (12-4)/2*exp t - ((9/2)-3)/2*exp (t/2)
    = 4*exp t - 3/4*exp (t/2)
    = LHS  -- OK!
-- Finally, check the initial conditions
f  0 = 4*exp 0 - 3*exp(0/2) = 4-3 = 1  -- OK
f' 0 = 4*exp 0 - (3/2)*exp (0/2) = 4-3/2 = 5/2   -- OK

-- Answer: the solution is f t = 4*exp t - 3*exp(t/2)

-- Optional extra check: the 3rd term from a) is f'' 0 / 2
-- f'' 0 / 2 = (4*1-(3/4)*1)/2 = (16-3)/8 = 13/8  -- OK


