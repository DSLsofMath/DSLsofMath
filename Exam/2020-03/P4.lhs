P4: Consider the following coupled differential equations:

    f'(x) = C*g(x),     f(0) = C
    g'(x) = f(x),       g(0) = 0

  where |C=(n+1)^2| and |n| is the last digit of your personal identity number.

a) [10p] Solve the equations assuming that |f| and |g| can be
   expressed by power series |fs| and |gs|, that is, use |integ|
   and the differential equation to express the relation between
   |fs|, |fs'|, |gs|, and |gs'|.  What are the first four
   coefficients of |fs|?

let c=m^2; m=n+1

Let us introduce fs, fs', gs, gs' :: [REAL] for the power series coefficients.
Then we get the following equations:
\begin{code}
scale = map . (*)
integ c cs = c : zipWith (/) cs [1..]

c = 1

fs  = integ c fs'
gs  = integ 0 gs'
fs' = scale c gs
gs' = fs
\end{code}
Then we substitute away gs' and reorder:
\begin{spec}
gs  = integ 0 fs
fs' = scale c gs
fs  = integ c fs'
\end{spec}
Finally we substitute away fs' and distribute (scale c)
\begin{spec}
gs  = integ 0 fs
fs  = scale c (integ 1 gs)
\end{spec}
Then we can start filling in the sequences "by hand":
Step 1: just read off start value from integ
\begin{spec}
gs  = 0 : _
fs  = c : _
\end{spec}
Step 2: move first coeff crosswise and divide by 1
\begin{spec}
gs  = 0 : c : _
fs  = c : 0 : _
\end{spec}
Step 3: move second coeff crosswise and divide by 2
\begin{spec}
gs  = 0 : c : 0     : _
fs  = c : 0 : c^2/2 : _
\end{spec}
Step 4: move third coeff crosswise and divide by 3
\begin{spec}
gs  = 0 : c : 0     : c^2/6 : _
fs  = c : 0 : c^2/2 : 0     : _
\end{spec}

Answer: take 4 fs = [0, c, 0, c^2/6]
More specifically, for the ten different digits, we get:
\begin{code}
answer :: Rational -> [Rational]
answer c = c : 0 : c^2/2 : 0     : []
main = mapM_ print $ map (answer.(^2).(1+)) [0..9]
\end{code}
  [  1,   0,   1/2,   0]
  [  4,   0,     8,   0]
  [  9,   0,  81/2,   0]
  [ 16,   0,   128,   0]
  [ 25,   0, 625/2,   0]
  [ 36,   0,   648,   0]
  [ 49,   0,2401/2,   0]
  [ 64,   0,  2048,   0]
  [ 81,   0,6561/2,   0]
  [100,   0,  5000,   0]

================================================================


f(x) = m^2/2*(e^(m*x) + e^(-m*x)) = m^2*cosh(m*x)
g(x) =   m/2*(e^(m*x) - e^(-m*x)) =   m*sinh(m*x)

----------------

b) [15p] Solve the equations using the Laplace transform.  You should
   need this formula (note that |alpha| can be zero) and the rules
   for linearity + derivative:

   ℒ\, (\lambda t.\, e^{\alpha*t})\, s  = 1 / (s - \alpha)\]

   Show that your solutions do indeed satisfy the four requirements.

f(0) = c; g(0) = 0
Transform both equations:
  L f' s = c*L g s
  L g' s =   L f s
Add the two equations from L (D f) s = -f 0 + s*L f s,  Forall f
  L f' s = -c+s*L f s
  L g' s = s*L g s
Combine the equations to eliminate L f' and L g':
  c*L g s = -c+s*L f s
    L f s =    s*L g s
write F = L f; G = L g and substitute the snd in the fst eq.
  c*G s = -c+s^2*G s
collect terms, remember c=m^2
  (m^2-s^2)*G s = -m^2
factor the polynomial in s, divide by (-1)
  (s-m)*(s+m)*G s = m^2
Now do partial fraction decomposition:
assume G s = A/(s-m) + B/(s+m) and try to find A and B
  (s-m)*(s+m)*(A/(s-m) + B/(s+m)) = m^2
simplify
  (s+m)*A + (s-m)*B = m^2
This is polynomial equation in s - we test it for s=+-m to get eqs for A and B
Case s=m:    2*m*A = m^2  <=>  A =  m/2
Case s=-m:  -2*m*B = m^2  <=>  B = -m/2

Thus G s = (m/2)*(1/(s-m) - 1/(s+m))

We now use linearity and "Laplace of exponential" backwards:
  g x = (m/2)*(exp (m*x) - exp (-m*x))
    (which may be recognized as m*sinh(m*x))
finally, from g' = f, we get
  f x = (m^2/2)*(exp (m*x) + exp (-m*x))
    (which may be recognized as m^2*cosh(m*x))

Last step: check all four equations:
  f' x = (m^2/2)* m*(exp (m*x) - exp (-m*x))
       = m^2* (m/2)*(exp (m*x) - exp (-m*x))
       = m^2* g x
       = C * g x
  g'(x) = f(x) -- by definition
  f 0 = (m^2/2)*(exp (m*0) + exp (-m*0)) = (m^2/2)*(1+1) = m^2 = C
  g 0 = (m/2)*(exp (m*0) - exp (-m*0)) = (m/2)*(1-1) = 0

Yes! All four equations hold.
