\begin{code}
module P2_Laplace where
\end{code}

f''(t) = 2*f(t) - f'(t),      f(0) = a,     f'(0) = b

----------------
P2 a) Power-series expansion method

Assume f is represented by the power series coefficients as, f' by as'
and f'' by as''.

We then have

\begin{code}
as    = integ  a  as'
as'   = integ  b  as''
as''  = scale 2 as + scale (-1) as'
\end{code}

We can fill in the first few coefficients "by hand":

as   = a        :  b       :  a - b/2   : -a/3 + b/2
as'  = b        :  2*a-b   : -a + 3*b/2
as'  = (2*a-b)  :  3*b-2*a : 

Answer: the series representation starts with a : b : a-b/2 : ...

----------------
P2 b) Laplace transform method

We transform both sides

  L f'' s = 2 * L f s  -  L f' s

and remember the Laplace-D-laws:
  L (D f) s = -f 0 + s*L f s
  L (D f') s = -f' 0 + s*L f' s
or, using our initial conditions:
  L f'  s = -a + s*L f s
  L f'' s = -b + s*L f' s
          = -b - a*s + s^2*L f s
Now let F = L f and use the Laplace-D-laws:

  -b - a*s + s^2*F s = 2 * F s  -  (-a + s*F s)

Collect terms (F s on the left, the rest on the right):

  (s^2 + s - 2)*F s = a + b + a*s

Factorise: s^2 + s - 2 = (s-1)*(s+2)

  F s = (a+b + a*s)/((s-1)*(s+2))

Now use partial fraction decomposition:

Ansatz:  F s = A/(s-1) + B/(s+2)

Multiply the least common multiplier to get a polynomial equation:

  A*(s+2) + B*(s-1) = a+b + a*s

Make a linear system in A and B by letting s=1 and s=-2:

s=1:  A*3 = a+b + a       <=>   A = (2*a+b)/3
s=-2: B*(-3) = a+b - 2*a  <=>   B =   (a-b)/3

Thus F s = (2*a+b)/3 * 1/(s-1) + (a-b)/3 * 1/(s+2)

Transform back through "pattern matching" and linearity

f t = (2*a+b)/3 * e^t + (a-b)/3 * e^(-2*t)

Check the equations:
+ Compute the symbolic derivatives:

f'  t = (2*a+b)/3 * e^t - 2*(a-b)/3 * e^(-2*t)
f'' t = (2*a+b)/3 * e^t + 4*(a-b)/3 * e^(-2*t)

f(0) = (2*a+b)/3 * e^0 + (a-b)/3 * e^(-2*0)
     = (2*a+b)/3 + (a-b)/3
     = a  -- OK!
f'(0)= (2*a+b)/3 * e^0 - 2*(a-b)/3 * e^(-2*0)
     = (2*a+b)/3 - 2*(a-b)/3
     = b  -- OK!

LHS = L f'' s
    = (2*a+b)/3 * e^t + 4*(a-b)/3 * e^(-2*t)

RHS = 2 * L f s  -  L f' s
    =  2*((2*a+b)/3 * e^t + (a-b)/3 * e^(-2*t))
        -((2*a+b)/3 * e^t - 2*(a-b)/3 * e^(-2*t))
    = (2-1)*(2*a+b)/3*e^t + (2*(a-b)+2*(a-b))/3 * e^(-2*t)
    = (2*a+b)/3 * e^t + 4*(a-b)/3 * e^(-2*t)
    = LHS  -- OK!

Yes, the three conditions are satisfied.

----------------
Helper code - not part of the exam question
\begin{code}
type REAL = Double
a, b :: REAL
a = 3
b = 100
integ :: (Fractional a, Enum a) => a -> [a] -> [a]
integ a0 as = a0 : zipWith (/) as [1..]
scale c = map (c*)
addL :: Num a => [a] -> [a] -> [a]
addL = zipWithLonger (+)
zipWithLonger op = z where
  z [] ys = ys
  z xs [] = xs
  z (x:xs) (y:ys) = op x y : z xs ys
  
instance Num a => Num [a] where (+) = addL; -- bare minimum for testing
\end{code}
TODO more operators
