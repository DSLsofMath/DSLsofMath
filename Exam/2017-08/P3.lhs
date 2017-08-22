Laplace: f'' t - c * f' t + 4 * f t = 0, f 0 = 2, f' 0 = c
  where  c = 3*r
         r = sqrt 2

> module P3 where
> import PS

3a: Solve using power series

> c :: Fractional a => a
> c = 4.242640687119286 -- 3 * sqrt 2
> fs'' = c*fs' - 4*fs
> fs' = integ fs'' c
> fs  = integ fs'  2

Hand-computing these series gives us:

Step 1: definition of |integ| (and ignoring the PS constructor):

c = 3*r
r = sqrt 2

< fs   ~=   2 : _
< fs'  ~=   c : _

Compute the first element of |fs''|:

< head fs''  =  c*c-4*2 = 10

Now we can compute the 2nd elements of |fs| and |fs'| from the first
elements of |fs'| and |fs''|:

< fs    ~=   2 :  c : _
< fs'   ~=   c : 10 : __

Finally we can compute the 3rd element of |fs| from the 2nd of |fs'|:

< fs    ~=   2 :  c : 5 : _

And that is all the question asked for:

Answer: take 3 fs ~= [2,c,5].

----------------
3b: Solve using the Laplace transform

Derivative rule + initial conditions:

L f' s = -f 0 + s * L f s = -2 + s * L f s
L f'' s = -f' 0 - s*f 0 + s^2*L f s = -c - 2*s + s^2*L f s

The Laplace transform of the equation thus gives

  (-c - 2*s + s^2*L f s)-c*(-2 + s * L f s)+4*L f s = 0

Collecting terms:

  (-c - 2*s +2*c) + (s^2-c*s+4)*L f s  = 0

Factoring and moving terms to make the L-term stand alone.

  (s-r)*(s-2*r)*L f s  = 2*s - 3*r

Now use the Ansatz  L f s = A/(s-r) + B/(s-2*r)

  A*(s-2*r) + B*(s-r) = 2*s - 3*r

Solve for A and B (for example by inspection).

  A=1, B=1

Thus

  L f s = 1/(s-r) + 1/(s-2r)

Reverse transform by linearity and the rule in the question.

  f t = exp(r*t) + exp(2*r*t)

Check the initial conditions:

  f 0 = 1 + 1 = 2 -- OK

  f' t = r*exp(r*t) + 2*r*exp(2*r*t)
  f' 0 = r + 2*r = 3*r = c -- OK

Check the full equation:

  f'' t = r^2*exp(r*t) + 4*r^2*exp(2*r*t)
        =   2*exp(r*t) +     8*exp(2*r*t)

  f'' t - c * f' t + 4 * f t
= -- expand the definitions + use r^2 = 2
    2*exp(r*t) + 8*exp(2*r*t)
  - 3*r*(r*exp(r*t) + 2*r*exp(2*r*t))
  + 4*(exp(r*t) + exp(2*r*t))
= -- collect the coefficients
  (2-6+4)*exp(r*t) + (8-12+4)*exp(2*r*t)
= -- simple arithmetics
  0

OK!
