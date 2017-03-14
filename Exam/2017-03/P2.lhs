> module P2 where
> import PS
> default (Rational, Integer)

[25pts] Consider the following differential equation:

    f'' t + 4*f t = 6*cos t,       f 0 = 0,   f' 0 = 0

    * [10pts] Solve the equation assuming that |f| can be
      expressed by a power series |fs|, that is, use |integ| and the
      differential equation to express the relation between |fs|, |fs'|,
      |fs''|, and |rhs| where |rhs| is the power series representation
      of |(6*).cos|.

    What are the first four coefficients of |fs|?

----------------

> fs    =  integ fs' 0
> fs'   =  integ fs'' 0
> fs''  =  rhs - 4*fs

One way of describing |cos| is as a solution to |g'' = -g|.

> rhs   =  integ rhs' 6
> rhs'  =  - (integ rhs 0)

Short: Hand-computing these series a few steps (more than asked for in
the exam question) gives us

> hfs    =  0:  0:   3:  0: (-5)/4:[]
> hfs'   =  0:  6:   0: -5: []
> hfs''  =  6:  0: -15: []

> hrhs   =  6:  0:  -3:  0: (1/4): []
> hrhs'  =  0: -6:   0:  1:  0   : []

Details of the method: start by filling in the constant term for uses of |integ|

< fs    =  0:
< fs'   =  0:
< fs''  =
<
< rhs   =  6:
< rhs'  =  0:

Then compute |head fs''| from |head rhs == 6| and |head fs == 0|

< fs''  =  6:

Now we fill in the second coefficients for uses of |integ|

< fs    =  0:0:
< fs'   =  0:6:
< fs''  =  6:
<
< rhs   =  6: 0:
< rhs'  =  0:-6:

In fact we can compute |fs| one step further as |(fs!!1) / 2|

< fs    =  0:0:3

Then we compute |fs''!!1| as before: from |rhs!!1 == 0| and |fs!!1 == 0|

< fs''  =  6:0

And this can be propagated to |fs'| and |fs|

< fs    =  0:0:3:0
< fs'   =  0:6:0

We have now reached the first four terms as requested: |f t ~= 3*t^2|

----------------------------------------------------------------

    * [15pts] Solve the equation using the Laplace transform.  You
        should need only two formulas (and linearity):

<   ℒ (\t -> exp (a*t)) s  = 1 / (s - a)

<   2 * cos t = e^{i*t} + e^{-i*t}

Helper computations, using the initial conditions:
  L f'  s  =  -f 0  + s*L f s   ={here}=    s*L f s
  L f'' s  =  -f' 0 + s*L f' s  ={here}=  s^2*L f s

Start from the point free form of the equation:

  f'' + 4*f = (6*).cos

apply |\g -> L g s| to both sides to get LHS and RHS.

  LHS
=
  L (f'' + 4*f) s
= -- linearity
  L f'' s + 4*L f s
= -- helper computation of L f'' s
  (s^2 + 4) * L f s
=
  RHS
=
  L ((6*).cos) s
= -- the |cos| equation, Laplace of exponential, linearity
  3*(1/(s-i) + 1/(s+i))

Thus we have

  (s^2 + 4) * L f s  =  3*(1/(s-i) + 1/(s+i))

Factoring (and juxtaposition for multiplication):
  s^2 + 4 =
Multiply by (s-i)(s+i) on both sides:
  (s-i)(s+i)(s-2i)(s+2i)L f s  =  3(s+i) + 3(s-i)  =  6s

Ansatz:
  L f s = A/(s-i) + B/(s+i) + C/(s-2i) + D/(s+2i)
We then get
  6s  =      (s+i)(s-2i)(s+2i)A
      + (s-i)     (s-2i)(s+2i)B
      + (s-i)(s+i)      (s+2i)C
      + (s-i)(s+i)(s-2i)      D

Let s=i:    6i = (i+i)  (i-2i) (i+2i)  A = (2i)(-i)(3i)  A = 6iA   => A = 1
Let s=-i:  -6i = (-i-i) (-i-2i)(-i+2i) B = (-2i)(-3i)(i) B = -6iB  => B = 1
Let s=2i:  12i = (2i-i) (2i+i) (2i+2i) C = (i)(3i)(4i)   C = -12iC => C = -1
Let s=-2i:-12i = (-2i-i)(-2i+i)(-2i-2i)D = (-3i)(-i)(-4i)D = 12iD  => D = -1

Thus
  L f s = (1/(s-i) + 1/(s+i)) - (1/(s-2i) + 1/(s+2i))
which we recognise as the transform of exponentials
  f t = (exp(it)+exp(-it)) - (exp(2it)+exp(-2it))
and we can apply the |cos| formula again
  f t = 2*cos t - 2*cos (2*t)

Last step: check the original specification:

First compute derivatives:
  f' t  = -2*sin t - 4*sin (2*t)
  f'' t = -2*cos t + 8*cos (2*t)

Then check boundary conditions:
  f 0  =  2*cos 0 - 2*cos 0 = 2 - 2 = 0   -- OK
  f' 0 = -2*sin 0 - 4*sin 0 = 0 - 0 = 0   -- OK

Finally check the diff. eq.:

  f'' t + 4*f t
=
  (-2*cos t + 8*cos (2*t)) + 4*(2*cos t - 2*cos (2*t))
=
  6*cos t

OK.

----------------------------------------------------------------
-- Sanity check - not part of the exam question:

> eqPrefix :: Eq a => [a] -> PS a -> Bool
> eqPrefix xs ps = and (zipWith (==) xs (coeff ps))
> check =    eqPrefix hfs fs
>         && eqPrefix hfs' fs'
>         && eqPrefix hfs'' fs''
>         && eqPrefix hrhs rhs
>         && eqPrefix hrhs' rhs'

> main = print check
