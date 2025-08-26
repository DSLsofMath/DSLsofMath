4. [25p] Typing maths: differentials

   The Newton quotient Δy/Δx is actually the quotient of two
quantities, Δy and Δx. It is not at all clear, however, that the
derivative dy/dx, the limit of Δy/Δx as Δx approaches zero, can
be regarded as a quotient.  If y is a continuous function of x,
then Δy approaches zero when Δx approaches zero, so dy/dx
appears to be the meaningless quantity 0/0. Nevertheless, it is
sometimes useful to be able to refer to quantities dy and dx in
such a way that quotient is the derivative dy/dx. We can justify
this by regarding dx as a new independent variable (called *the
differential of x*) and defining a new dependent variable dy
(*the differential of y) as a function of x and dx by

  dy = \frac{dy}{dx} dx = f'(x) dx

----------------
4a. Give the types of x, y, f, dy/dx, dx, dy, f'. Explain your reasoning.

Let X and Y be subsets of the REALs.
We have a few variants possible - here is one:
    x : X
    y : X -> Y             -- "y is a ... function of x"
    f : X -> Y             -- same as y
dy/dx : X -> Y             -- matches the type of the function
   dx : REAL               -- or perhaps REAL except for zero
   dy : X -> REAL -> REAL  -- "dy as ... a function of x and dx"
   f' : X -> Y             -- same as dy/dx
There are some inconsistencies, for example the division in
  dy / dx
takes one two-argument function dy and scales it with recip dx.
The result should have the same type as dy : X -> REAL -> REAL
but as it does not depend on the second argument anymore, it is
"silently coerced" to the type X -> REAL or X -> Y.

Note that the 2-arg. function dy is rather boring:
  dy x dx = scale dx (D f x)

----------------
4b:

  dy x dx
=
  scale dx (D (f.(^2)) x)
=
  scale dx (((D f.(^2))*D (^2)) x)
=
  scale dx ((D f (x^2) * 2*x)
=
  2*x*f'(x^2)*dx

----------------
4c: For a two-argument function the corresponding differential would be

  dz x1 x2 dx1 dx2 = D1 g (x1,x2) * dx1 + D2 g (x1,x2) * dx2
    {- optionally
      = dot (D1 g (x1,x2), D2 g (x1,x2)) (dx1,dx2)
    -}
where the partial derivatives D1 and D2 are defined in terms of D:

D1 g (x1,x2) = D g1 x1
  where g1 x = g(x,x2)

D2 g (x1,x2) = D g2 x2
  where g2 x = g(x1,x)

----------------------------------------------------------------
Just checking:
\begin{code}
d :: (a->b)->(a->b)
d = undefined

d1, d2 :: ((a,b)->c) -> ((a,b)->c)

d1 g (x1,x2) = d g1 x1
  where g1 x = g(x,x2)

d2 g (x1,x2) = d g2 x2
  where g2 x = g(x1,x)
\end{code}
