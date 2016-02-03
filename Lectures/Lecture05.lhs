Lecture 5: Types in Mathematics
===============================

1. Types in mathematics
-----------------------

Types are sometimes mentioned explicitly in mathematical texts:

- x ∈ ℝ
- $\sqrt{~} : ℝ_{≥0} → ℝ_{≥0}$
- (_)²  : ℝ → ℝ or, alternatively but *not* equivalently
- (_)²  : ℝ → $ℝ_{≥0}$

The types of "higher-order" operators are usually not given
explicitly:

- lim : (ℕ → ℝ) → ℝ  for lim_{n → ∞} {a_n}
- d/dt : (ℝ → ℝ) → ℝ → ℝ
   + sometimes, instead of df/dt one sees $f'$ or f° or D f
- ∂f/∂x_i : (ℝⁿ → ℝ) → ℝⁿ → ℝ
   + we mostly see ∂f/∂x, ∂f/∂y, ∂f/∂z etc. when, in the context, the
     function f has been given a definition of the form f (x, y, z) =
     ...
   + a better notation which doesn't rely on the names given to the
     arguments was popularised by Landau in @landau1934einführung
     (English edition @landau2001differential): D₁ for the partial
     derivative with respect to x₁, etc.

   + Exercise: for f : ℝ² → ℝ define D₁ and D₂ using only D.



2. Type inference and understanding
-----------------------------------

From [@sussman2013functional]:

  > A mechanical system is described by a Lagrangian function of the
  > system state (time, coordinates, and velocities).  A motion of the
  > system is described by a path that gives the coordinates for each
  > moment of time.  A path is allowed if and only if it satisfies the
  > Lagrange equations.  Traditionally, the Lagrange equations are
  > written

<        d   ∂L       ∂L
<        --  --   -   --  =  0
<        dt  ∂q°      ∂q

  > What could this expression possibly mean?

To start answering the question, we start typing the elements
involved:

(1) ∂L / ∂q suggests that L is a function of at least a pair of
arguments:

        L : ℝⁿ → ℝ,    n ≥ 2

This is consistent with the description: "Lagrangian function of the
system state (time, coordinates, and velocities)".  So we can take n =
3:

        L : ℝ³ → ℝ

(2) ∂L / ∂q suggests that "q" is the name of a real variable, one of
the three arguments to L.  In the context, which we do not have, we
would expect to find somewhere the definition of the Lagrangian as

        L (t, q, v) = ...

(3) therefore, ∂L / ∂q should also be a function of a triple of
arguments:

        ∂L / ∂q : ℝ³ → ℝ

It follows that the equation expresses a relation between *functions*,
therefore the 0 on the right-hand side is *not* the real number 0, but
rather the constant function 0:

        const 0  :  ℝ³ → ℝ
        const 0 (t, q, v) = 0

(4) We now have a problem: d / dt can only be applied to functions of
*one* real argument "t", and the result is a function of one real
argument:

        d   ∂L
        --  --    :  ℝ → ℝ
        dt  ∂q°

Since we subtract from this the function ∂L / ∂q, it follows that
this, too, must be of type ℝ → ℝ, contradiction.

(5) The expression ∂L / ∂q° appears to also be malformed.  We would
expect a variable name where we find q°, but q° is the same as dq /
dt, a function.

(6) Looking back at the description above, we see that the only
candidate for an application of d/dt is "a path that gives the
coordinates for each moment of time".  Thus, the path is a function of
time, let us say

        w  :  ℝ → ℝ
        w(t) is a coordinate at time t

We can now guess that the use of the plural form "equations" might
have something to do with the use of "coordinates".  In an
n-dimensional space, a position is given by n coordinates.  A path
would be a function

        w  :  ℝ → ℝⁿ

which is equivalent to n functions of type ℝ → ℝ.  We would then have
an equation for each of them.

(7) The Lagrangian is a "function of the system state (time,
coordinates, and velocities)".  If we have a path, then the
coordinates at any time are given by the path.  The velocity is the
derivative of the path, also fixed by the path:

        q  :  ℝ → ℝ
        q t  =  w t

        q° :  ℝ → ℝ
        q° t = dw / dt

The equations do not use a function L : ℝ³→ ℝ, but rather

<       L ∘ expand w  :  ℝ → ℝ

where the "combinator" expand is given by

<       expand      :  (ℝ → ℝ) → ℝ → ℝ³
<       expand w t  =  (t, w t, D w t)

(8) Similarly, using D₁, D₂, D₃ instead of ∂L/∂t etc., we have that,
instead of ∂L/∂q what is meant is

<      D₂ L ∘ expand w  :  ℝ → ℝ

and instead of ∂L/∂q°

<      D₃ L ∘ expand w  : ℝ → ℝ

The equation becomes

<      D (D₃ L ∘ expand w)  -  D₂ L ∘ expand w  =  0

a relation between functions of type ℝ → ℝ.  In particular, the
right-hand 0 is the constant function

<      const 0  :  ℝ → ℝ


References
----------
* [@sussman2013functional]
