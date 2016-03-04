Typing Mathematics
------------------

As discussed in Lecture 5, it is often useful to try to type the
elements of a mathematical expression.

For example, on page 169 of @maclane1986mathematics, we read

  > [...] a function `z = f (x, y)` for all points `(x, y)` in some
  > open set `U` of the cartesian `(x, y)`-plane.  [...] If one holds
  > `y` fixed, the quantity `z` remains just a function of `x`; its
  > derivative, when it exists, is called the *partial derivative*
  > with respect to `x`.  Thus at a point `(x, y)` in `U` this
  > derivative for `h ≠ 0` is

<    ∂ z / ∂ x  =  f'_{x} (x, y) =
<                  lim_{h -> 0} (f (x + h, y) - f (x, y)) / h

What are the types of the elements involved?  We have

<   U ⊆ ℝ x ℝ    -- cartesian plane

<   f : U -> ℝ

<   z : U -> ℝ   -- but see below

<   f'_{x}  :  U -> ℝ

The `x` in the subscript of `f'` is *not* a real number, but a symbol
(a `Char`).

The expression `(x, y)` has several occurrences.  The first two denote
variables of type `U`, the third is just a name (`(x, y)`-plane).  The
third denotes a variable of type `U`, it is bound by a universal
quantifier

< ∀ (x, y) ∈ U

`h` appears to be a non-zero real number, bound by a universal
quantifier, but that is incorrect.  In fact, `h` is used as a variable
to construct the arguments of a function, whose limit is then taken
at 0.

That function, which we can denote by `φ` has the type `φ : U ->
ℝ-{0} -> ℝ` and is defined by

< φ (x, y) h = (f (x + h, y) - f (x, y)) / h

The limit is then `lim (φ (x, y)) 0`.  Note that `0` is a limit point
of `ℝ`, so the type of `lim` is the one we have discussed:

< lim : (X -> ℝ) -> {p | p ∈ ℝ, p limit point of X } -> ℝ

`z = f (x, y)` probably does not mean that `z ∈ ℝ`, although the
phrase "the quantity `z`" suggests this.  A possible interpretation is
that `z` is used to abbreviate the expression `f(x, y)`; thus,
everywhere we can replace `z` with `f(x, y)`.  In particular,
`∂ z / ∂ x` becomes `∂ f (x, y) / ∂ x`, which we can interpret as
`∂ f / ∂ x` applied to `(x, y)` (remember that `(x, y)` is bound in
the context by a universal quantifier).  There is the added difficulty
that, just like `_{x}`, the `x` in `∂ x` is not the `x` bound by the
universal quantifier, but just a symbol.

References
----------
