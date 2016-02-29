3. The Laplace transform
------------------------

This material was inspired by @quinn2008discovering, which is highly
recommended reading.

Consider the differential equation

< f'' x - 3 * f' x + 2 * f x = exp (3 * x),  f 0 = 1,  f' 0 = 0

We can solve such equations with the machinery of power series:

> fs = integ fs' 1
>      where
>      fs' = integ (exp (3*x) + 3 * fs' - 2 * fs) 0

We have done this by "zooming in" on the function `f` and representing
it by a power series, `f x = Σ a_n * x^n`.  This allows us to reduce
the problem of finding a function ℝ → ℝ to that of finding a function
ℕ → ℝ (or finding sufficiently many values for a good approximation).

Still, recursive equations are not always easy to solve (especially
without a computer), so it's worth looking for alternatives.

We have gone from

<     a : ℕ → ℝ     to       f : ℝ → ℝ

via

<                Σ a_n * x^n

In "zooming out", we would like to go one step further

<     a : ℕ → ℝ     to       f : ℝ → ℝ    to       ??

via

<                Σ a_n * x^n              ??

At least the second part is "easy".  The analogue of series for a
continuous function is integral

<                Σ a_n * x^n              ∫_0^{\infty} (f t) * x^t dt

We note that, for the integral `∫_0^{\infty} (f t) * x^t dt` to
converge for a larger class of functions (say, bounded functions), we
have to limit ourselves to `|x| < 1`.  Both this condition and the
integral make sense for `x ∈ ℂ`, so we could take

<     a : ℕ → ℝ     to     f : ℝ → ℝ    to     F : {z | |z| < 1} → ℂ

but let us stick to ℝ for now.

Writing, somewhat optimistically

< ℒ f x = ∫_0^{\infty} (f t) * x^t dt

we can ask ourselves what `ℒ f'` looks like.  After all, we want to
solve differential equations by "zooming out".  We have

< ℒ f' x = ∫_0^{\infty} (f' t) * x^t dt

Remember that `(f * g)' = f' * g + f * g'`, therefore

<   ℒ f' x

< =  {g t = x^t; g' t = log x * x^t}

<   ∫_0^{\infty} (f t * x^t)' - f t * log x * x^t dt

< =

<   ∫_0^{\infty} (f t * x^t)' dt  -  ∫_0^{\infty} f t * log x * x^t dt

< =

<   lim_{t -> \infty} (f t * x^t) - (f 0 * x^0)  - log x * ∫_0^{\infty} f t * x^t dt

< =

<   - f 0 - log x * ∫_0^{\infty} f t * x^t dt

< =

<   -f 0  - log x * ℒ f x

The factor `log x` is somewhat awkward.  Let us therefore return to
the definition of `ℒ` and operate a change of variables:

<  ℒ f x = ∫_0^{\infty} (f t) * x^t dt

< <=>

<  ℒ f x = ∫_0^{\infty} (f t) * (exp (log x))^t dt

< <=>

<  ℒ f x = ∫_0^{\infty} (f t) * (exp (log x *t)) dt

Since `log x < 0` for `|x| < 1`, we make the substitution `-s = log
x`.  The condition `|x| < 1` becomes `s > 0` (or, in ℂ, real s > 0),
and we have

<  ℒ f s = ∫_0^{\infty} (f t) * (exp (-s * t)) dt

This is the definition of the Laplace transform of the function `f`.
Going back to the problem of computing `ℒ f'`, we now have

<  ℒ f' s

< =

< - f 0 + s * ℒ f s

We have obtained

< ℒ f' s  =  s * ℒ f s - f 0

From this, we can deduce

< ℒ f'' s

< =

< s * ℒ f' s - f' 0

< =

< s * (s * ℒ f s - f 0) - f' 0

< =

< s^2 * ℒ f s - s * f 0 - f' 0

Exercise: what is the general formula for ℒ f^(n) s?

Returning to our differential equation, we have

< f'' x - 3 * f' x + 2 * f x = exp (3 * x),  f 0 = 1,  f' 0 = 0

< <=>  {point-free form}

< f'' - 3 * f' + 2 * f  = exp ∘ (3*),  f 0 = 1,  f' 0 = 0

< =>   {applying ℒ to both sides}

< ℒ (f'' - 3 * f' + 2 * f)  =  ℒ (exp ∘ (3*)), f 0 = 1, f' 0 = 0  (1)

**Remark:** Note that this is a necessary condition, but not a
sufficient one.  The Laplace transform is not injective.  For one
thing, it does not take into account the behaviour of `f` for negative
arguments.  Because of this, we often assume that the domain of
definition for functions to which we apply the Laplace transform is
`ℝ_{≥ 0}`.  For another, it is known that changing the values of `f`
for a countable number of its arguments does not change the value of
the integral.

For the definition of ℒ and the linearity of the integral, we have
that, for any `f` and `g` for which the transformation is defined, and
for any constants α and β

< ℒ (α * f + β * g)  =  α * ℒ f  + β * ℒ g

(note that this is an equality between functions).

Applying this to the left-hand side of (1), we have for any `s`

< ℒ (f'' - 3 * f' + 2 * f) s

< =

< ℒ f'' s - 3 * ℒ f' s + 2 * ℒ f s

< = {re-writing ℒ f'' and ℒ f' in terms of ℒ f}

< s^2 * ℒ f s - s * f 0 - f' 0 - 3 * (s ⋅ ℒ f s - f 0) + 2 * ℒ f

< = {f 0 = 1, f' 0 = 0}

< (s^2 - 3 * s + 2) * ℒ f s - s + 3

For the right-hand side, we apply the definition:

< ℒ (exp ∘ (3*)) s

< =

< ∫_0^{\infty} (exp (3 * t)) * (exp (-s * t)) dt

< =

< ∫_0^{\infty} exp ((3 - s) * t)) dt

< =

< lim_{t -> \infty} 1 / (3 - s) * exp ((3 - s) * t) -
< 1 / (3 - s) * exp ((3 - s) * 0)

< = {for s > 3}

< 1 / (s - 3)

Therefore, we have, writing `F` for `ℒ f`

< (s^2 - 3 * s + 2) * F s - s + 3 = 1/(s-3)

and therefore

< F s

< =

< (1 / (s - 3) + s - 3) / (s^2 - 3 * s + 2)

< =  {s^2 - 3 * s + 2 = (s - 1) * (s - 2)}

< (10 - 6 * s + s^2) / ((s - 1)*(s-2)*(s-3))

We now have the problem of "recovering" the function `f` from its
Laplace transform.  The standard approach is to use the linearity of
`ℒ` to write `F` as a sum of functions with known inverse transforms.
We know one such function:

< exp (α * t)  is the inverse of  1 / (s - α)

In fact, in our case, this is all we need.

The idea is to write `F s` as a sum of three fractions with
denominators `s - 1`, `s - 2`, and `s - 3` respectively, i.e., to find
`A`, `B`, and `C` such that

< A / (s - 1) + B / (s - 2) + C / (s - 3) =
< (10 - 6 * s + s^2) / ((s - 1) * (s - 2) * (s - 3))

< =>

< A * (s - 2) * (s - 3) + B * (s - 1) * (s - 3) + C * (s - 1) * (s - 2)
< = 10 - 6 * s + s^2                                                     (2)

We need this equality (2) to hold for values `s > 3`.  A *sufficient*
condition for this is for (2) to hold for *all* `s`.  A *necessary*
condition for this is for (2) to hold for the specific values 1, 2,
and 3.

For `s` = 1:

< A * (-1) * (-2) = 10 - 6 + 1  =>  A = 2.5

For `s` = 2:

< B * 1 * (-1) = 10 - 12 + 4    =>  B = -2

For `s` = 3:

< C * 2 * 1 = 10 - 18 + 9      =>  C = 0.5

It is now easy to check that, with these values, (2) does indeed hold,
and therefore that we have

< F s = 2.5 * (1 / (s - 1)) - 2 * (1 / (s - 2)) +
<       0.5 * (1 / (s - 3))

The inverse transform is now easy:

< f t = 2.5 * exp x - 2 * exp (2 * x) + 0.5 * exp (3 * x)

Our mix of necessary and sufficient conditions makes it necessary to
check that we have, indeed, a solution for the differential equation.
The verification is in this case trivial.

