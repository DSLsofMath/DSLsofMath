> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeSynonymInstances #-}

> module Lecture12 where

> import Lecture10
> import Lecture11

Lecture 12: The Exponential Function, Taylor Series
===================================================

1.  The exponential function
----------------------------

One of the classical analysis textbooks, Rudin's [-@rudin1987real]
starts with a prologue on the exponential function.  The first
sentence is

  > This is undoubtedly the most important function in mathematics.

Rudin goes on

  > It is defined, for every complex number `z`, by the formula

<          exp z  =  Σ (z^n) / n!

We have defined the exponential function as the function represented
by the power series

< expx :: Fractional a => PowerSeries a
< expx = integ expx 1

and approximated by

< exps :: Fractional a => a -> a
< exps = eval 100 expx

It is easy to see, using the definition of `integ` that the power
series `expx` is, indeed

< expx = [1, 1/2, 1/(2 * 3), ..., 1 / (2 * 3 * ... * n), ..]

We can compute the exponential for complex values if we can
give an instance of `Fractional` for complex numbers.  We could use
the datatype `Data.Complex`, but we prefer to roll our own in order to
remind the basic operations on complex numbers.

Complex values can be represented as pairs of real values

> type Complex a       =  (a, a)

> real                 =  fst
> imag                 =  snd

The operations on complex numbers are then translated to operations on
pairs of numbers:

> instance Num a => Num (Complex a) where
>   (x, x') + (y, y')  =  (x + y, x' + y')
>   (x, x') * (y, y')  =  (x * y - x' * y', x' * y + x * y')
>   fromInteger n      =  (fromInteger n, 0)

The operation of division reduces to complex multiplication (by the
conjugate of the denominator) and multiplication with a scalar (the
inverse of the square of the modulus of the denominator).

> a .* (x, y)    =  (a * x, a * y) -- multiplication with a real number

> conj (x, x')   =  (x, -x')

> instance Fractional a => Fractional (Complex a) where
>   x / y             =  (1/sqmod) .* x * conjy
>                        where
>                        conjy  =  conj y
>                        sqmod  =  real (y * conjy)
>   fromRational r    = (fromRational r, 0)
>

Now, we have, for example

> ex1  ::  Fractional a => Complex a
> ex1   =  exps (0, 1)

We have `ex1 = (0.5403023058681398,0.8414709848078965)`.  Note that

< coss 1  =  0.5403023058681398
< sins 1  =  0.8414709848078965

and therefore `exps (0, 1) = (coss 1, sins 1)`.  Coincidence?

Instead of evaluating the sum of the terms `a_n * z^n`, let us instead
collect the terms in a series:

> terms as z = terms1 as z 0
>   where
>   terms1 (Cons a as) z n = Cons (a * z^n) (terms1 as z (n+1))

We obtain

> ex2   ::  Fractional a => PowerSeries (Complex a)
> ex2    =  takePoly 10 (terms expx (0, 1))

< ex2    =  [(1.0,0.0),(0.0,1.0),(-0.5,0.0),(0.0,-0.16666666666666666),
<            (4.1666666666666664e-2,0.0),(0.0,8.333333333333333e-3),
<            (-1.3888888888888887e-3,0.0),(0.0,-1.9841269841269839e-4),
<            (2.4801587301587298e-5,0.0),(0.0,2.7557319223985884e-6)]

We can see that the real part of this series is the same as

> ex2R = takePoly 10 (coeffs cosx 1)

and the imaginary part is the same as

> ex2I = takePoly 10 (coeffs sinx 1)

(within approx 20 decimals).  But the terms of a series evaluated at 1
are the coefficients of the series.  Therefore, the terms of `cosx`
are

< [1, 0, -1/2!, 0, 1/4!, 0, -1/6!, ...]

i.e.

< a (2 * n) = (-1)^n / (2 * n)!,   a (2*n+1) = 0

and the terms of `sinx` are

< [0, 1, 0, -1/3!, 0, 1/5!, 0, -1/7!, ...]

i.e.

< a (2*n) = 0,  a(2*n+1) = (-1)^n / (2*n+1)!

This can be proven from the definitions of `cosx` and `sinx`.  From
this we obtain *Euler's formula*:

< e^(i*x) = cos x + i*sin x

One thing which comes out of Euler's formula is the fact that the
exponential is a *periodic function*.  A function `f : A -> B` is said
to be periodic if there exists `T ∈ A` such that

< `f a = f (a + T)    ∀ a ∈ A

(therefore, for this definition to make sense, we need addition on
`A`; in fact we normally assume at least group structure, i.e.,
addition and subtraction).

Since `sin` and `cos` are periodic, with period `2 * pi`, we have,
using the standard notation `a+i*b` for `(a, b)`

<   e^(a + i * b + i * 2*pi)

<  =

<   e^(a + i * (b + 2*pi))

<  =

<  e^a * e^(i * (b + 2 * pi))

<  =

<  e^a * (cos (b + 2 * pi) + i * sin (b + 2 * pi))

<  =

<  e^a * (cos b + i * sin b)

<  =

<  e^a * e^(i*b)

<  =

<  e^(a + i * b)


2.  Taylor series
-----------------

If `f = eval [a0, a1, ..., an, ...]`, then

<   f 0    =  a0
<   f'     =  eval (deriv [a0, a1, ..., an, ...])
<          =  eval ([a1, 2 * a2, 3 * a3, ..., n * an, ...])
<   =>
<   f' 0   =  a1
<   f''    =  eval (deriv [a1, 2 * a2, ..., n * an, ...])
<          =  eval ([2 * a2, 3 * 2 * a3, ..., n * (n - 1) * an, ...])
<   =>
<   f'' 0  =  2 * a2

In general:

<   f^(k) 0  =  fact k * ak

Therefore

<   f      =  eval [f 0, f' 0, f'' 0 / 2, ..., f^(n) 0 / (fact n), ...]

The series `[f 0, f' 0, f'' 0 / 2, ..., f^(n) 0 / (fact n), ...]` is
called the Taylor series centred in 0, or the Maclaurin series.

Therefore, if we can represent `f` as a power series, we can find the
value of all derivatives of `f` at 0!

> derivs :: Num a => PowerSeries a -> PowerSeries a
> derivs as = derivs1 as 0 1     -- series n n!
>   where
>   derivs1 (Cons a as) n factn  =
>           Cons (a * factn) (derivs1 as (n + 1) (factn * (n + 1)))
>   derivs1 (Single a) n factn   =  Single (a * factn)

> ex3 = takePoly 10 (derivs (x^3 + 2 * x))
> ex4 = takePoly 10 (derivs sinx)

What if we want the value of the derivatives at `a ≠ 0`?

We then need the power series of the "shifted" function g:

<  g x  =  f (x + a)  <=>  g = f . (+ a)

If we can represent g as a power series, say `[b0, b1, ...]`, then we
have

< g^(k) 0  =  fact k * bk  =  f^(k) a

In particular, we would have

< f x  =  g (x - a)  =  Σ bn * (x - a)^n

which is called the Taylor expansion of `f` at `a`.

Unfortunately, we cannot in general compute the power series
representation of `f ∘ (+a)`.  For example, we cannot apply the
procedure of Section 4 in Lecture 11 (Composition of power series),
since we only have the power series representation of `f`, instead of
a function of type `a -> a` extensible to `PowerSeries a ->
PowerSeries a`.

However, we can apply the same techniques we used for automatic
differentiation, and compute the Taylor expansions while constructing
the functions, in the same way we computed the derivatives while
calculating the functions.  Indeed, given `fs, gs :: a -> PowerSeries
a` which compute the Taylor coefficients of `f` and `g` at a given
`a`, respectively, so that `fs a = [f a, f' a, f'' a / 2, ...]` and
`gs a = [g a, g' a, g'' a / 2, ...]`, we have that `fs + gs`, `fs *
gs`, and `1 / fs` compute the Taylor coefficients of `f + g`, `f * g`,
and `1 / f` respectively.  We do not even have to give a separate `Num
(a -> PowerSeries a)` instance declaration: the structure induced by
`Num a => Num (x -> a)`, given in Lecture 6, is the correct one.  In
fact, we can take over the `Floating` instance above as well: the
language of power series induces the correct language for automatic
computation of all derivatives!

Example:

> idT   ::  Num a => a -> PowerSeries a
> idT a  =  Cons a (Single 1)

> testT0, testT1 :: (Eq a, Num a, Fractional a) => a -> PowerSeries a
> testT0 = idT^3
> testT1 = idT / (1 + idT)

In fact, we can go one (final!) step further, observing that in all
the computations we only use the value at a single `a`.  Therefore, as
we did in Lecture 9, we can simply use the structure on `PowerSeries
a` given by the instance declarations for `Num` and `Fractional`
above, and by the `Floating` definition partially given below.  The
interpretation of an element `fs :: PowerSeries a` is now that of the
Taylor coefficients at an implicit `a`.  More than just inducing the
language of automatic derivatives, we can now say that the language of
power series *is* the language of automatic derivatives.

