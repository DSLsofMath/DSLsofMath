> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeSynonymInstances #-}

> module Lecture11 where

> import Lecture10
> import FunNumInst

Lecture 11: Power Series and Differential Equations
===================================================

1. Integration of power series
------------------------------

We can specify the operation of integration on power series by

<        deriv (integ as) = as     ∀ as :: PowerSeries a

Putting `integ as = [b0, b1, ...]`, we obtain

<        deriv [b0, b1, ...] = [a0, a1, ...]
< <=  {def deriv}
<        b1      =  a0
<        2 * b2  =  a1
<        ...
<        n * bn  =  a_{n-1}
<        ...
< <=>  {arithmetic}
<        bn      =  a_{n-1} / n     ∀ n ≥ 1

Notice that:

- the integration operation requires a richer structure
  than derivation (we must have division)

- the value of `b0` is not determined; thus, it must be supplied
  separately.

> integ  ::  Fractional a => PowerSeries a -> a -> PowerSeries a
> integ  as b0  =  Cons b0 (integ' as 1)
>   where integ' (Single a) n   =  Single (a / n)
>         integ' (Cons a as) n  =  Cons (a / n) (integ' as (n+1))

The implementation accounts for polynomials as well.

2. Differential equations and the elementary functions
------------------------------------------------------

For convergent series, i.e., for series `as` for which there exist
`f = eval as`, `eval` is a homomorphism, i.e.

<      eval as  +  eval bs  =  eval (as + bs)
<      eval as  *  eval bs  =  eval (as * bs)

and so on, and moreover

<      eval (deriv as)       =  (eval as)'  -- the derivative of eval as
<      eval (integ bs f0) x  =  ∫_0^x (eval bs t) dt + f0

The last of these identities follows from

<      {specification of integ}
<      bs = deriv (integ bs f0)
< =>   {function}
<      eval bs = eval (deriv (integ bs f0))
< =>   {eval morphism}
<      eval bs = (eval (integ bs f0))'
< =>   {fundamental theorem of calculus}
<      ∫_0^x (eval bs t) dt =  eval (integ bs f0) x -
<                              eval (integ bs f0) 0
< =>   {def of integ and eval}
<      ∫_0^x (eval bs t) dt =  eval (integ bs f0) x - f0

Reminder: the fundamental theorem of calculus is

<     ∫_a^b (f' t) dt  =  f b  -  f a

Therefore, for a = 0 and b = x we have

<     ∫_0^x (f' t) dt  =  f x  -  f 0
< <=>  {arithmetic}
<     f x = ∫_0^x (f' t) dt + f 0


<     f'  =  g f,  f 0 = f0

The fundamental theorem of calculus then gives us

<     f x   =   ∫_0^x (g f t) dt + f0

a fix-point equation that can be difficult to solve.  However, if `f`
is representable as a power series `as` and if `g` is a polymorphic
function such that

<     g f   =  g (eval as)  =  eval (g as)

then we can transform the fix-point equation on functions in a
recursive equation on power series:

<     f x   =   ∫_0^x (g f t) dt + f0
< <=>  {f = eval as}
<     eval as x  =  ∫_0^x (g (eval as) t) dt + f0
< <=>  {g "commutes" with eval}
<     eval as x  =  ∫_0^x (eval (g as) t) dt + f0
< <=>  {eval-integ}
<     eval as x  =  eval (integ (g as) f0) x
< <=
<     as = integ (g as) f0

Let us consider some simple examples:

=== 1. The identity function.

We have

<  id x = x

therefore, the identity function is the solution to the differential
equation

<  f' x = 1, f 0 = 0

> idx  ::  Fractional a => PowerSeries a
> idx   =  integ 1 0     -- (1 :: PowerSeries a) == fromInteger (1::Integer)
> ids  ::  Fractional a => a -> a
> ids   =  eval 100 idx

=== 2.  The exponential function

The exponential function is characterised as being equal to its
derivative, with the value at 0 being 1:

< f = exp    <=>    f'  =  f ,  f 0 = 1

Therefore, we can define

> expx :: Fractional a => PowerSeries a
> expx = integ expx 1     -- note the recursive definiton

> exps :: Fractional a => a -> a
> exps = eval 100 expx

=== 3. The trigonometric functions


We have

< f = sin    <=>  f' = cos  ,  f 0 = 0
< g = cos    <=>  g' = -sin ,  g 0 = 1

> sinx, cosx :: Fractional a => PowerSeries a
> sinx = integ cosx 0
> sins = eval 100 sinx

> cosx = integ (-sinx) 1
> coss = eval 100 cosx

=== 4. The logarithm

Here we have a problem: the logarithm is not defined in 0.  But all
our power series are, moreover the value of eval as 0 = head as.  We
need to "shift" the function, implementing log . (+1) instead:

< g x = log (x + 1)   <=>  g' x = 1 / (x + 1)  ,  g 0 = 0
< f = log             <=>  f  x = g (x - 1)

> logx   ::  (Eq a, Fractional a) => PowerSeries a
> logx    =  integ (1 / (x + 1)) 0

> logs   ::  (Eq a, Fractional a) => a -> a
> logs x  =  eval 100 logx (x - 1)

Notice that `logs x` gives very poor results for `x > 2`. (The series
is converging very slowly so a 100 degree polynomial is still a poor
approximation.) We can improve the results by choosing a different
starting point, for example:

> logx'   =  integ (1 / (x + Single (exps 1))) 1
> logs' x  =  eval 100 logx' (x - exps 1)

In general, we will need to perform such shifts in order to ensure
accuracy, even when the series converges in ℝ.  The estimation of
errors, the choice of the number of terms to evaluate (here fixed at
100) and starting point, etc., are studied in the field of numerical
analysis.

=== 5. Computing general exponentiation.

Since we can compute exponentials and (natural) logarithms, we can
compute *any* exponentiation:

<       a^b
<  =    {def logarithm}
<       (e^log a)^b
<  =    {exponentials}
<       e^(b * log a)

Therefore:

> pows a b = exps (b * logs a)

=== A general solver

We can implement a generic solver for the differential equation

<  f' = g f  ,  f 0 = f0

> solve :: Fractional a => (PowerSeries a -> PowerSeries a) -> a -> PowerSeries a
> solve rhs f0 = f
>   where f = integ (rhs f) f0

For example

> expx' = solve (\ y -> y) 1
> exps' = eval 100 expx'

> logx'' = solve (\ y -> 1 / (x + 1)) 0
> logs'' x = eval 100 logx'' (x-1)

4. Composition of power series
------------------------------

Let `f :: A -> A` and `fs` its power series representation.
Let `g :: A -> A` extensible to power series in the sense that there
exists `gPS :: PowerSeries A -> PowerSeries A` such that

<    g (f x)  =  eval (gPS fs) x       ∀ x

When can we find `gPS`?

We have

<    gPS fs   =  integ (deriv gPS fs) (g (val fs))
< =>
<    gPS fs   =  integ (gPS' fs * deriv fs) (g (val fs))

We can write a recursive equation for `gPS` if we can provide an
extension of its derivative, `gPS'`.  There is no general way of doing
this (for an alternative explanation and treatment of composition see
@mcilroy1999functional), but we can do it in many interesting cases,
for example:

> val             ::  PowerSeries a -> a
> val (Single a)   =  a
> val (Cons a as)  =  a

<    expPS fs  =  integ (expPs fs * deriv fs) (exp (val fs))

<    sinPS fs  =  integ (cosPS fs * deriv fs) (sin (val fs))
<    cosPS fs  =  integ (-sinPS fs * deriv fs) (cos (val fs))

and so on.  Therefore, we can also give an instance declaration for
`Floating`:

> instance (Eq a, Floating a) => Floating (PowerSeries a) where
>   pi       =  Single pi
>   exp fs   =  integ (exp fs * deriv fs)  (exp (val fs))
>   sin fs   =  integ (cos fs * deriv fs)  (sin (val fs))
>   cos fs   =  integ (-sin fs * deriv fs) (cos (val fs))

Exercise: complete this instance declaration.

We now have a complete language for manipulating formal power series,
inspired by their semantics, but independent of it.


References
==========
