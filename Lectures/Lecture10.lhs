> {-# LANGUAGE TypeSynonymInstances #-}

Lecture 10: Polynomials and Power Series
========================================

0. Preliminaries
----------------

Last time, we defined a Num structure on pairs (Double, Double) by
requiring the operations to be compatible with the interpretation (f
a, f' a).  For example

< (x, x') * (y, y') = (x * y, x' * y + x * y')

There is nothing in the "nature" of pairs of Double that forces this
definition upon us.  We chose it, because of the intended
interpretation.

This multiplication is obviously not the one we need for *complex
numbers*:

< (x, x') * (y, y') = (x * y - x' * y', x * y' + x' * y)

Again, there is nothing in the nature of pairs that foists this
operation on us.  In particular, it is, strictly speaking, incorrect
to say that a complex number *is* a pair of real numbers.  The correct
interpretation is that a complex number can be *represented* by a pair
of real numbers, provided we define the operations on these pairs in a
suitable way.

The distinction between definition and representation is similar to
the one between specification and implementation, and, in a certain
sense, to the one between syntax and semantics.  All these
distinctions are frequently obscured, for example, because of
prototyping (working with representations/implementations/concrete
objects in order to find out what definition/specification/syntax is
most adequate).  They can also be context-dependent (one man's
specification is another man's implementation).  Insisting on the
difference between definition and representation can also appear quite
pedantic (as in the discussion of complex numbers above).  In general
though, it is a good idea to be aware of these distinctions, even if
they are suppressed for reasons of brevity or style.

1. Polynomials
--------------

From @adams2010calculus, page 55:

  > A **polynomial** is a function $P$ whose value at $x$ is

   $P x = a_n x^n + a_{n-1} x^{n - 1} + ... a_1 x + a_0$

  > where $a_n$, $a_{n-1}$, ..., $a_1$, and $a_0$, called the
  > **coefficients** of the polymonial [original spelling], are
  > constants and, if $n > 0$, then $a_n ≠ 0$.  The number $n$, the
  > degree of the highest power of $x$ in the polynomial, is called
  > the **degree** of the polynomial. (The degree of the zero
  > polynomial is not defined.)

This definition raises a number of questions, for example "what is the
zero polynomial?".

The types of the elements involved in the definition appear to be

  > $P : ℝ → ℝ$, $x ∈ ℝ$, $a_0$, ... $a_n ∈ ℝ$ with $a_n ≠ 0$ if $n > 0$

The phrasing should be "whose value at *any* $x$ is".  The remark
that the $a_i$ are constants is probably meant to indicate that they
do not depend on $x$, otherwise every function would be a polynomial.
The zero polynomial is, according to this definition, the `const 0`
function.  Thus, what is meant is

  > A **polynomial** is a function $P : ℝ → ℝ$ which is either constant zero,
  > or there exist $a_0$, ..., $a_n$ ∈ ℝ with $a_n ≠ 0$ such that, for
  > any $x ∈ ℝ$

  > $P x = a_n x^n + a_{n-1} x^{n - 1} + ... a_1 x + a_0$


Obviously, given the coefficients $a_i$ we can evaluate $P$ at any
given $x$.  Assuming the coefficients are given as

<      as = [a0, a1, ..., an]

(we prefer counting up), then the evaluation function is written

<      eval   :: [Real] -> Real -> Real
<      eval []  x       =  0
<      eval (a : as) x  =  a + x * eval as x

Not every list is valid according to the definition.  In particular,
the empty list is not a valid list of coefficients, so we have a
conceptual, if not empirical, type error in our evaluator.

The valid lists are those *finite* lists in the set

<      {[0]} ∪ {(a : as) | last (a : as) ≠ 0}

We cannot express the "last (a : as) ≠ 0" in Haskell, but we can
express the condition that the list should not be empty:

> data Poly a  =  Single a  |  Cons a (Poly a)
>                 deriving (Eq, Ord)

The relationship between `Poly a` and `[a]` is given by the following
functions:

> toList :: Poly a -> [a]
> toList (Single a)   =  [a]
> toList (Cons a as)  =  a : toList as

> fromList :: [a] -> Poly a
> fromList [a] = Single a
> fromList (a0 : a1 : as) = Cons a0 (fromList (a1 : as))

> instance Show a => Show (Poly a) where
>   show = show . toList

Since we only use the arithmetical operations, we can generalise our
evaluator:

> evalPoly ::  Num a => Poly a -> a -> a
> evalPoly (Single a)     x   =  a
> evalPoly (Cons a as)    x   =  a + x * evalPoly as x

Since we have Num a, there is a Num structure on `a -> a`, and
`evalPoly` looks like a homomorphism.  Question: is there a Num
structure on `Poly a`, such that `evalPoly` is a homomorphism?

For example, the homomorphism condition gives for `(+)`

<  evalPoly as + evalPoly bs = evalPoly (as + bs)

Both sides are functions, they are equal iff they are equal for every
argument.  For an arbitrary `x`

<  (evalPoly as + evalPoly bs) x = evalPoly (as + bs) x

<<=> {+ on functions is point-wise}

<  evalPoly as x + evalPoly bs x = evalPoly (as + bs) x
 
To proceed further, we need to consider the various cases in the
definition of `evalPoly`.  We give here the computation for the last
case, using the traditional list notation (:) for brevity.

<  evalPoly (a : as) x  +  evalPoly (b : bs) x =
<  evalPoly ((a : as)  +  (b : bs))

For the left-hand side, we have:

<  evalPoly (a : as) x  +  evalPoly (b : bs) x

<=  {def evalPoly}

<  a + x * evalPoly as x + b + x * eval bs x

<=  {properties of +, valid in any ring}

<  a + b + x * (evalPoly as x + evalPoly bs x)

<=  {homomorphism condition}

<  a + b + x * evalPoly (as + bs)

<=  {def evalPoly}

<  evalPoly ((a + b) : as + bs) x

The homomorphism condition will hold for every `x` if we define

<  (a : as) + (b : bs) = (a + b) : (as + bs)

We leave the derivation of the other cases and operations as an
exercise.  Here, we just give the corresponding definitions.

> instance Num a => Num (Poly a) where
>   Single a   +  Single b   =  Single (a + b)
>   Single a   +  Cons b bs  =  Cons (a + b) bs
>   Cons a as  +  Single b   =  Cons (a + b) as
>   Cons a as  +  Cons b bs  =  Cons (a + b) (as + bs)
>
>   Single a   *  Single b   =  Single (a * b)
>   Single a   *  Cons b bs  =  Cons (a * b) (Single a * bs)
>   Cons a as  *  Single b   =  Cons (a * b) (as * Single b)
>   Cons a as  *  Cons b bs  =  Cons (a * b) (as * Cons b bs + Single a * bs)
>
>   negate (Single a)        =  Single (negate a)
>   negate (Cons a as)       =  Cons (negate a) (negate as)
>
>   fromInteger              =  Single . fromInteger


Therefore, we *can* define a ring structure (the mathematical
counterpart of Num) on `Poly a`, and we have arrived at the canonical
definition of polynomials, as found in any algebra book (see, for
example, @rotman2006first for a very readable text):

 > Given a commutative ring A, the commutative ring given by the set
 > `Poly A` together with the operations defined above is the ring of
 > **polynomials** with coefficients in A.

The functions `evalPoly as` are known as *polynomial functions*.

**Caveat:** The canonical representation of polynomials in algebra
  does not use finite lists, but the equivalent

  > `Poly' A` = { a : ℕ → A | a has only a finite number of non-zero values }

Exercise: what are the ring operations on `Poly' A`?  For example,
here is addition:

<          a + b = c  <=>  a n + b n = c n    ∀ n : ℕ

**Observations:**

  a) Polynomials are not, in general, isomorphic (in one-to-one
  correspondence) with polynomial functions.  For any finite ring A,
  there is a finite number of functions |A → A|, but there is a
  countable number of polynomials.  That means that the same
  polynomial function on A will be the evaluation of many different
  polynomials.

  > For example, consider the ring ℤ₂ ({0, 1} with addition and
    multiplication modulo 2).  In this ring, we have

<    evalPoly [0, -1, 1] = const 0 = evalPoly [0]

  > but

<    [0, -1, 1] ≠ [0]  in Poly ℤ₂

  > Therefore, it is not generally a good idea to confuse polynomials
    with polynomial functions.

  b) In keeping with the DSL terminology, we can say that the
  polynomial functions are the semantics of the language of
  polynomials.  We started with polynomial functions, we wrote the
  evaluation function and realised that we have the makings of a
  homomorphism.  That suggested that we could create an adequate
  language for polynomial functions.  Indeed, this turns out to be the
  case; in so doing, we have recreated an important mathematical
  achievement: the algebraic definition of polynomials.

Let

> x = Cons 0 (Single 1)

Then (again, using the list notation for brevity) for any polynomial
`as = [a0, a1, ..., an]` we have

<  as = a0 + a1 * x + a2 * x^2 + ... + an * x^n

Exercise: check this.

This justifies the standard notation

<  as = Σ_{i = 0}^n a_i * x^i

2. Power series
----------------------

Power series are obtained from polynomials by removing in `Poly'` the restriction
that there should be a finite number of non-zero coefficients; or, in,
the case of `Poly`, by going from lists to streams.

  PowerSeries' a = { f : ℕ → a }

> type PowerSeries a = Poly a -- finite and infinite non-empty lists

The operations are still defined as before.  If we consider only
infinite lists, then only the equations which do not contain the
patterns for singleton lists will apply.

Power series are usually denoted

    Σ_{n = 0}^{\infty} a_n * x^n

the interpretation of `x` being the same as before.

The evaluation of a power series represented by a : ℕ → A is defined,
in case the necessary operations make sense on A, as a function

<   eval a : A -> A
<   eval a x  =  lim s where s n = Σ_{i = 0}^n (a i) * x^i

eval a is, in general a partial function (the limit might not exist).

We will consider, as is usual, only the case in which A = ℝ or A = ℂ.

The term *formal* refers to the independence of the definition of
power series from the ideas of convergence and evaluation.  In
particular, two power series represented by a and b, respectively, are
equal only if a = b (as functions).  If a ≠ b, then the power series
are different, even if eval a = eval b.

Since we cannot compute limits, we can use an "approximative" `eval`,
by evaluating the polynomial resulting from an initial segment of the
power series.

> eval n as x = evalPoly (takePoly n as) x

> takePoly :: Integer -> Poly a -> Poly a
> takePoly n (Single a)   =  Single a
> takePoly n (Cons a as)  =  if n <= 1
>                               then  Single a
>                               else  Cons a (takePoly (n-1) as)

3. Operations on power series
-----------------------------

Power series have a richer structure than polynomials.  For example,
we also have division (this is similar to the move from ℤ to ℚ).
Assume that a * b ≠ 0.  Then (again, using list notation for brevity),
we want to find, for any given (a : as) and (b : bs), the series (c :
cs) satisfying

<  (a : as) / (b : bs) = c : cs

<<=>

<  (a : as) = (c : cs) * (b : bs)

<<=>

<  (a : as) = c * b : (cs * (b : bs) +
<                              [c]*bs)

<<=>

<  c = a / b and
<  as = cs * (b : bs) + [a/b] * bs

<<=>

<  c = a / b and
<  cs =  (as - [a/b] * bs) / (b : bs)

This leads to the implementation:

> instance (Eq a, Fractional a) => Fractional (PowerSeries a) where
>   as / Single b           =  as * Single (1 / b)
>   Single a / Cons b bs    =  if a == 0 then Single 0 else Cons a (Single 0) / Cons b bs
>   Cons a as / Cons b bs   =  let  q = a / b
>                              in   Cons q  ((as - Single q * bs) / Cons b bs)
>   fromRational         =  undefined

The first two equations allow us to also use division on polynomials,
but the result will, in general, be a power series, not a polynomial.
The first one should be self-explanatory.  The second one extends a
constant polynomial, in a process similar to that of long division.

For example:

> test0, test1, test2 :: PowerSeries Double
> test0 = 1 / (1 - x)
> test1 = 1 / (1 - x)^2
> test2 = (x^2 - 2 * x + 1) / (x - 1)

Every test is the result of a division of polynomials: the first two
return power series, the third is a polynomial (almost: it has a
trailing 0.0).

> x :: Num a => Poly a -- defined earlier

4. Formal derivative
--------------------

Considering the analogy between power series and polynomial functions
(via polynomials), we can define a formal derivative for power series
according to the formula

<    (Σ a_n * X^n)'  =  Σ (n * a_n * X^(n-1))

We can implement this, for example, as

> deriv (Single a)   =  Single 0
> deriv (Cons a as)  =  deriv' as 1
>                       where deriv' (Single a)  n  =  Single (n * a)
>                             deriv' (Cons a as) n  =  Cons (n * a) (deriv' as (n+1))

Side note: we cannot in general implement a Boolean equality test for
PowerSeries. For example, we know that `deriv test0` equals `test1`
but we cannot compute `True` in finite time by comparing the
coefficients of the two power series.
