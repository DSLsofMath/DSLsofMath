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

Again, there is nothing in the nature of pairs that foisters this
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

We cannot express the precise type constraints in Haskell, but we can
introduce a type synonim to remind us of them:

> type Poly a = [a]

and, since we only use the arithmetical operations, we can generalise
our evaluator:

> evalPoly                  ::  Num a => Poly a -> a -> a
> evalPoly [a0]           x  =  a0
> evalPoly (a0 : a1 : as) x  =  a0 + x * evalPoly (a1 : as) x

Since we have Num a, there is a Num structure on `a -> a`, and
`evalPoly` looks like a homomorphism.  Question: is there a Num
structure on `Poly a`, such that `evalPoly` is a homomorphims?

For example, the homomorphism condition gives for `(+)`

<  evalPoly as + evalPoly bs = evalPoly (as + bs)

Both sides are functions, they are equal iff they are equal for every
argument.  For an arbitrary `x`

<  (evalPoly as + evalPoly bs) x = evalPoly (as + bs) x

<<=> {+ on functions is point-wise}

<  evalPoly as x + evalPoly bs x = evalPoly (as + bs) x
 
To proceed further, we need to consider the various cases in the
definition of `evalPoly`.  We give here the computation for the last
case:

<  evalPoly (a0 : a1 : as) x  +  evalPoly (b0 : b1 : bs) x =
<  evalPoly ((a0 : a1 : as)  +  (b0 : b1 : bs))

For the left-hand side, we have:

<  evalPoly (a0 : a1 : as) x  +  evalPoly (b0 : b1 : bs) x

<=  {def evalPoly}

<  a0 + x * evalPoly (a1 : as) x + b0 + x * eval (b1 : bs) x

<=  {properties of +, valid in any ring}

<  a0 + b0 + x * (evalPoly (a1 : as) x + evalPoly (b1 : bs) x)

<=  {homomorphism condition}

<  a0 + b0 + x * evalPoly ((a1 : as) + (b1 : bs))

<=  {def evalPoly}

<  evalPoly ((a0 + b0) : (a1 : as) + (b1 : bs)) x

The homomorphism condition will hold for every `x` if we can satisfy

<  (a0 : a1 : as) + (b0 : b1 : bs) = (a0 + b0) : ((a1 : as) + (b1 : bs))

We leave the derivation of the other cases and operations as an
exercise.  Here, we just give the definitions of multiplication and
fromInteger

> instance Num a => Num [a] where
>   (*) = mul
>   (+) = plus
>   fromInteger n = [fromInteger n]

> mul (a : as) (b : bs) = (a*b) : mulRest a as b bs
> mulRest _  []         _  []          =  []
> mulRest a  []         _  (b1 : bs)   =  (a*b1):mulRest a [] b1 bs
> mulRest _  (a1 : as)  b  []          =  (a1*b):mulRest a1 as b []
> mulRest a  (a1 : as)  b  (b1 : bs)   =  plus ((a1*b):mulRest a1 as b (b1 : bs))
>                                              ((a*b1):mulRest a [] b1 bs)

> plus (a:as) (b:bs) = (a+b) : plusRest as bs
> plusRest [] bs              =  bs
> plusRest as []              =  as
> plusRest (a : as) (b : bs)  =  (a + b) : plusRest as bs

Therefore, we *can* define a ring structure (the mathematical
counterpart of Num) on `Poly a`, and we have arrived at the canonical
definition of polynomials, as found in any algebra book (see, for
example, @rotman2006first for a very readable text):

 > Given a commutative ring A, the commutative ring given by the set
 > `Poly A` together with the operations defined above is the ring of
 > **polynomials** with coefficients in A.

The functions `evalPoly as` are known as *polynomial functions*.

**Caveat:** The canonical representation of polynomials in algebra
  does not use finite list, but the equivalent

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

<  x = [0, 1]

Then, for any polynomial `as = [a0, a1, ..., an]` we have

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

> type PowerSeries a = [a] -- finite or infinite lists

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
which in our stream notation is given by

> eval n as x = evalPoly (take n as) x

3. Operations on power series
-----------------------------

Power series have a richer structure than polynomials.  For example,
we also have division.  Assume that a0 * b0 ≠ 0.  Then

<  (a0 : a1 : as) / (b0 : b1 : bs) = c0 : c1 : cs

<<=>

<  (a0 : a1 : as) = (c0 : c1 : cs) * (b0 : b1 : bs)

<<=>

<  (a0 : a1 : as) = c0 * b0 : ((c1 : cs) * (b0 : b1 : bs) +
<                              [c0]*(b1 : bs))

<<=>

<  c0 = a0 / b0 and
<  (a1 : as) = (c1 : cs) * (b0 : b1 : bs) + [a0/b0] * (b1 : bs)

<<=>

<  c0 = a0 / b0 and
<  (c1 : cs) =  ((a1 : as) - [a0/b0] * (b1 : bs)) / (b0 : b1 : bs)
 
> instance (Eq a, Fractional a) => Fractional (PowerSeries a) where
>   (x : xs) / (y : ys)  =  if y == 0 && x == 0
>                           then xs / ys
>                           else     let q = x / y
>                                    in  q : (xs - [q] * ys) / (y : ys)
>   fromRational         =  undefined

4. Formal derivation
--------------------

Considering the analogy between power series and polynomial functions
(via polynomials), we can define a formal derivative for power series
according to the formula

<    (Σ a_n * X^n)'  =  Σ (n * a_n * X^(n-1))

We can implement this, for example, as

> deriv (a : as) = zipWith (*) [1 ..] as


> zeros = 0 : zeros
> x = 0 : 1 : zeros

> test0, test1, test2 :: PowerSeries Double
> test0 = 1 / (1 - x)
> test1 = 1 / (1 - x)^2
> test2 = (1 - 2 * x ^ 2) ^ 3
> test3 = zeros * x

----------------

Try evaluating test3 with "too eager" definition of plus:

> mul1 [a0]           [b0]             =  [a0 * b0]
> mul1 [a0]           (b0 : b1 : bs)   =  (a0 * b0) : mul1 [a0] (b1 : bs)
> mul1 (a0 : a1 : as) [b0]             =  (a0 * b0) : mul1 (a1 : as) [b0]
> mul1 (a0 : a1 : as) (b0 : b1 : bs)   =  (a0 * b0) : plus
>                                           (mul1 (a1 : as) (b0 : b1 : bs))
>                                           (mul1 [a0] (b1 : bs))

> plus1 [a0] [b0]                       =  [a0 + b0]
> plus1 [a0] (b0 : b1 : bs)             =  (a0 + b0) : b1 : bs
> plus1 (a0 : a1 : as) [b0]             =  (a0 + b0) : a1 : as
> plus1 (a0 : a1 : as) (b0 : b1 : bs)   =  (a0 + b0) : plus1 (a1 : as) (b1 : bs)

> test3eval =
>   [ zeros * x
>   , -- Def. of (+) for Poly
>     mul1 zeros x
>   , -- mul1 does two level deep pattern matching on both arguments
>     mul1 (0:0:zeros) (0:1:zeros)
>   , -- 4:th case matches
>     let a0 : a1 : as = 0:0:zeros
>         b0 : b1 : bs = 0:1:zeros
>     in (a0 * b0) : plus1 (mul1 (a1 : as) (b0 : b1 : bs)) (mul1 [a0] (b1 : bs))
>   , -- Substitutions: a0 = a1 = b0 = 0; b1 = 1; as = bs = zeros
>     (0 * 0) : plus1 (mul1 (0 : zeros) (0 : 1 : zeros)) (mul1 [0] (1 : zeros))
>   , -- plus1 matches two levels deep on both args => mul1 matches two levels deep again (twice)
>     (0 * 0) : plus1 (mul1 (0 : zeros) (0 : 1 : zeros)) (mul1 [0] (1 : zeros))
>   , -- plus1 matches two levels deep on both args => mul1 matches two levels deep again (twice)
>     0 : plus1 (mul1 (0:0:zeros) (0:1:zeros)) (mul1 [0] (1:0:zeros))
>   , -- 1:st arg to plus1 is the same as what we try to evaluate (test3). (2:nd arg is zeros by separate lemma)
>     0 : plus1 test3 zeros
>   , -- Because plus1 matches two deep we end up with an infinite recursion
>     let ys = 0 : plus1 ys zeros
>     in ys
>   ]

> lemmaMul10 x y =
>   [ mul1 [0] (x:y:zeros)
>   , -- def. of mul1: 2:nd case
>     let [a0]           = [0]
>         (b0 : b1 : bs) = x:y:zeros
>     in (a0 * b0) : mul1 [a0] (b1 : bs)
>   , -- Substitute: a0 = 0, b0 = x, b1 = y, bs = zeros, Simplify
>     0 : mul1 [0] (y : zeros)
>   , -- Same procedure
>     0 : 0 : mul1 [0] zeros
>   , -- Note the pattern (use induction for proper proof)
>     zeros
>   ]
