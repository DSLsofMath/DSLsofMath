\section{Week 5: Polynomials and Power Series}
\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
module DSLsofMath.W05 where
\end{code}

\subsection{Preliminaries}

Last time, we defined a |Num| structure on pairs |(Double, Double)| by
requiring the operations to be compatible with the interpretation |(f
a, f' a)|.  For example

\begin{spec}
(x, x') *? (y, y') = (x * y, x' * y + x * y')
\end{spec}

There is nothing in the ``nature'' of pairs of |Double| that forces
this definition upon us.
%
We chose it, because of the intended interpretation.

This multiplication is obviously not the one we need for \emph{complex
  numbers}:

\begin{spec}
(x, x') *. (y, y') = (x * y - x' * y', x * y' + x' * y)
\end{spec}

Again, there is nothing in the nature of pairs that foists this
operation on us.
%
In particular, it is, strictly speaking, incorrect to say that a
complex number \emph{is} a pair of real numbers.
%
The correct interpretation is that a complex number can be
\emph{represented} by a pair of real numbers, provided we define the
operations on these pairs in a suitable way.

The distinction between definition and representation is similar to
the one between specification and implementation, and, in a certain
sense, to the one between syntax and semantics.
%
All these distinctions are frequently obscured, for example, because
of prototyping (working with representations / implementations /
concrete objects in order to find out what definition / specification
/ syntax is most adequate).
%
They can also be context-dependent (one man's specification is another
man's implementation).
%
Insisting on the difference between definition and representation can
also appear quite pedantic (as in the discussion of complex numbers
above).
%
In general though, it is a good idea to be aware of these
distinctions, even if they are suppressed for reasons of brevity or
style.

\subsection{Polynomials}

From \cite{adams2010calculus}, page 55:

\begin{quote}
A \textbf{polynomial} is a function $P$ whose value at $x$ is

\[P x = a_n x^n + a_{n-1} x^{n - 1} + ... a_1 x + a_0\]

where $a_n$, $a_{n-1}$, \ldots, $a_1$, and $a_0$, called the
\textbf{coefficients} of the polymonial [original spelling], are
constants and, if $n > 0$, then $a_n ≠ 0$.
%
The number $n$, the degree of the highest power of $x$ in the
polynomial, is called the \textbf{degree} of the polynomial.
%
(The degree of the zero polynomial is not defined.)
\end{quote}

This definition raises a number of questions, for example ``what is
the zero polynomial?''.

The types of the elements involved in the definition appear to be

\begin{quote}
  $P : ℝ → ℝ$, $x ∈ ℝ$, $a_0$, ... $a_n ∈ ℝ$ with $a_n ≠ 0$ if $n > 0$
\end{quote}

The phrasing should be ``whose value at \emph{any} $x$ is''.
%
The remark that the $a_i$ are constants is probably meant to indicate
that they do not depend on $x$, otherwise every function would be a
polynomial.
%
The zero polynomial is, according to this definition, the `const 0`
function.
%
Thus, what is meant is

\begin{quote}
  A \textbf{polynomial} is a function $P : ℝ → ℝ$ which is either
  constant zero, or there exist $a_0$, ..., $a_n$ ∈ ℝ with $a_n ≠ 0$
  such that, for any $x ∈ ℝ$

  \[P x = a_n x^n + a_{n-1} x^{n - 1} + ... a_1 x + a_0\]
\end{quote}

Obviously, given the coefficients $a_i$ we can evaluate $P$ at any
given $x$.
%
Assuming the coefficients are given as

\begin{spec}
as = [a0, a1, ..., an]
\end{spec}

(we prefer counting up), then the evaluation function is written

\begin{spec}
eval ::  [Real] ->  Real  ->  Real
eval     []         x     =   0
eval     (a : as)   x     =   a + x * eval as x
\end{spec}

Not every list is valid according to the definition.
%
In particular, the empty list is not a valid list of coefficients, so
we have a conceptual, if not empirical, type error in our evaluator.

The valid lists are those *finite* lists in the set

\begin{spec}
  {[0]} ∪ {(a : as) | last (a : as) ≠ 0}
\end{spec}

We cannot express the |last (a : as) ≠ 0| in Haskell, but we can
express the condition that the list should not be empty:

\begin{code}
data Poly a  =  Single a  |  Cons a (Poly a)
                deriving (Eq, Ord)
\end{code}

(TODO: show the version and motivation for using just |[a]| as well.
Basically, one can use |[]| as the syntax for the ``zero polynomial''
and |(c:cs)| for all other.)

The relationship between |Poly a| and |[a]| is given by the following
functions:

\begin{code}
toList :: Poly a   ->  [a]
toList (Single a)   =  a : []
toList (Cons a as)  =  a : toList as

fromList :: [a]         ->  Poly a
fromList (a : [])        =  Single a
fromList (a0 : a1 : as)  =  Cons a0 (fromList (a1 : as))

instance Show a => Show (Poly a) where
  show = show . toList
\end{code}

Since we only use the arithmetical operations, we can generalise our
evaluator:

\begin{code}
evalPoly :: Num a => Poly a -> a -> a
evalPoly (Single a)     x   =  a
evalPoly (Cons a as)    x   =  a + x * evalPoly as x
\end{code}

Since we have |Num a|, there is a |Num| structure on |a -> a|, and
|evalPoly| looks like a homomorphism.
%
Question: is there a |Num| structure on |Poly a|, such that |evalPoly|
is a homomorphism?

For example, the homomorphism condition gives for |(+)|
%
\begin{spec}
evalPoly as + evalPoly bs = evalPoly (as + bs)
\end{spec}

Both sides are functions, they are equal iff they are equal for every
argument.
%
For an arbitrary |x|

\begin{spec}
  (evalPoly as + evalPoly bs) x = evalPoly (as + bs) x

<=> {- |+| on functions is defined point-wise -}

  evalPoly as x + evalPoly bs x = evalPoly (as + bs) x
\end{spec}

To proceed further, we need to consider the various cases in the
definition of |evalPoly|.
%
We give here the computation for the last case (where |as| has at
least one |Cons|), using the traditional list notation |(:)| for
brevity.
%

\begin{spec}
evalPoly (a : as) x  +  evalPoly (b : bs) x  =  evalPoly ((a : as)  +  (b : bs)) x
\end{spec}

For the left-hand side, we have:
%
\begin{spec}
  evalPoly (a : as) x  +  evalPoly (b : bs) x

=  {- def. |evalPoly| -}

  (a + x * evalPoly as x) + (b + x * eval bs x)

=  {- properties of |+|, valid in any ring -}

  (a + b) + x * (evalPoly as x + evalPoly bs x)

=  {- homomorphism condition -}

  (a + b) + x * (evalPoly (as + bs) x)

=  {- def. |evalPoly| -}

  evalPoly ((a + b) : (as + bs)) x
\end{spec}

The homomorphism condition will hold for every |x| if we define

\begin{spec}
(a : as) + (b : bs)  = (a + b) : (as + bs)
\end{spec}

We leave the derivation of the other cases and operations as an
exercise.
%
Here, we just give the corresponding definitions.
%
\begin{code}
instance Num a => Num (Poly a) where
  (+) = polyAdd
  (*) = polyMul

  negate = polyNeg

  fromInteger =  Single . fromInteger

polyAdd :: Num a => Poly a -> Poly a -> Poly a
polyAdd (Single a )  (Single b )  =  Single (a + b)
polyAdd (Single a )  (Cons b bs)  =  Cons (a + b) bs
polyAdd (Cons a as)  (Single b )  =  Cons (a + b) as
polyAdd (Cons a as)  (Cons b bs)  =  Cons (a + b) (polyAdd as bs)

polyMul :: Num a => Poly a -> Poly a -> Poly a
polyMul (Single a )  (Single b )  =  Single (a * b)
polyMul (Single a )  (Cons b bs)  =  Cons (a * b) (polyMul (Single a) bs)
polyMul (Cons a as)  (Single b )  =  Cons (a * b) (polyMul as (Single b))
polyMul (Cons a as)  (Cons b bs)  =  Cons (a * b) (polyAdd  (polyMul as (Cons b bs))
                                                            (polyMul (Single a) bs)  )
polyNeg :: Num a => Poly a -> Poly a
polyNeg = fmap negate
\end{code}
%
Therefore, we \emph{can} define a ring structure (the mathematical
counterpart of |Num|) on |Poly a|, and we have arrived at the
canonical definition of polynomials, as found in any algebra book
(see, for example, \cite{rotman2006first} for a very readable text):

\begin{quote}
  Given a commutative ring |A|, the commutative ring given by the set
  |Poly A| together with the operations defined above is the ring of
  \textbf{polynomials} with coefficients in |A|.
\end{quote}

The functions |evalPoly as| are known as \emph{polynomial functions}.

\textbf{Caveat:} The canonical representation of polynomials in
algebra does not use finite lists, but the equivalent

\begin{quote}
  \begin{spec}
    Poly' A = { a : ℕ → A | {- |a| has only a finite number of non-zero values -} }
  \end{spec}
\end{quote}

Exercise: what are the ring operations on |Poly' A|?
%
For example, here is addition:

\begin{spec}
  a + b = c  <=>  a n + b n = c n  --  |∀ n : ℕ|
\end{spec}

\textbf{Observations:}

\begin{enumerate}
\item Polynomials are not, in general, isomorphic (in one-to-one
  correspondence) with polynomial functions.
  %
  For any finite ring |A|, there is a finite number of functions |A ->
  A|, but there is a countable number of polynomials.
  %
  That means that the same polynomial function on |A| will be the
  evaluation of many different polynomials.

  For example, consider the ring |ℤ₂| (|{0, 1}| with addition and
  multiplication modulo |2|).
  %
  In this ring, we have

  \begin{spec}
    evalPoly [0, 1, 1] = const 0 = evalPoly [0]  {- in |ℤ₂ -> ℤ₂| -}
  \end{spec}

  but

  \begin{spec}
    [0, 1, 1] ≠ [0]  {- in |Poly ℤ₂| -}
  \end{spec}

  Therefore, it is not generally a good idea to confuse polynomials
  with polynomial functions.

\item In keeping with the DSL terminology, we can say that the
  polynomial functions are the semantics of the language of
  polynomials.
  %
  We started with polynomial functions, we wrote the evaluation
  function and realised that we have the makings of a homomorphism.
  %
  That suggested that we could create an adequate language for
  polynomial functions.
  %
  Indeed, this turns out to be the case; in so doing, we have
  recreated an important mathematical achievement: the algebraic
  definition of polynomials.

Let

\begin{code}
x :: Num a => Poly a
x = Cons 0 (Single 1)
\end{code}

Then (again, using the list notation for brevity) for any polynomial
|as = [a0, a1, ..., an]| we have

\begin{spec}
as = a0 + a1 * x + a2 * x^2 + ... + an * x^n
\end{spec}

Exercise: check this.

This justifies the standard notation

\[as = \sum_{i = 0}^n a_i * x^i\]

\end{enumerate}

\subsection{Polynomial degree as a homomorphism}

TODO: textify black board notes

It is often the case that a certain function is \emph{almost} a
homomorphism and the domain or range \emph{almost} a monoid.
%
In the section on |eval| and |eval'| for |FunExp| we have seen
``tupling'' as one way to fix such a problem and here we will
introduce another way.

The |degree| of a polynomial is a good candidate for being a
homomorphism: if we multiply two polynomials we can normally add their
degrees.
%
If we try to check that |degree :: Poly a -> Nat| is the function
underlying a monoid morphism we need to decide on the monoid structure
to use for the source and for the target, and we need to check the
homomorphism laws.
%
We can use |unit = Single 1| and |op = polyMul| for the source monoid
and we can try to use |unit = 0| and |op = (+)| for the target monoid.
%
Then we need to check that
%
\begin{spec}
degree (Single 1) = 0
∀ x, y? degree (x `op` y) = degree x  +  degree y
\end{spec}
%
The first law is no problem and for most polynomials the second law is
also straighforward to prove (exercise: prove it) except for one
special case: the zero polynomial.

Looking back at the definition from \cite{adams2010calculus}, page 55
it says that the degree of the zero polynomial is not defined.
%
Let's see why that is the case and how we might ``fix'' it.
%
Assume there is a |z| such that |degree 0 = z| and that we have some
polynomial |p| with |degree p = n|.
%
Then we get
%
\begin{spec}
  z
= {- assumption -}
  degree 0
= {- simple calculation -}
  degree (0 * p)
= {- homomorphism condition -}
  degree 0 + degree p
= {- assumption -}
  z + n
\end{spec}
%
Thus we need to find a |z| such that |z = z + n| for all natural
numbers |n|!
%
At this stage we could either give up, or think out of the box.
%
Intuitively we could try to use |z = -Infinity|, which would seem to
satisfy the law but which is not a natural number.
%
More formally what we need to do is to extend the monoid |(Nat,0,+)|
by one more element.
%
In Haskell we can do that using the |Maybe| type constructor:

%{
%format Monoid' = Monoid
\begin{code}
class Monoid' a where
  unit  :: a
  op    :: a -> a -> a

instance Monoid' a => Monoid' (Maybe a) where
  unit  = Nothing
  op    = opMaybe

opMaybe Nothing    m          = m
opMaybe m          Nothing    = m
opMaybe (Just m1)  (Just m2)  = Just (op m1 m2)
\end{code}
%}

We quote the Haskell prelude implementation:
% https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.Base.html#line-314
\begin{quote}
  Lift a semigroup into |Maybe| forming a |Monoid| according to
  \url{http://en.wikipedia.org/wiki/Monoid}: "Any semigroup |S| may be
  turned into a monoid simply by adjoining an element |e| not in |S|
  and defining |e*e = e| and |e*s = s = s*e| for all |s ∈ S|." Since
  there is no |Semigroup| typeclass [..], we use |Monoid| instead.
\end{quote}

Thus, to sum up, |degree| is a monoid homomorphism from |(Poly a, 1,
*)| to |(Maybe Nat, Nothing, opMaybe)|.

TODO: check all the properties.

\section{Power Series}

Power series are obtained from polynomials by removing in |Poly'| the
restriction that there should be a \emph{finite} number of non-zero
coefficients; or, in, the case of |Poly|, by going from lists to
streams.

\begin{spec}
PowerSeries' a = { f : ℕ → a }
\end{spec}

\begin{code}
type PowerSeries a = Poly a   -- finite and infinite non-empty lists
\end{code}

The operations are still defined as before.
%
If we consider only infinite lists, then only the equations which do
not contain the patterns for singleton lists will apply.

Power series are usually denoted

\[   \sum_{n = 0}^{\infty} a_n * x^n   \]

the interpretation of |x| being the same as before.

The evaluation of a power series represented by |a : ℕ → A| is defined,
in case the necessary operations make sense on |A|, as a function

\begin{spec}
eval a : A -> A
eval a x  =  lim s   where   s n = {-" \sum_{i = 0}^n a_i * x^i "-}
\end{spec}

Note that |eval a| is, in general, a partial function (the limit might
not exist).

We will consider, as is usual, only the case in which |A = ℝ| or |A =
ℂ|.

The term \emph{formal} refers to the independence of the definition of
power series from the ideas of convergence and evaluation.
%
In particular, two power series represented by |a| and |b|, respectively,
are equal only if |a = b| (as functions).
%
If |a ≠ b|, then the power series are different, even if |eval a =
eval b|.

Since we cannot in general compute limits, we can use an
``approximative'' |eval|, by evaluating the polynomial resulting from
an initial segment of the power series.

\begin{code}
eval :: Num a => Integer -> PowerSeries a -> (a -> a)
eval n as x = evalPoly (takePoly n as) x

takePoly :: Integer -> PowerSeries a -> Poly a
takePoly n (Single a)   =  Single a
takePoly n (Cons a as)  =  if n <= 1
                              then  Single a
                              else  Cons a (takePoly (n-1) as)
\end{code}
%
Note that |eval n| is not a homomorphism: for example |eval 2 (x*x) 1
= 0 /= 1 = 1*1 = (eval 2 x 1) * (eval 2 x 1)|.


\section{Operations on power series}

Power series have a richer structure than polynomials.
%
For example, we also have division (this is similar to the move from |ℤ|
to |ℚ|).
%
Assume that |a * b ≠ 0|.
%
Then (again, using list notation for brevity), we want to find, for
any given |(a : as)| and |(b : bs)|, the series |(c : cs)| satisfying

\begin{spec}
  (a : as) / (b : bs) = (c : cs)

<=> {- def. of division -}

  (a : as) = (c : cs) * (b : bs)

<=> {- def. of |*| for |Cons| -}

  (a : as) = (c * b)  :  (cs * (b : bs)  +  [c]*bs)

<=> {- equality on compnents, def. of division -}

  c   = a / b    {- and -}
  as  = cs * (b : bs) + [a/b] * bs

<=> {- arithmetics -}

  c   = a / b    {- and -}
  cs  =  (as - [a/b] * bs) / (b : bs)
\end{spec}

This leads to the implementation:

\begin{code}
instance (Eq a, Fractional a) => Fractional (PowerSeries a) where
  (/) = divPS
  fromRational =  Single . fromRational

divPS :: (Eq a, Fractional a) => PowerSeries a -> PowerSeries a -> PowerSeries a
divPS as           (Single b)    =  as * Single (1 / b)
divPS (Single 0)   (Cons b bs)   =  Single 0
divPS (Single a)   (Cons b bs)   =  divPS (Cons a (Single 0)) (Cons b bs)
divPS (Cons a as)  (Cons b bs)   =  Cons c  (divPS (as - (Single c) * bs) (Cons b bs))
                                    where  c = a / b

\end{code}

The first two equations allow us to also use division on polynomials,
but the result will, in general, be a power series, not a polynomial.
%
The first one should be self-explanatory.
%
The second one extends a constant polynomial, in a process similar to
that of long division.

For example:

\begin{code}
ps0, ps1, ps2 :: (Eq a, Fractional a) => PowerSeries a
ps0  = 1 / (1 - x)
ps1  = 1 / (1 - x)^2
ps2  = (x^2 - 2 * x + 1) / (x - 1)
\end{code}
%
Every |ps| is the result of a division of polynomials: the first two
return power series, the third is a polynomial (almost: it has a
trailing |0.0|).

\begin{code}
example0   = takePoly 10 ps0
example01  = takePoly 10 (ps0 * (1-x))
\end{code}

\section{Formal derivative}

Considering the analogy between power series and polynomial functions
(via polynomials), we can define a formal derivative for power series
according to the formula

\[(\sum_{n = 0}^{\infty} a_n * x^n)'  =
  \sum_{n = 0}^{\infty} (a_n * x^n)'  =
  \sum_{n = 0}^{\infty} (a_n * (n * x^{n-1}))  =
  \sum_{n = 0}^{\infty} ((n * a_n) * x^{n-1}) \]

We can implement this, for example, as

\begin{code}
deriv (Single a)   =  Single 0
deriv (Cons a as)  =  deriv' as 1
  where  deriv' (Single a)   n  =  Single  (n * a)
         deriv' (Cons a as)  n  =  Cons    (n * a)  (deriv' as (n+1))
\end{code}

Side note: we cannot in general implement a Boolean equality test for
PowerSeries.
%
For example, we know that |deriv ps0| equals |ps1| but we cannot
compute |True| in finite time by comparing the coefficients of the two
power series.

\begin{code}
checkDeriv :: Integer -> Bool
checkDeriv n  =  takePoly n (deriv ps0) == takePoly n ps1
\end{code}


Recommended reading: the Functional pearl: ``Power series, power
serious'' \cite{mcilroy1999functional}.


% ================================================================

\section{Signals and Shapes}

Shallow and deep embeddings of a DSL

TODO: perhaps textify DSL/




\section{Helpers}

\begin{code}
instance Functor Poly where
  fmap = fmapPoly

fmapPoly :: (a->b) -> (Poly a -> Poly b)
fmapPoly f (Single a)   = Single (f a)
fmapPoly f (Cons a as)  = Cons (f a) (fmapPoly f as)

po1 :: Num a => Poly a
po1 = 1 + x^2 - 3*x^4
\end{code}
