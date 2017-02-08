% Week 4-5: Poly, PowerSeries, ...

\section{Week 5: Polynomials and Power Series}
\begin{code}
module DSLsofMath.W05 where
\end{code}

\subsection{Preliminaries}

Last time, we defined a |Num| structure on pairs |(Double, Double)| by
requiring the operations to be compatible with the interpretation |(f
a, f' a)|.  For example

\begin{spec}
(x, x') * (y, y') = (x * y, x' * y + x * y')
\end{spec}

There is nothing in the ``nature'' of pairs of |Double| that forces
this definition upon us.
%
We chose it, because of the intended interpretation.

This multiplication is obviously not the one we need for \emph{complex
  numbers}:

\begin{spec}
(x, x') * (y, y') = (x * y - x' * y', x * y' + x' * y)
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
|evalPoly| looks like a homomorphism.  Question: is there a |Num|
structure on |Poly a|, such that |evalPoly| is a homomorphism?

For example, the homomorphism condition gives for |(+)|

\begin{spec}
evalPoly as + evalPoly bs = evalPoly (as + bs)
\end{spec}

Both sides are functions, they are equal iff they are equal for every
argument.
%
For an arbitrary |x|

\begin{spec}
  (evalPoly as + evalPoly bs) x = evalPoly (as + bs) x

<=> {- + on functions is point-wise -}

  evalPoly as x + evalPoly bs x = evalPoly (as + bs) x
\end{spec}

To proceed further, we need to consider the various cases in the
definition of |evalPoly|.
%
We give here the computation for the last case, using the traditional
list notation (:) for brevity.

\begin{spec}
evalPoly (a : as) x  +  evalPoly (b : bs) x =
evalPoly ((a : as)  +  (b : bs)) x
\end{spec}

For the left-hand side, we have:
%
\begin{spec}
  evalPoly (a : as) x  +  evalPoly (b : bs) x

=  {- def. |evalPoly| -}

  a + x * evalPoly as x + b + x * eval bs x

=  {- properties of |+|, valid in any ring -}

  a + b + x * (evalPoly as x + evalPoly bs x)

=  {- homomorphism condition -}

  a + b + x * evalPoly (as + bs)

=  {- def. |evalPoly| -}

  evalPoly ((a + b) : as + bs) x
\end{spec}

The homomorphism condition will hold for every |x| if we define

\begin{spec}
(a : as) + (b : bs) = (a + b) : (as + bs)
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

  fromInteger              =  Single . fromInteger

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
  For any finite ring A, there is a finite number of functions |A →
  A|, but there is a countable number of polynomials.
  %
  That means that the same polynomial function on |A| will be the
  evaluation of many different polynomials.

  For example, consider the ring |ℤ₂| (|{0, 1}| with addition and
  multiplication modulo |2|).
  %
  In this ring, we have

  \begin{spec}
    evalPoly [0, -1, 1] = const 0 = evalPoly [0]
  \end{spec}

  but

  \begin{spec}
    [0, -1, 1] ≠ [0]  in Poly ℤ₂
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

\section{Power Series}

TODO

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