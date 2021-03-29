\chapter{Polynomials and Power Series}
\label{sec:poly}
%if False
\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
module DSLsofMath.W05 where
import Prelude hiding (Num(..),(/),(^))
import DSLsofMath.Algebra
type REAL = Double
\end{code}
%endif

\section{Polynomials}

Again we take as starting point a definition from
  \cite{adams2010calculus}, this time from page 39:

\begin{quote}
A \textbf{polynomial} is a function $P$ whose value at $x$ is

\[P(x) = a_n x^n + a_{n-1} x^{n - 1} + \cdots + a_1 x + a_0\]

where $a_n$, $a_{n-1}$, \ldots, $a_1$, and $a_0$, called the
\textbf{coefficients} of the polymonial [\textit{sic}], are
constants and, if $n > 0$, then $a_n ≠ 0$.
%
The number $n$, the degree of the highest power of $x$ in the
polynomial, is called the \textbf{degree} of the polynomial.
%
(The degree of the zero polynomial is not defined.)
\end{quote}

This definition raises a number of questions, for example ``what is
the zero polynomial?'' (and why isn't its degree defined).

The types of the elements involved in the definition appear to be
%
\begin{quote}
  $P : ℝ → ℝ$; $x : ℝ$; $n : ℕ$; $a_0$, \ldots, $a_n : ℝ$ with $a_n ≠ 0$ if $n > 0$
\end{quote}
%
The phrasing should be ``whose value at \emph{any} $x$ is''.
%
The remark that the $a_i$ are constants is probably meant to indicate
that they do not depend on $x$, otherwise every function would be a
polynomial.
%
The zero polynomial is, according to this definition, the |const 0|
function.
%
Thus, what is meant is
%
\begin{quote}
  A \textbf{polynomial} is a function $P : ℝ → ℝ$ which either is the
  constant zero function, or there exist $a_0$, \ldots, $a_n$ : ℝ with
  $a_n ≠ 0$ (called \textbf{coefficients}) such that, for every $x : ℝ$

  \[P(x) = a_n x^n + a_{n-1} x^{n - 1} + \cdots + a_1 x + a_0\]
  For the constant zero polynomial the degree is not defined.
  Otherwise, the degree is $n$.
\end{quote}
Given the coefficients $a_i$ we can evaluate $P$ at any given $x$.
%
Ignoring the condition on coefficients for now, we can assume that the
coefficients are given as a list |as = [a0, a1, ..., an]| (we prefer
counting up).
%
Then the evaluation function is:
%
\label{def:evalL}
\begin{spec}
evalL ::  [REAL] ->  REAL  ->  REAL
evalL     []         x     =   0
evalL     (a : as)   x     =   a + x * evalL as x
\end{spec}
%
Note that we can read the type as |evalL :: [REAL] -> (REAL -> REAL)|
and thus identify |[REAL]| as the type for the (abstract) syntax (for
polynomials) and |(REAL -> REAL)| as the type of the semantics (for
polynomial functions).
%
Exercise~\ref{exc:evalLSpec}: Show that this evaluation function gives
the same result as the formula above.

Using the |Ring| instance for functions we can rewrite |eval| into a
one-argument function (returning a polynomial function):
%
\begin{code}
evalL :: [REAL] -> (REAL -> REAL)
evalL []      = const 0
evalL (a:as)  = const a  +  id * evalL as
\end{code}
%
As an example, the polynomial which is usually written just |x| is
represented by the list |[0, 1]| and the polynomial function |\x -> x^2-1| is
represented by the list |[-1,0,1]|.


It is worth noting that the definition of what we call a ``polynomial
function'' is semantic, not syntactic.
%
A syntactic defintion would talk about the form of the expression (a
sum of coefficients times natural powers of $x$).
%
In contrast, this semantic definition only requires that the function
|P| \emph{behaves like} such a sum.
%
% (Has the same value for all |x|.)
%
Insisting on this difference may seem pedantic, but here is an
interesting example of a family of functions which syntactically does
not look like a sum of powers:
%
\[T_n(x) = \cos (n*\arccos(x))\ .\]
%
And yet, it can be shown that \(T_n\) is a polynomial function of
degree |n| (on the interval |[-1,1]|).
%
Exercise~\ref{ex:chebyshev} guides you to a proof.
%
At this point you could just compute \(T_0\), \(T_1\), and \(T_2\) by
hand to get a feeling for how it works.

% TODO: perhaps talk about an alternative, recursive, definition of polynomial function, closer to the implementation of |eval| blackboard/W5/20170213_114415.jpg

Not every list of coefficients is valid according to the definition.
%
In particular, the empty list is not a valid list of coefficients, so
we have a conceptual, if not empirical, type error in our evaluator.

The valid lists are those \emph{finite} lists in the set

\begin{spec}
  {[0]} ∪ {(a : as) | last (a : as) ≠ 0}
\end{spec}

The fact that the element should be non-zero is easy to express as a
Haskell expression (|last (a : as) ≠ 0|), but not so easy to express
in the \emph{types}.

We could try jumping through the relevant hoops.
%
However, at this stage, we can realise that the the non-zero condition
is there only to define the degree of the polynomial.
%
The same can be said about the separation between zero and non-zero
polynomials, which is there to explicitly leave the degree
undefined.
%
So we can further improve the definition as follows:
\begin{quote}
  A \textbf{polynomial} is a function $P : ℝ → ℝ$ such that
  there exist $a_0$, \ldots, $a_n$ : ℝ and for any $x : ℝ$
  \[P(x) = a_n x^n + a_{n-1} x^{n - 1} + \cdots + a_1 x + a_0\]
  The degree of the polynomial is the largest $i$ such that $a_i≠0$.
\end{quote}
This definition is much simpler to manipulate and clearly separates
the definition of degree from the definition of polynomial.
%
Perhaps surprisingly, there is no longer any need to single out the
zero polynomial to define the degree.
%
Indeed, when the polynomial is zero, $a_i=0$ for every $i$, and we
have an empty set of indices where $a_i≠0$.
%
The largest element of this set is undefined (by definition of
largest, see also \cref{ex:maximum-homo}), and we have the intended
definition.

So, we can simply use any list of coefficients to \emph{represent} a
polynomial:
\begin{code}
newtype Poly a = Poly [a] deriving (Show,Eq)
\end{code}

Since we only use the arithmetic operations, we can generalise our
evaluator to an arbitrary |Ring| type.
%
\begin{code}
evalPoly :: Ring a => Poly a -> (a -> a)
evalPoly (Poly [])        _   =  0
evalPoly (Poly (a:as))    x   =  a + x * evalPoly (Poly as) x
\end{code}
%
Since we have |Ring a|, there is a |Ring| structure on |a -> a|, and
|evalPoly| looks like a homomorphism.
%
Question: is there a |Ring| structure on |Poly a|, such that |evalPoly|
is a homomorphism?

For example, the homomorphism condition gives for |(+)|
%
\begin{spec}
evalPoly as + evalPoly bs = evalPoly (as + bs)
\end{spec}
%
Note that this equation uses |(+)| at two different type: on the left
hand side (lhs) two functions of type |a->a| are added (pointwise) and
on the right hand side (rhs) two |Poly a| (lists of coefficients) are
added.
%
We are using the homomorphism condition to find requirements on the
definition of |(+)| on |Poly a|.

Both sides (lhs and rhs) are functions, thus they are equal if and
only if they are equal for every argument.
%
For an arbitrary |x|

%
\begin{spec}
  (evalPoly as + evalPoly bs) x = evalPoly (as + bs) x

<=> {- |(+)| on functions is defined point-wise -}

  evalPoly as x + evalPoly bs x = evalPoly (as + bs) x
\end{spec}

To proceed further, we need to consider the various cases in the
definition of |evalPoly| and use list induction.
%
We give here the computation for the step case, dropping the |Poly|
constructor and writing |eval cs = evalPoly (Poly cs)| for brevity.
%
%{
%format evalPoly = eval
\begin{spec}
evalPoly (a : as) x  +  evalPoly (b : bs) x  =  evalPoly ((a : as)  +  (b : bs)) x
\end{spec}

We use the homomorphism condition for |as| and |bs|
For the left-hand side, we have:
%
\begin{spec}
  evalPoly (a : as) x  +  evalPoly (b : bs) x        =  {- def. |evalPoly| -}

  (a + x * evalPoly as x) + (b + x * evalPoly bs x)  =  {- arithmetic (ring laws) -}

  (a + b) + x * (evalPoly as x + evalPoly bs x)      =  {- homomorphism condition -}

  (a + b) + x * (evalPoly (as + bs) x)               =  {- def. |evalPoly| -}

  evalPoly ((a + b) : (as + bs)) x
\end{spec}
%}

The homomorphism condition will hold for every |x| if we define
%
\begin{spec}
  (a : as) + (b : bs)  = (a + b) : (as + bs)
\end{spec}
%
This definition looks natural (we could probably have guessed it early
on) but it is still interesting to see that we can derive it as the
form that it has to take for the proof to go through.

We leave the derivation of the other cases and operations as an
exercise.
%
Here, we just give the corresponding definitions.
%
\label{sec:mulPoly}
\begin{code}
instance Additive a => Additive (Poly a) where
  (+) = addPoly
  zero = Poly []

instance Ring a => Multiplicative (Poly a) where
  (*) = mulPoly
  one = Poly [one]

instance AddGroup a => AddGroup (Poly a) where
  negate = negPoly

addPoly :: Additive a => Poly a -> Poly a -> Poly a
addPoly (Poly xs) (Poly ys) = Poly (addList xs ys)

addList :: Additive a => [a] -> [a] -> [a]
addList = zipWithLonger (+)

zipWithLonger :: (a->a->a) -> ([a] -> [a] -> [a])
zipWithLonger _   []      bs      = bs  -- |0+bs == bs|
zipWithLonger _   as      []      = as  -- |as+0 == as|
zipWithLonger op  (a:as)  (b:bs)  = op a b : zipWithLonger op as bs

mulPoly :: Ring a => Poly a -> Poly a -> Poly a
mulPoly (Poly xs) (Poly ys) = Poly (mulList xs ys)

mulList :: Ring a => [a] -> [a] -> [a]
mulList  []      _       =  []    -- |0*bs == 0|
mulList  _       []      =  []    -- |as*0 == 0|
mulList  (a:as)  (b:bs)  =  (a * b) :  addList  (scaleList a  bs)
                                                (mulList as   (b:bs))
scaleList :: Multiplicative a => a -> [a] -> [a]
scaleList a = map (a*)

negPoly :: AddGroup a => Poly a -> Poly a
negPoly = polyMap negate

polyMap :: (a->b) -> (Poly a -> Poly b)
polyMap f (Poly as)   = Poly (map f as)

polyCons :: a -> Poly a -> Poly a
polyCons x (Poly xs) = Poly (x:xs)
\end{code}
Therefore, we \emph{can} define a |Ring| structure on |Poly a|, and we
have arrived at the canonical definition of polynomials, as found in
any algebra book (see, for example, \cite{rotman2006first} for a very
readable text):
%
\begin{quote}
  Given a commutative ring |A|, the commutative ring given by the set
  |Poly A| together with the operations defined above is the ring of
  \textbf{polynomials} with coefficients in |A|.
\end{quote}
%
Note that from here on we will use the term ``polynomial'' for the
abstract syntax (the list of coefficients, |as|) and ``polynomial
function'' for its semantics (the function |evalPoly as : A -> A|).


\textbf{Caveat:} The canonical representation of polynomials in
algebra does not use finite lists, but the equivalent

\begin{spec}
  Poly' A = { a : ℕ → A | {- |a| has a finite number of non-zero values -} }
\end{spec}

Exercise~\ref{exc:Poly'}: What are the ring operations on |Poly' A|?
%
For example, here is the specification of addition:
%
\begin{spec}
  a + b = c  <=>  Forall (n : ℕ) (a n + b n = c n)
\end{spec}
%
Hint: they are not all the same as the operations on arbitrary
functions |X -> A| defined in \cref{sec:FunNumInst}.

Remark: Using functions from |ℕ| in the definition has certain
technical advantages over using finite lists.
%
For example, consider adding |[a0, a1, ..., an]| and |[b0, b1, ...,
bm]|, where |n > m|.
%
Then, we obtain a polynomial of degree |n|: |[c0, c1, ..., cn]|.
%
The formula for the |ci| must now be given via a case distinction:

< ci = if i > m then ai else ai + bi

\noindent
since |bi| does not exist for values greater than |m|.

Compare this with the formula for functions from |ℕ|, where no case
distinction is necessary.
%
The advantage is even clearer in the case of multiplication.

\paragraph{Observations:}

\label{sec:polynotpolyfun}
\begin{enumerate}
\item If one considers arbitrary rings, polynomials are not isomorphic
  (in one-to-one correspondence) to polynomial functions.
  %
  For any finite ring |A|, there is a finite number of functions |A ->
  A|, but there is a countable infinity of polynomials.
  %
  That means that the same polynomial function on |A| will be the
  evaluation of many different polynomials.

  For example, consider the ring |ℤ₂| (|{0, 1}| with addition and
  multiplication modulo |2|).
  %
  In this ring, we have that |p x = x+x^2| is actually a constant
  function.
  %
  The only two input values to |p| are |0| and |1| and we can easily
  check that |p 0 = 0| and also |p 1 = mod (1+1^2) 2 = mod 2 2 = 0|.
  %
  Thus
  \begin{spec}
    eval [0, 1, 1] = p = const 0 = eval []  {-"\quad"-}-- in |ℤ₂ -> ℤ₂|
  \end{spec}

  but

  \begin{spec}
    [0, 1, 1] ≠ []  {-"\quad"-} -- in |Poly ℤ₂|
  \end{spec}

  Therefore, it is not generally a good idea to conflate polynomials
  (syntax) and polynomial functions (semantics).

\item Following the DSL terminology, we can say that the polynomial
  functions are the semantics of the language of polynomials.
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
%
\begin{code}
x :: Ring a => Poly a
x = Poly [0,1]
\end{code}

Then for any polynomial |as = Poly [a0, a1, ..., an]| we have

%{
%format .* = "\mathbin{\cdot}"
  \begin{spec}
    as = a0 .* x^0 + a1 .* x^1 + a2 .* x^2 + ... + an .* x^n
  \end{spec}
  where |(+)| is addition of coefficient lists and |(.*)| is an infix version of
  |scaleList|.
Exercise~\ref{exc:polySpecList}: Prove the above equality.

This equality justifies the standard notation

\begin{spec}
  as = {-"\sum_{i = 0}^n"-} ai .* x^i
\end{spec}
%}

\end{enumerate}

\section{Aside: division and the degree of the zero polynomial}

Recall the fundamental property of division that we learned in high school:
\begin{quote}
For all naturals |a|, |b|, with |b ≠ 0|, there exist \emph{unique}
integers |q| and |r|, such that
%
\begin{spec}
a = b * q + r,{-"\qquad\text{with}\quad"-} 0 <= r < b
\end{spec}
%
When |r = 0|, |a| is divisible by |b|.
\end{quote}
%
Questions of divisibility are essential in number theory and its
applications (including cryptography).
%
A similar theorem holds for polynomials (see, for example, \cite[page 40]{adams2010calculus}):
\begin{quote}
For all polynomials |as|, |bs|, with |bs ≠ 0|, there exist \emph{unique} polynomials |qs| and |rs|, such that
%
\begin{spec}
as = bs * qs + rs,{-"\qquad\text{with}\quad"-} degree rs < degree bs
\end{spec}
\end{quote}
%
The condition |r < b| is replaced by |degree rs < degree bs|.
%
However, we now have a problem.
%
Every polynomial is divisible by any non-zero constant polynomial,
resulting in a zero polynomial remainder.
%
But the degree of a constant polynomial is zero.
%
If the degree of the zero polynomial were a natural number, it would
have to be smaller than zero.
%
For this reason, it is either considered undefined (as in
\cite{adams2010calculus}), or it is defined as |-∞|.%
\footnote{Likewise we could define the largest element of the empty
  set to be |-∞|.}
%
The next section examines this question from the point of view of
homomorphisms.

\section{Polynomial degree as a homomorphism}

It is often the case that a certain function is \emph{almost} a
homomorphism and the domain or range structure is \emph{almost} a monoid.
%
In \cref{sec:evalD}, we saw tupling as one way to fix such a
problem and here we will introduce another way.

The |degree| of a polynomial is a good candidate for being a
homomorphism:
%
if we multiply two polynomials we can normally add their degrees.
%
If we try to check that |degree :: Poly a -> Nat| is the function
underlying a monoid morphism we need to decide on the monoid structure
to use for the source and for the target, and we need to check the
homomorphism laws.
%
We can use the multiplicative monoid (|unit = one| and |op = mulPoly|)
for the source and we can try to use the additive monoid (|unit = zero|
and |op = (+)|) for the target monoid.
%
Then we need to check that
%
\begin{spec}
degree one = zero
∀ x, y? degree (x `op` y) = degree x  +  degree y
\end{spec}
%
The first law is no problem and for most polynomials the second law is
also straighforward to prove (try it as an exercise).
%
But we run into trouble with one special case: the zero polynomial.

Looking back at the definition from \cite[page 55]{adams2010calculus}
it says that the degree of the zero polynomial is not defined.
%
Let's see why that is the case and how we might ``fix'' it.
%
Assume that there exists a natural number |z| such that |degree 0 =
z|.
%
Assume additionally a polynomial |p| with |degree p = n|.
%
Then we get
%
\begin{spec}
  z                               = {- assumption -}

  degree 0                        = {- simple calculation -}

  degree (0 * p)                  = {- homomorphism condition -}

  degree 0 + degree p             = {- assumption -}

  z + n
\end{spec}
%
Thus we need to find a degree |z|, for the zero polynomial, such that
|z = z + n| for all natural numbers |n|!
%
At this stage we could either give up, or think out of the box.
%
Intuitively we could try to use |z = -Infinity|, which would seem to
satisfy the law but is not a natural number (not even an integer).
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
  unit  = Just unit
  op    = opMaybe

opMaybe :: Monoid' a => Maybe a -> Maybe a -> Maybe a
opMaybe Nothing    _m         = Nothing    -- |(-Inf) + m  = -Inf|
opMaybe _m         Nothing    = Nothing    -- |m + (-Inf)  = -Inf|
opMaybe (Just m1)  (Just m2)  = Just (op m1 m2)
\end{code}
%}

% TODO: perhaps mention another construction:
% We quote the Haskell prelude implementation:
% % https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.Base.html#line-314
% \begin{quote}
%   Lift a semigroup into |Maybe| forming a |Monoid| according to
%   \url{http://en.wikipedia.org/wiki/Monoid}: "Any semigroup |S| may be
%   turned into a monoid simply by adjoining an element |e| not in |S|
%   and defining |e*e = e| and |e*s = s = s*e| for all |s ∈ S|." Since
%   there is no |Semigroup| typeclass [..], we use |Monoid| instead.
% \end{quote}

Thus, to sum up, |degree| is a monoid homomorphism from |(Poly a, 1,
*)| to |(Maybe Nat, Just 0, opMaybe)|.

Exercise~\ref{exc:degreeMonoidHomomorphism}: Check all the Monoid and
homomorphism properties.

\section{Power Series}
\label{sec:power-series}
Consider the following (false) proposition:

\begin{proposition}
  Let |m, n ∈ ℕ| and let |cs| and |as| be any polynomials of degree |m
  + n| and |n|, respectively, and with |a0 /= 0|.
%
  Then there exists a polynomial |bs| of degree |m| such that |cs = as
  * bs| (thus |cs| is divisible by |as|).
\end{proposition}

Even if the proposition is false, we can make the following proof
attempt:
\begin{proof}
We need to find |bs = [b0, ..., bm]| such that |cs = as * bs|.
%
From the multiplication of polynomials, we know that

< ck = {-"\sum_{i = 0}^k a_i * b_{k-i}"-}

Therefore:

< c0 = a0 * b0

Since |c0| and |a0| are known, computing |b0 = c0/a0| is trivial.
%
Next

< c1 = a0 * b1 + a1 * b0

Again, we are given |c1|, |a0| and |a1|, and we have just computed
|b0|, therefore we can obtain |b1 = (c1-a1*b0)/a0|.
%
Similarly

< c2 = a0 * b2 + a1 * b1 + a2 * b0

from which we obtain,as before, the value of |b2| by subtraction and division.

It is clear that this process can be continued, yielding at every step
a value for a coefficient of |bs|, and thus we have obtained |bs|
satisfying |cs = as * bs|.

\end{proof}

The problem with this proof attempt is in the statement ``it is clear
that this process can be continued''.
%
In fact, it is rather clear that it cannot be continued (for polynomials)!
%
Indeed, |bs| only has |m+1| coefficients, therefore for all remaining
|n| equations of the form |ck = {-"\sum_{i = 0}^k a_i * b_{k-i}"-}|,
the values of |bk| (for |k>m|) have to be zero.
%
But in general this will not satisfy the equations.

However, we can now see that, if we were able to continue forever, we
would be able to divide |cs| by |as| exactly.
%
The only obstacle is the ``finite'' nature of our lists of
coefficients.

Power series are obtained from polynomials by removing in |Poly'| the
restriction that there should be a \emph{finite} number of non-zero
coefficients; or, in, the case of |Poly|, by going from lists to
streams.


\begin{joincode}%
\begin{spec}
PowerSeries' a = { f : ℕ → a }
\end{spec}
\begin{code}
type PowerSeries a = Poly a   -- finite and infinite lists
\end{code}
\end{joincode}

The operations are still defined as before.
%
If we consider only infinite lists, then only the equations which deal
with nonempty lists will apply.

Power series are usually denoted

\[   \sum_{n = 0}^{\infty} a_n * x^n   \]

the interpretation of |x| being the same as before.
%
The simplest operation, addition, can be illustrated as follows:
\[
\begin{array}{>{\displaystyle}lllll}
    \sum_{i = 0}^{\infty} a_i * x^i            &\cong  &[a_0,     &a_1,    &\ldots ]
\\  \sum_{i = 0}^{\infty} b_i * x^i            &\cong  &[b_0,     &b_1,    &\ldots ]
\\  \sum_{i = 0}^{\infty} (a_i+b_i) * x^i      &\cong  &[a_0+b_0, &a_1+b_1,&\ldots ]
\end{array}
\]

The evaluation of a power series represented by |a : ℕ → A| is defined,
in case the necessary operations make sense on |A|, as a function
%
\begin{spec}
eval a : A -> A
eval a x  =  lim s   where   s n = {-" \sum_{i = 0}^n a_i * x^i "-}
\end{spec}
%
We will focus on the case in which |A = ℝ| or |A = ℂ|.
%
Note that |eval a| is, in general, a partial function (the limit might
not exist).
%
To make |eval a| a total function, the domain |A| would have to be
restricted to just those values of |x| for which the limit exists (the
infinite sum converges).
%
Keeping track of different domains for different power series is
cumbersome and the standard treatment is to work with ``formal power
series'' with no requirement of convergence.

Here the qualifier ``formal'' refers to the independence of the
definition of power series from the ideas of convergence and
evaluation.
%
In particular, two power series represented by |a| and |b|, respectively,
are equal only if |a = b| (as infinite series of numbers).
%
If |a ≠ b|, then the power series are different, even if |eval a =
eval b|.

Since we cannot in general compute limits, we can use an
approximative |eval|, by evaluating the polynomial initial segment
of the power series.

\begin{code}
evalPS :: Ring a => Int -> PowerSeries a -> (a -> a)
evalPS n as = evalPoly (takePoly n as)

takePoly :: Int -> PowerSeries a -> Poly a
takePoly n (Poly xs) = Poly (take n xs)
\end{code}
%TODO: perhaps explain with plain lists: |takeP :: Nat -> PS r -> P r| with |takeP n (PS as) = P (take n as)|
%
Note that |evalPS n| is not a homomorphism: for example:
%
\begin{spec}
  evalPS 2 (x*x) 1                     =
  evalPoly (takePoly 2 [0, 0, 1]) 1  =
  evalPoly [0,0] 1                   =
  0
\end{spec}
but
\begin{spec}
  (evalPS 2 x 1)                    =
  evalPoly (takePoly 2 [0, 1]) 1  =
  evalPoly [0, 1] 1               =
  1
\end{spec}
%
and thus |evalPS 2 (x*x) 1 = 0 /= 1 = 1*1 = (evalPS 2 x 1) * (evalPS 2 x
1)|.


\section{Operations on power series}

Power series have a richer structure than polynomials.
%
% TODO (by DaHe): I think the note about moving from Z to Q could be expanded
% upon. In Q, we have potentially infinite (repeating) digits, allowing division.
% Similarly, PowerSeries have infinite coefficients, and thus allows division.
% (not sure if there is any actual connection or just a coincidence, but still
% interesting to note)
%
For example, as suggested above, we also have division (this is reminiscent of the move from |ℤ|
to |ℚ| to allow division to be generalised).
%
To illustrate, let us start with a special case: trying to compute |p = frac 1 (1-x)| as a
power series.
%
The specification of |a/b = c| is |a=c*b|, thus in our case we need to
find a |p| such that |1 = (1-x)*p|.
%
For polynomials there is no solution to this equation.
%
One way to see that is by using the homomorphism |degree|: the degree
of the left hand side is |0| and the degree of the RHS is |1 +  degree p /= 0|.
%
But there is still hope if we move to formal power series.

Remember that |p| is then represented by a stream of coefficients,
and let that stream be |[p0, p1, ...]|.
%
We make a table of the coefficients of the RHS |= (1-x)*p =
p - x*p| and of the LHS |= 1| (seen as a power series).
%
\begin{spec}
  p      ==  [  p0,  p1,     p2,     ...
  x*p    ==  [  0,   p0,     p1,     ...
  p-x*p  ==  [  p0,  p1-p0,  p2-p1,  ...
  1      ==  [  1,   0,      0,      ...
\end{spec}
%
Thus, to make the last two lines equal, we are looking for
coefficients satisfying |p0=1|, |p1-p0=0|, |p2-p1=0|, \ldots.
%
The solution is unique: |1 = p0 = p1 = p2 = | \ldots
%
but it only exists for streams (infinite lists) of coefficients.
%
In the common math notation we have just computed
%
\[
  \frac{1}{1-x} = \sum_{i = 0}^{\infty} x^i
\]
%
Note that this equation holds when we interpret both sides as formal
power series, but not necessarily if we try to evaluate the
expressions for a particular~|x|.
%
Indeed, the RHS will converge if |absBar x < 1| but not for |x=2|, for example.

For a more general case of power series division, consider |p/q| with |p =
a:as|, |q = b:bs|, and assume that |a * b ≠ 0|.
%
Then we want to find, for any given |(a : as)| and |(b : bs)|, the
series |(c : cs)| satisfying
%
\begin{spec}
  (a : as) / (b : bs) = (c : cs)                     <=> {- spec. of division -}

  (a : as) = (c : cs) * (b : bs)                     <=> {- def. of |*| for |Cons| -}

  (a : as) = (c * b)  :  ([c]*bs  +  cs * (b : bs))  <=> {- equality on components -}

  a   = c * b                          {- and -}
  as  = [c] * bs + cs * (b : bs)       {-" "-}       <=> {- solve for |c| and |cs| -}

  c   = a / b                          {- and -}
  cs  = (as - [c] * bs) / (b : bs)
\end{spec}

This leads to the implementation:

\begin{code}
instance (Eq a, Field a) => MulGroup (PowerSeries a) where
  (/) = divPS

divPS :: (Eq a, Field a) => PowerSeries a -> PowerSeries a -> PowerSeries a
divPS (Poly as) (Poly bs) = Poly (divL as bs)

divL :: (Eq a, Field a) => [a] -> [a] -> [a]
divL []      _bs     =  []                             -- case |0/q|
divL (0:as)  (0:bs)  =  divL as bs                     -- case |xp/xq|
divL (0:as)  bs      =  0 : divL as bs                 -- case |xp/q|
divL as      [b]     =  scaleList (1 / b) as           -- case |p/c|
divL (a:as)  (b:bs)  =  c : divL (addList as (scaleList (-c) bs)) (b:bs)
                        where c = a/b
divL _       []      = error "divL: division by zero"
\end{code}

This definition allows us to also use division on polynomials, but the
result will, in general, be a power series, not a polynomial.
%
The different cases can be calculated from the specification.
%
Some examples:
%
\begin{code}
ps0, ps1, ps2 :: (Eq a, Field a) => PowerSeries a
ps0  = 1 / (1 - x)                    -- |ps0 == Poly [1, 1, 1, 1, ...]|
ps1  = 1 / (1 - x)^2                  -- |ps1 == Poly [1, 2, 3, 4, ...]|
ps2  = (x^2 - 2 * x + 1) / (x - 1)    -- |ps2 == Poly [-1,1,0]|
\end{code}
%
Every |ps| is the result of a division of polynomials: the first two
return power series, the third is a polynomial (even though it ends up
having a trailing zero).
%
%if False
\begin{code}
example0, example01 :: (Eq a, Field a) => PowerSeries a
example0   = takePoly 10 ps0
example01  = takePoly 10 (ps0 * (1-x))
\end{code}
%endif
%
We can get a feeling for the definition by computing |ps0| ``by
hand''.
%
We let |p = [1]| and |q=[1,-1]| and seek |r = p/q|.
%
\begin{spec}
  divL p q                                  =  {- def. of |p| and |q| -}
  divL (1:[])  (1:[-1])                     =  {- main case of |divL| -}
  (1/1) : divL ([] - [1] * [-1])  (1:[-1])  =  {- simplification, def. of |(*)|, |(-)| -}
  1 : divL [1] (1:[-1])                     =  {- def. of |p| and |q| -}
  1 : divL p q
\end{spec}
%
Thus, the answer |r| starts with |1| and continues with |r|!
%
In other words, we have that |1/[1,-1] = [1,1..]| as an infinite list
of coefficients and \(\frac{1}{1-x} = \sum_{i=0}^{\infty} x^i\) in the
more traditional mathematical notation.


\section{Formal derivative}
\label{sec:poly-formal-derivative-1}
Considering the analogy between power series and polynomial functions
(via polynomials), we can arrive at a formal derivative for power
series through the following computation:
%
\begin{equation}
  \label{eq:formalderivative}
\begin{aligned}
  \left(\sum_{n = 0}^{\infty} a_n * x^n\right)'  &= \sum_{n = 0}^{\infty} (a_n * x^n)'  =  \sum_{n = 0}^{\infty} a_n * (x^n)' = \sum_{n = 0}^{\infty} a_n * (n * x^{n-1})  \\
  &=  \sum_{n = 0}^{\infty} (n * a_n) * x^{n-1} =  \sum_{n = 1}^{\infty} (n * a_n) * x^{n-1}  \\
  &=  \sum_{m = 0}^{\infty} ((m+1) * a_{m+1}) * x^m
\end{aligned}
\end{equation}
%
Thus the $m$th coefficient of the derivative is \((m+1) * a_{m+1}\).

%TODO: redo to arrive at the recursive formulation.

We can implement this formula, for example, as

\begin{code}
deriv :: Ring a => Poly a -> Poly a
deriv (Poly as) = Poly (derivL as)

derivL :: Ring a => [a] -> [a]
derivL []      = []
derivL (_:as)  = zipWith (*) oneUp as

oneUp :: Ring a => [a]
oneUp = one : map (one+) oneUp
\end{code}

Side note: we cannot in general implement a decidable (Boolean) equality test for
|PowerSeries|.
%
For example, we know that |deriv ps0| equals |ps1| but we cannot
compute |True| in finite time by comparing the coefficients of the two
power series.

\begin{code}
checkDeriv :: Int -> Bool
checkDeriv n  =  takePoly n (deriv ps0) == takePoly n (ps1 :: Poly Rational)
\end{code}

Recommended reading: the Functional pearl: ``Power series, power
serious'' \cite{mcilroy1999functional}.


% ================================================================

% \section{Signals and Shapes}
%
% Shallow and deep embeddings of a DSL
%
% TODO: perhaps textify DSL/


%if False
\section{Helpers}

\begin{code}
instance Functor Poly where  fmap = polyMap

instance Ring a => Monoid' (Poly a) where
  unit  = Poly [one]
  op    = (*)

instance Monoid' Integer where
  unit  = 0
  op    = (+)

type Nat = Integer

degree :: (Eq a, Ring a) => Poly a -> Maybe Nat
degree (Poly []) = Nothing
degree (Poly (x:xs)) = mayMax  (if x == zero then Nothing else Just 0)
                               (fmap (1+) (degree (Poly xs)))

mayMax :: Ord a => Maybe a -> Maybe a -> Maybe a
mayMax x        Nothing   = x
mayMax Nothing  (Just d)  = Just d
mayMax (Just a) (Just b)  = Just (max a b)

degreeAlt :: (Eq a,AddGroup a) => Poly a -> Maybe Nat
degreeAlt = mayMaximum . coefIndices

coefIndices :: (Eq a,AddGroup a) => Poly a -> [Nat]
coefIndices (Poly as) = [i | (a,i) <- zip as [1..], a  /= zero]

mayMaximum :: Ord a => [a] -> Maybe a
mayMaximum []      = Nothing
mayMaximum (x:xs)  = mayMax (Just x) (mayMaximum xs)

checkDegree0 = degree (unit :: Poly Integer) == unit
checkDegreeM :: Poly Integer -> Poly Integer -> Bool
checkDegreeM p q = degree (p*q) == op (degree p) (degree q)
\end{code}
%endif

%include E5.lhs
