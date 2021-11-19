%if False
\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DSLsofMath.W06 where
import DSLsofMath.FunExp hiding (eval, derive)
import DSLsofMath.W05
import DSLsofMath.Algebra
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..))
import Prelude (abs)
import Data.Complex ()
\end{code}
%endif

\chapter{Higher-order Derivatives and their Applications}
\label{sec:deriv}

In this chapter we make heavy use of concepts from \cref{sec:CompSem}
and thus we urge readers to verify their understanding of
\cref{sec:homomophism-roadmap,exc:findFunExp0} in case they
might have skipped it.
%
We have seen in particular that we can give a numeric (|Field|, etc.)
structure not only to functions, but also to pairs of functions and
their derivatives (|Field x => Field (a -> x, a -> x)|).
%
But why stop there?
%
Why not compute a (lazy) list (also called a stream) of a function
together with its derivative, second derivative, etc.:
%
\begin{spec}
[f, f', f'', ...] :: [a -> a]
\end{spec}
%
The above then represents the evaluation of a 1-variable expression as
a function, and all its derivatives.
%
We can write this evaluation as an explicit function:
%
\begin{code}
evalAll :: Transcendental a => FunExp -> [a -> a]
evalAll e = (evalFunExp e) : evalAll (derive e)
\end{code}

However |evalAll| is a non-compositional way of computing this
stream. We will now proceed to define a specification of |evalAll| (as
a homomorphism), and then derive a compositional implementation. Along
the way we will continue to build insight about such streams of
derivatives.

Notice that if we look at our stream of derivatives,
%
\begin{spec}
[f, f', f'', ...] = evalAll e
\end{spec}
%
then the tail is also such a stream, but starting from |f'|:
%
\begin{spec}
[f', f'', ...] = evalAll (derive e)
\end{spec}
%
Thus |evalAll (derive e) == tail (evalAll e)| which can be written
|evalAll . derive = tail . evalAll|.
%
Thus |evalAll| is a homomorphism from |derive| to |tail|, or in other
words, we have |H1(evalAll,derive,tail)| (|H1| was defined in
\cref{exc:homomorphisms}) --- this is our specification for what
follows.

We want to define the other numeric operations on streams of
derivatives in such a way that |evalAll| is a homomorphism in each of
them.
%
For example:
%
\begin{spec}
evalAll (e1 :*: e2) = evalAll e1 * evalAll e2
\end{spec}
%
where the |(*)| sign stands for the multiplication of derivative
streams --- an operation we are trying to determine.
%
We assume that we have already derived the definition of |(+)| for
these streams (it is |zipWithLonger (+)|, or just |zipWith (+)| if we
stick to infinite streams only).

We have the following derivation (writing |eval| for |evalFunExp| and
|d| for |derive| in order to get a better overview):
%
\begin{spec}
    LHS
= {- def. -}
    evalAll (e1 :*: e2)
= {- def. of |evalAll| -}
    eval (e1 :*: e2) : evalAll (d (e1 :*: e2))
= {- def. of |eval| for |(:*:)| -}
    (eval e1 * eval e2) : evalAll (d (e1 :*: e2))
= {- def. of |derive| for |(:*:)| -}
    (eval e1 * eval e2) : evalAll ((d e1 :*: e2) :+: (e1 * d e2))
= {- we assume |H2(evalAll, (:+:), (+))| -}
    (eval e1 * eval e2) : (evalAll (d e1 :*: e2) + evalAll (e1 :*: d e2))
\end{spec}

Similarly, starting from the other end we get
%
\begin{spec}
    evalAll e1 * evalAll e2
= {- def. of |evalAll|, twice -}
    (eval e1 : evalAll (d e1)) * (eval e2 : evalAll (d e2))
\end{spec}
%
Now, to see the pattern it is useful to give simpler names to some
common subexpressions: let |a = eval e1| and |b = eval e2|.
%
\begin{spec}
    (a * b) : (evalAll (d e1 :*: e2) + evalAll (e1 * d e2))
=?
    (a : evalAll (d e1)) * (b : evalAll (d e2))
\end{spec}
%
Now we can solve part of the problem by defining |(*)| as
%
\begin{spec}
(a : as) * (b : bs) = (a*b) : help a b as bs
\end{spec}
%
The remaining part is then
%
\begin{spec}
    evalAll (d e1 :*: e2) + evalAll (e1 :*: d e2)
=?
    help a b (evalAll (d e1)) (evalAll (d e2))
\end{spec}

We now have two terms of the same form as we started out from: calls
of |evalAll| on the constructor |(:*:)|.
%
If we assume the homomorphism condition holds for these two calls we
can rewrite |evalAll (d e1 :*: e2)| to |evalAll (d e1) * evalAll e2|
and similarly for the second term.
%
(For a formal proof we also need to check that this assumption can be
discharged.\pj{Do the formal proof - perhaps in appendix.}
\jp{Look at this})

We also have |evalAll . d = tail . evalAll| which leads to:
%
\begin{spec}
    tail (evalAll e1) * evalAll e2 + evalAll e1 * tail (evalAll e2)
=?
    help a b (tail (evalAll e1)) (tail (evalAll e2))
\end{spec}
%
Finally we rename common subexpressions: let |a:as = evalAll e1| and
|b:bs = evalAll e2|.
\begin{spec}
    tail (a:as) * (b:bs)  +  (a:as) * tail (b:bs)
=?
    help a b (tail (a:as)) (tail (b:bs))
\end{spec}
%
This equality is clearly satisfied if we define |help| as follows:
%
\begin{code}
help a b as bs = as * (b : bs) + (a : as) * bs
\end{code}
%
Thus, we can eliminate |help| to arrive at a definition for
multiplication\footnote{This expression is reminiscent of polynomial multiplication
  (\cref{sec:mulPoly}), but it is different from it because here each
  element is implicitly divided by a factorial, as we shall see below. Hence we compute several terms many times
  here, and sum them together.}:
%
\begin{code}
mulStream :: Ring a => Stream a -> Stream a -> Stream a
mulStream (a : as) (b : bs) = (a*b) :  (as * (b : bs) + (a : as) * bs)
\end{code}
%
As in the case of pairs, we find that we do not need any properties of
functions, other than their |Ring| structure, so the definitions apply
to any infinite list of |Ring| values which we call a |Stream|:
%
\begin{code}
type Stream a = [a]
instance Additive a => Additive (Stream a) where
  zero  = repeat zero
  (+)   = addStream
instance Ring a => Multiplicative (Stream a) where
  one   = one : zero
  (*)   = mulStream

addStream :: Additive a => Stream a -> Stream a -> Stream a
addStream (a : as)  (b : bs)  =  (a + b)  :  (as + bs)
\end{code}

\begin{exercise}
Complete the instance declarations for |Fractional| and
|Transcendental|.
\end{exercise}
%
Note that it may make more sense to declare a |newtype| for |Stream a|
instead of using |[a]|, for at least two reasons.
%
First, because the type |[a]| also contains finite lists, but we use |Stream|
here to represent only the infinite lists.
%
Second, because there are competing possibilities for |Ring| instances
for infinite lists, for example applying all the operations
indexwise.\footnote{These can be obtained applying the the
  homomorphism between |[a]| and |ℕ -> a| to the |Ring| instances of
  |(x -> a)|}
%
We used just a type synonym here to avoid cluttering the definitions
with the |newtype| constructors.

%
\begin{exercise}
  Write a general derivative computation: |drv k f x = | the |k|th
  derivative of |f| at |x|.
\end{exercise}

% \begin{exercise}
%   Compare the efficiency of different ways of computing derivatives.
%   \jp{This is a pretty tough exercise... See the footnote on exponentials.}

% \end{exercise}


\section{Taylor series}
\label{sec:taylor-series}

We have arrived at the instances for |Stream (a->a)| by reasoning
about lists of functions.
%
But everywhere we needed to manipulate functions, we ended up using
their numerical structure directly (assuming instances such as |Ring
(x -> a)|, rather than treating them at functions).
%
So, the |Stream| instances hold for \emph{any} numeric type |a|.
%
Effectively, we have implicitly used the |apply| homomorphism, as we
did in \cref{sec:applyFD}.
%
So we can view |Stream a| as series of higher-order derivatives taken
at the same point |a|:

\begin{spec}
[f(a), f'(a), f''(a), ...]
\end{spec}

Assume now that |f| is a power series of coefficients (|ai|):

|f = eval [a0, a1, ..., an, ...]|

We derive:
\begin{spec}
   f 0    =  a0
   f'     =  eval (deriv [a0, a1, ..., an, ...])
          =  eval ([1 * a1, 2 * a2, 3 * a3, ..., n * an, ...])
=>
   f' 0   =  1 * a1
   f''    =  eval (deriv [1 * a1, 2 * a2, ..., n * an, ...])
          =  eval ([2 * 1 * a2, 3 * 2 * a3, ..., n * (n - 1) * an, ...])
=>
   f'' 0  =  2 * 1 * a2
\end{spec}

In general:
%
\begin{spec}
   {-"f^{(k)} "-} 0  =  fact k * ak
\end{spec}
%
Therefore
%
\begin{spec}
   f      =  eval [f 0, f' 0, f'' 0 / 2, ..., {-"f^{(n)} "-} 0 / (fact n), ...]
\end{spec}
%
That is, there is a simple mapping between the representation of |f|
as a power series (the coefficients |ak|), and the value of all
derivatives of |f| at |0|.

The power series represented by |[f 0, f' 0, f'' 0 / 2, ...,
{-"f^{(n)} "-} 0 / (fact n), ...]| is called the Taylor series centred
at |0|, or the Maclaurin series.
\begin{code}
type Taylor a = Stream a
\end{code}

We can perform the above mapping (between a power series and its
Maclaurin series) efficiently as follows:
%
\begin{code}
toMaclaurin :: Ring a => PowerSeries a -> Taylor a
toMaclaurin (Poly as) = zipWith (*) as factorials

fromMaclaurin :: Field a => Taylor a -> PowerSeries a
fromMaclaurin as = Poly (zipWith (/) as factorials)
\end{code}
%
using a list of all factorials (starting from 0):
\begin{code}
factorials :: Ring a => [a]
factorials = factorialsFrom 0 1

factorialsFrom :: Ring a => a -> a -> [a]
factorialsFrom n factn = factn : factorialsFrom (n+1) (factn * (n + 1))
\end{code}
Remember that |x = Poly [0,1]|:
\begin{code}
ex3, ex4 :: (Eq a, Field a) => Taylor a
ex3 = toMaclaurin (x^3 + two * x)
ex4 = toMaclaurin sinx
\end{code}

This means that the |Taylor| type, interpreted as a Maclaurin series, can work as an alternative representation for power series (and in certain cases it can be a better choice computationally).

Regardless, we can see |toMaclaurin| as a way to compute all the derivatives at |0| for all
functions |f| constructed with the grammar of |FunExp|.
%
That is because, as we have seen, we can represent all of them by
power series!

What if we want the value of the derivatives at some other point |a|
(different from zero)?
%
We then need the power series of the ``shifted'' function g:
%
\begin{spec}
g x  =  f (a + x)  <=>  g = f . (a+)
\end{spec}
%
If we can represent |g| as a power series, say |[b0, b1, ...]|, then
we have
%
\begin{spec}
{-"g^{(k)} "-} 0  =  fact k * bk  =  {-"f^{(k)} "-} a
\end{spec}
%
In particular, we would have
%
\begin{spec}
f x  =  g (x - a)  =  {-"\sum"-} bn * (x - a){-"^n"-}
\end{spec}
%
which is called the Taylor expansion of |f| at |a|.

Example:
%
We have that |idx = [0, 1]|, thus giving us indeed the values
%
\begin{spec}
[id 0, id' 0, id'' 0, ...]
\end{spec}
%
In order to compute the values of
%
\begin{spec}
[id a, id' a, id'' a, ...]
\end{spec}
%
for |a /= 0|, we compute
%
\begin{code}
ida a = toMaclaurin (evalP (X :+: Const a))
\end{code}

More generally, if we want to compute the derivatives of a function
|f| constructed with |FunExp| grammar, at a point |a|, we can use the
power series of |g x = f (x + a)| (we additionally restrict ourselves
to the first 10 derivatives):
%
\begin{code}
d f a = take 10 (toMaclaurin (evalP (f (X :+: Const a))))
\end{code}

Use, for example, our |f x = sin x + 2 * x| from \cref{exc:findFunExp0}.
%
As before, we can use power series directly to construct the input:
%
\begin{code}
dP f a = toMaclaurin (f (idx + Poly [a]))
\end{code}

\section{Derivatives and Integrals for Maclaurin series}

Since the Maclaurin series represents |[f 0, f' 0, f'' 0, ...]|,
the tail of the list is equivalent to the derivative of |f|.
%
To prove that fact, one can substitute |f| by |f'| in the above.
%
Another way to see it is to remark that we started with the equation
|evalAll . derive = tail . evalAll|; but now our input represention is
\emph{already} a Maclaurin series so |evalAll = id|, and in turn
|derive = tail|.

In sum:
\begin{spec}
head  f    = eval f 0            -- value of |f| at |0|
tail  f    = deriv f             -- derivative of |f|
\end{spec}
Additionally, integration can be defined simply as the list
constructor ``cons'' with the first argument being the value of |f| at
|0|:
\begin{spec}
integ :: a -> Taylor a -> Taylor a
integ = (:)
\end{spec}

Given that we have an infinite list, we have |f = head f : tail
f|. Let's see what this law means in terms of calculus:
\begin{spec}
  f  ==  head f : tail f
     ==  integ (head f)  (tail f)
     ==  integ (f 0)  (deriv f)
\end{spec}
or, in traditional notation:

\[
  f(x) = f(0) + \int_0^x f'(t) \text{d}t
\]

which is the fundamental theorem of calculus.
%
In sum, we find that our definition of |integ| is exactly that needed
to comply with calculus laws.

\section{Integral for Formal Power series}
\label{sec:integral-power-series}

In \cref{sec:poly-formal-derivative-1} we found a definition of
derivatives for formal power series (which we can also divide, as well
as add and multiply):
\begin{spec}
deriv :: Ring a => PowerSeries a -> PowerSeries a
deriv (Poly [])      =  Poly []
deriv (Poly (_:as))  =  Poly (zipWith (*) oneUp as)

oneUp :: Ring a => [a]
oneUp = countUp one

countUp :: Ring a => a -> [a]
countUp = iterate (one+)
\end{spec}

With our insight regarding Taylor series, we can now see that |deriv =
fromMaclaurin . tail . toMaclaurin|.
%
We can apply the same recipe to obtain integration for power series:
%
\begin{code}
integ  ::  Field a => a -> PowerSeries a -> PowerSeries a
integ  a0 (Poly as)  =  Poly (integL a0 as)

integL :: Field a => a -> [a] -> [a]
integL c cs = c : zipWith (/) cs oneUp
\end{code}
%
Remember that |a0| is the constant that we need due to indefinite
integration.
%

These operations work on the type |PowerSeries a| which we can see as
the syntax of power series, often called ``formal power series''.
%
The intended semantics of a formal power series |a| is, as we saw in
Chapter~\ref{sec:poly}, an infinite sum
%
\begin{spec}
eval a : REAL -> REAL
eval a = \x ->  lim s   where   s n = {-" \sum_{i = 0}^n a_i * x^i "-}
\end{spec}
%
For any |n|, the prefix sum, |s n|, is finite and it is easy to see
that the derivative and integration operations are well defined.
%
When we take the limit, however, the sum may fail to converge for
certain values of |x|.
%
Fortunately, we can often ignore that, because seen as operations from
syntax to syntax, all the operations are well defined, irrespective of
convergence.

If the power series involved do converge, then |eval| is a morphism
between the formal structure and that of the functions represented:
%
\begin{spec}
eval as + eval bs    =  eval (as + bs)   -- |H2(eval,(+),(+))|
eval as * eval bs    =  eval (as * bs)   -- |H2(eval,(*),(*))|

eval (derive as)     =  D (eval as)      -- |H1(eval,derive,D)|
eval (integ c as) x  =  c + {-"\int_0^x "-} (eval as t) dt
\end{spec}

\section{Simple differential equations}

Many first-order differential equations have the structure
%
\begin{spec}
f' x = g f x, {-"\qquad"-} f 0 = f0
\end{spec}
%
i.e., they are defined in terms of the higher-order function |g| and initial value |f0|.
%
The fundamental theorem of calculus \cite[Sect.\ 5.5]{adams2010calculus} gives us
%
\begin{spec}
f x = f0 + {-"\int_0^x "-} (g f t) dt
\end{spec}

If |f = eval as|
%
\begin{spec}
eval as x = f0 + {-"\int_0^x "-} (g (eval as) t) dt
\end{spec}
%
Assuming that |g| is a polymorphic function defined both for the
syntax (|PowerSeries|) and the semantics (|REAL -> REAL|), and that
%
\begin{spec}
Forall as (eval (gSyn as) == gSem (eval as))
\end{spec}
%
or simply |H1(eval,g,g)|.
%
(This particular use of |H1| is read ``|g| commutes with |eval|''.)
%
Then we can move |eval| outwards step by step:
%
\begin{spec}
      eval as x = f0 + {-"\int_0^x "-} (eval (g as) t) dt
<=>   eval as x = eval (integ f0 (g as)) x
<==   as = integ f0 (g as)
\end{spec}
%
Finally, we have arrived at an equation expressed in only syntactic
operations, which is implementable in Haskell (for a reasonable |g|).
%

Which functions |g| commute with |eval|?
%
All the ones in |Ring|, |Field|, |Transcendental|, by construction;
additionally, as above, |deriv| and |integ|.
%
Therefore, we can implement a general solver for these simple
equations:
%
\begin{code}
solve :: Field a => a -> (PowerSeries a -> PowerSeries a) -> PowerSeries a
solve f0 g = f              -- solves |f' = g f|, |f 0 = f0|
  where f = integ f0 (g f)
\end{code}
On the face of it, the solution |f| appears not well defined, because
its definition depends on itself.
%
We come back to this point soon, but first we observe |solve| in
action on simple instances of |g|, starting with |const 1| and |id|:
\begin{code}
idx  ::  Field a => PowerSeries a
idx  =   solve 0 (\_f -> 1)         -- \(f'(x) = 1\), \(f(0) = 0\)

expx  ::  Field a => PowerSeries a
expx  =   solve 1 (\f -> f)         -- \(f'(x) = f(x)\), \(f(0) = 1\)
expf  ::  Field a => a -> a
expf  =   evalPS 100 expx
\end{code}

\begin{exercise}
  Write |expx| as a recursive equation (inline |solve| in the
  definition above).
\end{exercise}
%
The first solution, |idx| is just the polynomial |[0,1]| --- i.e. just
$x$ in usual mathematical notation.
%
We can easily check that its derivative is constantly |1| and its
value at |0| is |0|.

The second solution |expx| is a formal power series representing the
exponential function.
%
It is equal to its derivative and it starts at |1|.
%
The function |expf| is a good approximation of the semantics for small
values of its argument --- the following testing code shows that the
maximum difference in the interval from 0 to 1 is below \(5*10^{16}\)
(very close to the precision of |Double|).
%
\begin{code}
testExp :: Double
testExp = maximum (map diff [0,0.001..1::Double])
  where diff = abs . (expf - exp)  -- using the function instance for |exp|

testExpUnits :: Double
testExpUnits =  testExp / epsilon

epsilon :: Double  -- one bit of |Double| precision
epsilon = last (takeWhile (\x -> 1 + x /= 1) (iterate (/2) 1))
\end{code}

As an alternative to using |solve| we can use recursion directly.
%
For example, we can define sine and cosine in terms of each other:
%
\begin{code}
sinx,  cosx  :: Field a =>  PowerSeries a
sinx  =  integ 0 cosx
cosx  =  integ 1 (-sinx)

sinf,  cosf  :: Field a =>  a -> a
sinf  =  evalPS 100 sinx
cosf  =  evalPS 100 cosx
\end{code}
\begin{exercise}
  Write the differential equations characterising sine and cosine,
  using usual mathematical notation.
\end{exercise}
%
The reason that these definitions produce an output instead of
entering an infinite loop is that Haskell is a lazy language:
|integ| can immediately return the first element of the stream before
requesting any information about its second input.
%
It is instructive to mimic part of what the lazy evaluation machinery
is doing by hand, as follows.
%
We know that both |sinx| and |cosx| are streams, thus we can start by
filling in just the very top level structure:
\begin{spec}
sx = sh : st
cx = ch : ct
\end{spec}
where |sh| \& |ch| are the heads and |st| \& |ct| are the tails of the
two streams.
%
Then we notice that |integ| fills in the constant as the head, and we
can progress to:
%
\begin{spec}
sx  =  0  :  st
cx  =  1  :  ct
\end{spec}
%
%format neg x = "\text{-}" x
%
At this stage we only know the constant term of each power series, but
that is enough for the next step: the head of |st| is |frac 1 1| and
the head of |ct| is |frac (neg 0) 1|:
%
\begin{spec}
sx  =  0  :  1      : _
cx  =  1  :  neg 0  : _
\end{spec}
%
As we move on, we can always compute the next element of one series by
the previous element of the other series (divided by |n|, for |cx|
negated).
%
\begin{code}
sx,cx::[Double]
sx = 0  :  1      :  neg 0           :  frac (neg 1) 6  :  error "TODO"
cx = 1  :  neg 0  :  frac (neg 1) 2  :  0               :  error "TODO"
\end{code}
%

\section{Exponentials and trigonometric functions for |PowerSeries|}

We have now shown how to compute the power series representations of the functions |exp|, |sin|, and |cos|.
%
We have also implemented all the |Field| class operations on power
series.
%
The next step would be to compute the |Transcendental| class
operations directly on the power series representation.
%
For example, can we compute |expPS|?

Specification:

\begin{spec}
eval (expPS as) = exp (eval as)
\end{spec}
Differentiating both sides, we obtain

\begin{spec}
  D (eval (expPS as)) = exp (eval as) * D (eval as)

<=>  {- |eval| morphism -}

  eval (deriv (expPS as)) = eval (expPS as * deriv as)

<==

  deriv (expPS as) = expPS as * deriv as
\end{spec}
%
Now we have reached the form of an ordinary differential equation for
|expPS as|, and we know how to solve them by integration, given the
initial condition.
%
Using |eval (expPS as) 0 == exp (eval as 0) == exp (head as)|, we
obtain
%**TODO: head = val
\begin{spec}
expPS as = integ (exp (head as)) (expPS as * deriv as)
\end{spec}
%
Note: we cannot use |solve| here, because the |g| function uses both
|expPS as| and |as| (it ``looks inside'' its argument).

In the same style we can fill in the |Transcendental| instance declaration for |PowerSeries|:
\begin{code}

instance (Eq a, Transcendental a) => Transcendental (PowerSeries a) where
   pi   =  Poly [pi]
   exp  =  expPS
   sin  =  sinPS
   cos  =  cosPS

expPS, sinPS, cosPS :: (Eq a, Transcendental a) => PowerSeries a -> PowerSeries a
expPS  as  = integ  (exp  (val as))  (exp as   * deriv as)
sinPS  as  = integ  (sin  (val as))  (cos as   * deriv as)
cosPS  as  = integ  (cos  (val as))  (-sin as  * deriv as)

val ::  Additive a => PowerSeries a  ->  a
val (Poly (a:_))   =   a
val _              =   zero
\end{code}

In fact, we can now implement \emph{all} the operations needed for
evaluating |FunExp| functions as power series!
%
\begin{code}
evalP :: (Eq r, Transcendental r) => FunExp -> PowerSeries r
evalP (Const x)    =  Poly [fromRational (toRational x)]
evalP (e1 :+: e2)  =  evalP e1 + evalP e2
evalP (e1 :*: e2)  =  evalP e1 * evalP e2
evalP X            =  idx
evalP (Negate e)   =  negate  (evalP e)
evalP (Recip e)    =  recip   (evalP e)
evalP (Exp e)      =  exp     (evalP e)
evalP (Sin e)      =  sin     (evalP e)
evalP (Cos e)      =  cos     (evalP e)
\end{code}

\section{Associated code}

Here we collect in one place the definitions of |eval| for |FunExp|, syntactic derivative, and syntactic instance declarations for |FunExp|.


\begin{code}
evalFunExp  ::  Transcendental a => FunExp -> a -> a
evalFunExp  (Const alpha)  =   const (fromRational (toRational alpha))
evalFunExp  X              =   id
evalFunExp  (e1 :+: e2)    =   evalFunExp e1  +  evalFunExp e2    -- note the use of ``lifted |+|''
evalFunExp  (e1 :*: e2)    =   evalFunExp e1  *  evalFunExp e2    -- ``lifted |*|''
evalFunExp  (Exp e)        =   exp     (evalFunExp e)             -- and ``lifted |exp|''
evalFunExp  (Sin e)        =   sin     (evalFunExp e)
evalFunExp  (Cos e)        =   cos     (evalFunExp e)
evalFunExp  (Recip e)      =   recip   (evalFunExp e)
evalFunExp  (Negate e)     =   negate  (evalFunExp e)

-- and so on

derive     (Const _)      =  Const 0
derive     X              =  Const 1
derive     (e1 :+: e2)    =  derive e1  :+:  derive e2
derive     (e1 :*: e2)    =  (derive e1  :*:  e2)  :+:  (e1  :*:  derive e2)
derive     (Recip e)      =  let re = Recip e in Negate (re:*:re) :*: derive e
derive     (Negate e)     =  Negate (derive e)
derive     (Exp e)        =  Exp e :*: derive e
derive     (Sin e)        =  Cos e :*: derive e
derive     (Cos e)        =  Const (-1) :*: Sin e :*: derive e

instance Additive FunExp where
  (+)   =  (:+:)
  zero  = Const 0

instance AddGroup FunExp where
  negate x  = Const (-1) * x

instance Multiplicative FunExp where
  (*)  =  (:*:)
  one  = Const 1

instance MulGroup FunExp where
  recip = Recip

instance Transcendental FunExp where
  pi   =  Const pi
  exp  =  Exp
  sin  =  Sin
  cos  =  Cos
\end{code}

\subsection{Not included to avoid overlapping instances}

\begin{spec}
instance Num a => Num (FD a) where
  (f, f') + (g, g')  = (f + g,  f' + g')
  zero               = (zero,   zero)

instance Multiplicative (FD a) where
  (f, f') * (g, g')  = (f * g,  f' * g + f * g')
  one                = (one,    zero)

instance Field a => MulGroup (FD a) where
  (f, f') / (g, g')  = (f / g,  (f' * g - g' * f) / (g * g))

instance Transcendental a => Transcendental (FD a) where
  pi = (pi, zero)
  exp (f, f')        = (exp f,  (exp f) * f')
  sin (f, f')        = (sin f,  (cos f) * f')
  cos (f, f')        = (cos f,  -(sin f) * f')
\end{spec}

\subsection{This is included instead}


\begin{code}
instance Additive a => Additive (a, a) where
  (f, f') + (g, g')  = (f + g,  f' + g')
  zero               = (zero,   zero)

instance AddGroup a => AddGroup (a, a) where
  negate (f, f')     = (negate f, negate f')

instance Ring a => Multiplicative (a,a) where
  (f, f') * (g, g')  = (f * g,  f' * g + f * g')
  one                = (one,zero)

instance Field a => MulGroup (a, a) where
  (f, f') / (g, g')  = (f / g,  (f' * g - g' * f) / (g * g))

instance Transcendental a => Transcendental (a, a) where
  pi = (pi, zero)
  exp  (f, f')       = (exp f,  (exp f) * f')
  sin  (f, f')       = (sin f,  cos f * f')
  cos  (f, f')       = (cos f,  -(sin f) * f')
\end{code}

%include E6.lhs
