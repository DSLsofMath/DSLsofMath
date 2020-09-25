\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DSLsofMath.W06 where
import DSLsofMath.FunExp hiding (eval, f)
import DSLsofMath.W05
import DSLsofMath.Algebra
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..))
import Prelude (abs)
\end{code}

\section{Higher-order Derivatives and their Applications}
\label{sec:deriv}

\subsection{Review}
\jp{I don't get it. More words are needed in the itemize. Is this
  connected with what follows? It seems that the review ends before
  the end of the subsection. It seems that the flow goes all over the
  place. Using an example or generalising, using several methods,
  etc. I have a really hard time following where one is going, perhaps
  the whole sub section should be rewritten.}
\begin{itemize}
\item key notion \emph{homomorphism}: |S1 -> S2| (read ``from |S1| to |S2|'')

\item questions (``equations''):

  \begin{itemize}
  \item  |S1 ->? S2|     what is the homomorphism between two given structures

        - e.g., |apply c : Num (x -> a) -> Num a|

  \item |S1?? -> S2|    what is |S1| compatible with a given homomorphism

        - e.g., |eval : Poly a -> (a -> a)|

  \item |S1 -> S2??|   what is |S2| compatible with a given homomorphism

        - e.g.,   |applyFD c : FD a -> (a, a)|

  \item |S1 ->? S2??|   can we find a good structure on |S2| so that it becomes homomorphic w. |S1|?

        - e.g.,   |evalD : FunExp -> FD a|
  \end{itemize}
\end{itemize}

The importance of |applyFD| (from \cref{sec:applyFD}) and |evalD| (\refSec{sec:evalD}) lies in that they offer ``automatic
differentiation'', i.e., any function constructed according to the
grammar of |FunExp|, can be ``lifted'' to a function that computes the
derivative (e.g., a function on pairs).




\begin{example}
\begin{code}
f :: Transcendental a => a -> a
f x = sin x + 2 * x
\end{code}
%
We have: |f 0 = 0|, |f 2 = 4.909297426825682|, etc.

To compute the derivative at some point, say |2|, we have several choices.

\begin{enumerate}
\item Using |FunExp|

Recall (\refSec{sec:FunExp}):
%
\begin{spec}
data FunExp  =  Const Rational
             |  Id
             |  FunExp :+: FunExp
             |  FunExp :*: FunExp
             |  FunExp :/: FunExp
             |  Exp FunExp
             |  Sin FunExp
             |  Cos FunExp
                -- and so on
  deriving (Eq, Show)
\end{spec}
%
What is the expression |e| for which |f = eval e|?

We have
%
\begin{spec}
        eval e x = f x
<=>     eval e x = sin x + 2 * x
<=>     eval e x = eval (Sin Id) x + eval (Const 2 :*: Id) x
<=>     eval e x = eval ((Sin Id) :+: (Const 2 :*: Id)) x
<==     e = Sin Id :+: (Const 2 :*: Id)
\end{spec}
%
Finally, we can apply |derive| and obtain
%
\begin{code}
e = Sin Id :+: (Const 2 :*: Id)
f' 2 = evalFunExp (derive e) 2
\end{code}
%
This can hardly be called ``automatic'', look at all the work we did in
deducing |e|!
%
However, consider this definition:
%
\begin{code}
e2 :: FunExp
e2 = f Id
\end{code}
%
As |Id :: FunExp|, the Haskell interpreter will look for |FunExp| instances of |Num|
and other numeric classes and build the syntax tree for |f| instead of computing its
semantic value.
%
(Perhaps it would have been better to use, in the definition of
|FunExp|, the constructor name |X| instead of |Id|.)

In general, to find the derivative of a function |f :: Transcendental a => a -> a|, we can use
%
\begin{code}
drv f = evalFunExp (derive (f Id))
\end{code}

\item Using |FD| (pairs of functions)

Recall
%
\begin{code}
type FD a = (a -> a, a -> a)

applyFD x (f, g) = (f x, g x)
\end{code}
%
The operations (the numeric type class instances) on |FD a| are such that, if |eval e = f|, then
%
\begin{spec}
(eval e, eval' e) = (f, f')
\end{spec}
%
We are looking for |(g, g')| such that
%
\begin{spec}
f (g, g') = (f, f')   -- (*)
\end{spec}
%
so we can then do
%
\begin{spec}
f' 2 = snd (applyFD 2 (f (g, g')))
\end{spec}
%
We can fullfill (*) if we can find a pair |(g, g')| that is a sort of
``unit'' for |FD a|:
%
\begin{spec}
sin (g, g') = (sin, cos)
exp (g, g') = (exp, exp)
\end{spec}
%
and so on.

In general, the chain rule gives us
%
\begin{spec}
f (g, g') = (f . g, (f' . g) * g')
\end{spec}
%
Therefore, we need: |g = id| and |g' = const 1|.

Finally
%
\begin{spec}
f' 2 = snd (applyFD 2 (f (id, const 1)))
\end{spec}
%
In general
%
\begin{code}
drvFD f x = snd (applyFD x (f (id, const 1)))
\end{code}
%
computes the derivative of |f| at |x|.
%
\begin{code}
f1 :: FD Double -> FD Double
f1  = f
\end{code}

\item Using pairs.

We have |instance Transcendental a => Transcendental (a, a)|, moreover, the
instance declaration looks exactly the same as that for |FD a|:
%
\begin{spec}
instance Transcendental a => Transcendental (FD a) where  -- pairs of functions
  exp (f, f')       =  (exp f, (exp f) * f')
  sin (f, f')       =  (sin f, (cos f) * f')
  cos (f, f')       =  (cos f, -(sin f) * f')

instance Transcendental a => Transcendental (a, a) where  -- just pairs
  exp (f, f')       =  (exp f,  (exp f) * f')
  sin (f, f')       =  (sin f,   cos f  * f')
  cos (f, f')       =  (cos f, -(sin f) * f')
\end{spec}
%
In fact, the latter (just pairs) represents a generalisation of the former (pairs of functions).
%
To see this, note that if we have a |Transcendental| instance for some |A|,
we get a floating instance for |x->A| for all |x| from the module |FunNumInst|\jp{What is this module? Was it ever introduced?}.
%
Then from the instance for pairs we get an instance for any type of
the form |(x->A, x->A)|.
%
As a special case when |x=A| this includes all |(A->A, A->A)| which is
|FD A|.
%
Thus it is enough to have |FunNumInst| and the pair instance to get
the ``pairs of functions'' instance (and more).

The pair instance is also the ``maximally general'' such
generalisation (discounting the ``noise'' generated by the
less-than-clean design of |Num|, |Fractional|, |Transcendental|).

Still, we need to use this machinery.
%
We are now looking for a pair of values |(g, g')| such that
%
\begin{spec}
f (g, g') = (f 2, f' 2)
\end{spec}
%
In general
%
\begin{spec}
f (g, g') = (f g, (f' g) * g')
\end{spec}
%
Therefore
%
\begin{spec}
      f (g, g') = (f 2, f' 2)

<=>   (f g, (f' g) * g') = (f 2, f' 2)
<==   g = 2, g' = 1
\end{spec}
%
Introducing
%
\begin{code}
var x = (x, 1)
\end{code}
%
we can, as in the case of |FD|, simplify matters a little:
%
\begin{spec}
f' x = snd (f (var x))
\end{spec}
%
In general
%
\begin{code}
drvP f x  =  snd (f (x, 1))
\end{code}
%
computes the derivative of |f| at |x|.
%
\begin{code}
f2 :: (Double, Double) -> (Double, Double)
f2  = f
\end{code}

\end{enumerate}
\end{example}\jp{That's probably a bad structure: the example is very long.}

We have seen three different ways to use a generic |f :: Transcendental a =>
a -> a| to compute |f' 2|:
\begin{itemize}
\item fully symbolic (using |FunExp|),
\item using pairs of functions (|FD|),
\item or just pairs of values.
\end{itemize}


\subsection{Higher-order derivatives}
Consider
%
\begin{spec}
[f, f', f'', ...] :: [a -> a]
\end{spec}
%
representing the evaluation of an expression (of one variable $x$) as a function, and all its derivatives:
%
\begin{code}
evalAll e = (evalFunExp e) : evalAll (derive e)
\end{code}
%
Notice that, if
%
\begin{spec}
[f, f', f'', ...] = evalAll e
\end{spec}
%
then
%
\begin{spec}
[f', f'', ...] = evalAll (derive e)
\end{spec}
%
Thus |evalAll (derive e) == tail (evalAll e)| which can be written
|evalAll . derive = tail . evalAll|.
%
Thus |evalAll| is a homomorphism from |derive| to |tail|, or in other words, |H1(evalAll,derive,tail)| (defined in \cref{exc:homomorphisms}).

We want to define the other operations on lists of functions in such a way
that |evalAll| is a homomorphism.
%
For example:
%
\begin{spec}
evalAll (e1 :*: e2) = evalAll e1 * evalAll e2
\end{spec}
%
where the |(*)| sign stands for the multiplication of infinite lists of
functions, the operation we are trying to determine.
%
We assume that we have already derived the definition of |+| for these
lists (it is |zipWith (+)| --- and because the lists are infinite one needs not worry about differing lengths).

We have the following (writing |eval| for |evalFunExp| and |d| for |derive| in order
to save ink):
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
    (eval e1 * eval e2) : evalAll (d e1 :*: e2 :+: e1 * d e2)
= {- we assume |H2(evalAll, (:+:), (+))| -}
    (eval e1 * eval e2) : (evalAll (d e1 :*: e2) + evalAll (e1 :*: d e2))
\end{spec}

Similarly, starting from the other end we get
%
\begin{spec}
    evalAll e1 * evalAll e2
=
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
    evalAll (d e1 :*: e2) + evalAll (e1 * d e2)
=?
    help a b (evalAll (d e1)) (evalAll (d e2))
\end{spec}

Informally, we can refer to (co-)induction at this point\jp{what does this mean here?} and rewrite
|evalAll (d e1 :*: e2)| to |evalAll (d e1) * evalAll e2|.
%
We also have |evalAll . d = tail . evalAll| which leads to:
%
\begin{spec}
    tail (evalAll e1) * evalAll e2 + evalAll e1 * tail (evalAll e2)
=?
    help a b (tail (evalAll e1)) (tail (evalAll e2))
\end{spec}
%
Finally we rename common subexpressions:
let |a:as = evalAll e1| and |b:bs = evalAll e2|.
\begin{spec}
    tail (a:as) * (b:bs)  +  (a:as) * tail (b:bs)
=?
    help a b (tail (a:as)) (tail (b:bs))
\end{spec}
%
This equality is clearly solved by defining |help| as follows:
%
\begin{code}
help a b as bs = as * (b : bs) + (a : as) * bs
\end{code}
%
Thus, we can eliminate |help| to arrive at a definition for multiplication:
%
\begin{code}
mulStream (a : as) (b : bs) = (a*b) :  (as * (b : bs) + (a : as) * bs)
\end{code}
\jp{This is different from  |polyMul|, but this is because the coefficients here are "bigger". In fact,
we compute several times the same thing here and add them together. }
As in the case of pairs, we find that we do not need any properties of
functions, other than their |Num| structure, so the definitions apply
to any infinite list of |Num a|:
%
\todo{Rename |Stream| to |Taylor| and include UnusualStream.hs here}
\begin{code}
type Stream a = [a]
instance Additive a => Additive (Stream a) where
  (+) = addStream
instance Ring a => Multiplicative (Stream a) where
  (*) = mulStream

addStream :: Additive a => Stream a -> Stream a -> Stream a
addStream (a : as)  (b : bs)  =  (a + b)  :  (as + bs)

mulStream :: Ring a => Stream a -> Stream a -> Stream a
\end{code}

\begin{exercise}
Complete the instance declarations for |Fractional| and
|Transcendental|.
\end{exercise}
%
Note that it may make more sense to declare a |newtype| for |Stream a|
first, for at least two reasons.
%
First, because the type |[a]| also contains finite lists, but we use it
here to represent only the infinite lists (also known as streams).
%
Second, because there are competing possibilities for |Num| instances
for infinite lists, for example applying all the operations
``pointwise'' as with ``FunNumInst''.
%
We used just a type synonym here to avoid cluttering the definitions
with the newtype constructors.

%
Write a general derivative computation, similar to |drv| functions
above:
%
\begin{code}
drvList k f x = undefined    -- |k|th derivative of |f| at |x|
\end{code}

\begin{exercise}
  Compare the efficiency of different ways of computing derivatives.
  \jp{This is a pretty tough exercise...}
\end{exercise}
%

\subsection{Polynomials}
\jp{What is this doing here? and not in the relevant chapter?}
\begin{spec}
data Poly a  =  Single a  |  Cons a (Poly a)
                deriving (Eq, Ord)

evalPoly ::  Num a => Poly a -> a -> a
evalPoly (Single a)     x   =  a
evalPoly (Cons a as)    x   =  a + x * evalPoly as x
\end{spec}

\subsection{Formal power series}
\jp{Are we saying anything non-trivial and new in this section? If so indicate what it is right away.}
As we mentioned above, the Haskell list type contains both finite and
infinite lists.
%
The same holds for the type |Poly| that we designed as ``syntax'' for
polynomials.
%
Thus we can reuse that type also as ``syntax for power series'':
potentially infinite ``polynomials''.
%
\begin{spec}
type PowerSeries a = Poly a -- finite and infinite non-empty lists
\end{spec}
%
Now we can divide, as well as add and multiply.

We can also compute derivatives:
%
\begin{spec}
deriv (Single a)   =  Single 0
deriv (Cons a as)  =  deriv' as 1
  where  deriv' (Single a)   n  =  Single  (n * a)
         deriv' (Cons a as)  n  =  Cons    (n * a)  (deriv' as (n+1))
\end{spec}
%
and integrate:
%
%TODO: Perhaps swap the order of arguments to |integ| to match the order of |Cons|. Or remove that argument and just use |a0 +| in combination with a 1-arg. |integ|.
\begin{code}
integ  ::  Field a => a -> PowerSeries a -> PowerSeries a
integ  a0 as  =  Cons a0 (integ' as 1)
  where  integ' (Single a)   n  =  Single  (a / n)
         integ' (Cons a as)  n  =  Cons    (a / n)  (integ' as (n+1))
\end{code}
%
Note that |a0| is the constant that we need due to indefinite integration.
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

For any |n|, the prefix sum, |s n|, is finite and it is easy to see
that the derivative and integration operations are well defined.
%
We we take the limit, however, the sum may fail to converge for
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

\subsection{Simple differential equations}

Many first-order differential equations have the structure
%
\begin{spec}
f' x = g f x, {-"\qquad"-} f 0 = f0
\end{spec}
%
i.e., they are defined in terms of the higher-order function |g|.

The fundamental theorem of calculus gives us
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
All the ones in |Num|, |Fractional|, |Transcendental|, by construction;
additionally, as above, |deriv| and |integ|.

Therefore, we can implement a general solver for these simple
equations:
%
\begin{code}
solve :: Field a => a -> (PowerSeries a -> PowerSeries a) -> PowerSeries a
solve f0 g = f              -- solves |f' = g f|, |f 0 = f0|
  where f = integ f0 (g f)
\end{code}
On the face of it, the solution |f| appears not well defined, because its definition depends on itself. 
%
We come back to this point soon, but first we observe |solve| in action on simple instances of |g|,
starting with |const 1| and |id|:
\jp{It'd be helpful to restate what the differential equation looks like using usual mathematical notation.}
\begin{code}
idx  ::  Field a => PowerSeries a
idx  =   solve 0 (\f -> 1)
idf  ::  Field a => a -> a
idf  =   eval 100 idx

expx :: Field a => PowerSeries a
expx = solve 1 (\f -> f)
expf :: Field a => a -> a
expf = eval 100 expx
\end{code}
\begin{exercise}
  Write |expx| as a recursive equation (inline |solve| in the definition above).
\end{exercise}
\jp{|eval| was declared |REAL -> REAL| above. Which eval is this referring to?}
%
The first solution, |idx| is just the polynomial |[0,1]| --- i.e. just $x$ in usual mathematical notation.
%
We can easily check that its derivative is constantly |1| and its
value at |0| is |0|.
%
The function |idf| is just there to check that the semantics behaves
as expected.\jp{So what should we check? Is this meant to be a sanity check? Then we should print something?}

The second solution |expx| is a formal power series representing the
exponential function.
%
It is equal to its derivative and it starts at |1|.
%
The function |expf| is a good approximation of the semantics for small values of its argument.

\begin{code}
testExp :: Double
testExp = maximum $ map diff [0,0.001..1::Double]
  where diff = abs (expf - exp)  -- using the function instances for |abs| and |exp|
testExpUnits :: Double
testExpUnits =  testExp / epsilon
epsilon :: Double  -- one bit of |Double| precision
epsilon = last $ takeWhile (\x -> 1 + x /= 1) (iterate (/2) 1)
\end{code}

We can also\jp{why ``also''?} use mutual recursion to define sine and cosine in terms of
each other:
%
\begin{code}
sinx = integ 0 cosx
cosx = integ 1 (-sinx)
sinf = eval 100 sinx
cosf = eval 100 cosx

sinx, cosx :: Field a =>  PowerSeries a
sinf, cosf :: Field a =>  a -> a
\end{code}
\begin{exercise}
  Write the differential equations characterising sine and cosine,
  using usual mathematical notation.
\end{exercise}
%
The reason that these definitions ``work'' (in the sense of not
looping) is because |integ| immediately returns the first element of
the stream before requesting any information about its first input.
%
It is instructive to mimic part of what the lazy evaluation machinery
is doing ``by hand'' as follows.
%
We know that both |sinx| and |cosx| are streams, thus we can start
by filling in just the very top level structure:
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
that is enough for the next step: the head of |st| is |frac 1 1| and the
head of |ct| is |frac (neg 0) 1|:
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

\subsection{The |Field| structure of |PowerSeries|}

Can we compute |exp as|?\jp{Compute in what sense? We already have |expx| in the above section. Does this mean using the differential rather than the integral? I don't get the point.}

Specification:

\begin{spec}
eval (exp as) = exp (eval as)
\end{spec}
Differentiating both sides, we obtain\jp{What definition of |D| are we using?}

\begin{spec}
  D (eval (exp as)) = exp (eval as) * D (eval as)

<=>  {- |eval| morphism -}

  eval (deriv (exp as)) = eval (exp as * deriv as)

<==

  deriv (exp as) = exp as * deriv as
\end{spec}

Adding the ``initial condition'' |eval (exp as) 0 = exp (head as)|, we
obtain\jp{What is this coming from? Why suddenly using |head|? I am lost.}
%**TODO: head = val
\begin{spec}
exp as = integ (exp (head as)) (exp as * deriv as)
\end{spec}

Note: we cannot use |solve| here, because the |g| function uses both
|exp as| and |as| (it ``looks inside'' its argument).


\jp{So what's happening now? Why this code suddenly?}
\begin{code}
instance (Eq a, Transcendental a) => Transcendental (PowerSeries a) where
  pi   =  Single pi
  exp  =  expPS
  sin  =  sinPS
  cos  =  cosPS

expPS, sinPS, cosPS :: (Eq a, Transcendental a) => PowerSeries a -> PowerSeries a
expPS  fs  =  integ  (exp  (val fs))  (exp fs   * deriv fs)
sinPS  fs  =  integ  (sin  (val fs))  (cos fs   * deriv fs)
cosPS  fs  =  integ  (cos  (val fs))  (-sin fs  * deriv fs)

val ::  PowerSeries a  ->  a
val     (Single a)     =   a
val     (Cons a as)    =   a
\end{code}

In fact, we can implement \emph{all} the operations needed for
evaluating |FunExp| functions as power series! \jp{Wasn't it done already when first talking about power series? There does not seem to be anything pertaining derivatives here.}

\begin{code}
evalP :: (Eq r, Transcendental r) => FunExp -> PowerSeries r
evalP (Const x)    =  Single (fromRational (toRational x))
evalP (e1 :+: e2)  =  evalP e1 + evalP e2
evalP (e1 :*: e2)  =  evalP e1 * evalP e2
evalP (e1 :/: e2)  =  evalP e1 / evalP e2
evalP Id           =  idx
evalP (Exp e)      =  exp (evalP e)
evalP (Sin e)      =  sin (evalP e)
evalP (Cos e)      =  cos (evalP e)
\end{code}

\subsection{Taylor series}

If |f = eval [a0, a1, ..., an, ...]|, then\jp{Which eval is that, and what is the meaning of the list here? polynomial? power series? derivatives?}

\begin{spec}
   f 0    =  a0
   f'     =  eval (deriv [a0, a1, ..., an, ...])
          =  eval ([1 * a1, 2 * a2, 3 * a3, ..., n * an, ...])
=>
   f' 0   =  a1
   f''    =  eval (deriv [a1, 2 * a2, ..., n * an, ...])
          =  eval ([2 * a2, 3 * 2 * a3, ..., n * (n - 1) * an, ...])
=>
   f'' 0  =  2 * a2
\end{spec}

In general:

\begin{spec}
   {-"f^{(k)} "-} 0  =  fact k * ak
\end{spec}

Therefore

\begin{spec}
   f      =  eval [f 0, f' 0, f'' 0 / 2, ..., {-"f^{(n)} "-} 0 / (fact n), ...]
\end{spec}

That is, there is a simple mapping between the representation of |f|
as a power series (the coefficients |a_k|), and the value of all
derivatives of |f| at |0| (our |Stream a| type above).

The series |[f 0, f' 0, f'' 0 / 2, ..., {-"f^{(n)} "-} 0 / (fact n), ...]| is
called the Taylor series centred in |0|, or the Maclaurin series. 

\begin{code}
derivs :: Ring a => PowerSeries a -> PowerSeries a
derivs as = derivs1 as 0 1
  where
  derivs1 (Cons a as)  n factn  =  Cons    (a * factn)
                                           (derivs1 as (n + 1) (factn * (n + 1)))
  derivs1 (Single a)   n factn  =  Single  (a * factn)

-- remember that |x = Cons 0 (Single 1)|
ex3,ex4 :: Poly Double
ex3 = takePoly 10 (derivs (x^3 + 2 * x))
ex4 = takePoly 10 (derivs sinx)
\end{code}

In this way, we can compute all the derivatives at |0| for all
functions |f| constructed with the grammar of |FunExp|.
%
That is because, as we have seen, we can represent all of them by
power series!

What if we want the value of the derivatives at |a /= 0|?

We then need the power series of the ``shifted'' function g:

\begin{spec}
g x  =  f (x + a)  <=>  g = f . (+ a)
\end{spec}

If we can represent |g| as a power series, say |[b0, b1, ...]|, then
we have

\begin{spec}
{-"g^{(k)} "-} 0  =  fact k * bk  =  {-"f^{(k)} "-} a
\end{spec}

In particular, we would have

\begin{spec}
f x  =  g (x - a)  =  {-"\sum"-} bn * (x - a){-"^n"-}
\end{spec}

which is called the Taylor expansion of |f| at |a|.

Example:

We have that |idx = [0, 1]|, thus giving us indeed the values

\begin{spec}
[id 0, id' 0, id'' 0, ...]
\end{spec}

In order to compute the values of

\begin{spec}
[id a, id' a, id'' a, ...]
\end{spec}

for |a /= 0|, we compute

\begin{code}
ida a = takePoly 10 (derivs (evalP (Id :+: Const a)))
\end{code}

More generally, if we want to compute the derivative of a function |f|
constructed with |FunExp| grammar, at a point |a|, we need the power
series of |g x = f (x + a)|:

\begin{code}
d f a = takePoly 10 (derivs (evalP (f (Id :+: Const a))))
\end{code}

Use, for example, our |f x = sin x + 2 * x| above.

As before, we can use directly power series:

\begin{code}
dP f a = takePoly 10 (derivs (f (idx + Single a)))
\end{code}

\subsection{Associated code}

\jp{Feels like this should be moved upwards as the concepts are introduced}

\begin{code}
evalFunExp  ::  Transcendental a => FunExp -> a -> a
evalFunExp  (Const alpha)  =   const (fromRational (toRational alpha))
evalFunExp  Id             =   id
evalFunExp  (e1 :+: e2)    =   evalFunExp e1  +  evalFunExp e2    -- note the use of ``lifted |+|''
evalFunExp  (e1 :*: e2)    =   evalFunExp e1  *  evalFunExp e2    -- ``lifted |*|''
evalFunExp  (Exp e1)       =   exp (evalFunExp e1)                -- and ``lifted |exp|''
evalFunExp  (Sin e1)       =   sin (evalFunExp e1)
evalFunExp  (Cos e1)       =   cos (evalFunExp e1)
-- and so on

derive     (Const alpha)  =  Const 0
derive     Id             =  Const 1
derive     (e1 :+: e2)    =  derive e1  :+:  derive e2
derive     (e1 :*: e2)    =  (derive e1  :*:  e2)  :+:  (e1  :*:  derive e2)
derive     (Exp e)        =  Exp e :*: derive e
derive     (Sin e)        =  Cos e :*: derive e
derive     (Cos e)        =  Const (-1) :*: Sin e :*: derive e

instance Additive FunExp where
  (+)  =  (:+:)
  zero = Const 0

instance AddGroup FunExp where
  negate x  = Const (-1) * x

instance Multiplicative FunExp where
  (*)  =  (:*:)
  one  = Const 1

instance MulGroup FunExp where
  (/)  =  (:/:)

instance Transcendental FunExp where
  exp        =  Exp
  sin        =  Sin
  cos        =  Cos
\end{code}

\subsubsection{Not included to avoid overlapping instances}

\begin{spec}
instance Num a => Num (FD a) where
  (f, f') + (g, g')  = (f + g, f' + g')
  zero = (zero, zero)

instance Multiplicative (FD a) where
  (f, f') * (g, g')  = (f * g, f' * g + f * g')
  one = (one, zero)

instance Field a => MulGroup (FD a) where
  (f, f') / (g, g')  = (f / g, (f' * g - g' * f) / (g * g))

instance Transcendental a => Transcendental (FD a) where
  exp (f, f')        =  (exp f, (exp f) * f')
  sin (f, f')        =  (sin f, (cos f) * f')
  cos (f, f')        =  (cos f, -(sin f) * f')
\end{spec}

\subsubsection{This is included instead}


\begin{code}
instance Additive a => Additive (a, a) where
  (f, f') + (g, g')  = (f + g, f' + g')
  zero = (zero,zero)

instance AddGroup a => AddGroup (a, a) where
  negate (f, f') = (negate f, negate f')

instance Ring a => Multiplicative (a,a) where
  (f, f') * (g, g')  = (f * g, f' * g + f * g')
  one = (one,zero)

instance Field a => MulGroup (a, a) where
  (f, f') / (g, g')  = (f / g, (f' * g - g' * f) / (g * g))

instance Transcendental a => Transcendental (a, a) where
  exp (f, f')        =  (exp f, (exp f) * f')
  sin (f, f')        =  (sin f, cos f * f')
  cos (f, f')        =  (cos f, -(sin f) * f')
\end{code}

\jp{What about log?}
\jp{What about adding composition to the language of functions? See attempt in Taylor.hs}


%include E6.lhs
