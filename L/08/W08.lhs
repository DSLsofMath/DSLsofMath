\chapter{Exponentials and Laplace}
\label{sec:Laplace}

\section{The Exponential Function}
\label{sec:exp}
%if False
\begin{code}
{-# LANGUAGE RebindableSyntax, TypeSynonymInstances #-}
module DSLsofMath.W08 where
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..),sum)
import DSLsofMath.W05
import qualified DSLsofMath.W06
import DSLsofMath.W06 (Taylor, integ, sinx, sinf, cosx, cosf)
import DSLsofMath.Algebra
import Data.Complex
\end{code}
%endif

In one of the classical analysis textbooks, \citet{rudin1987real}
starts with a prologue on the exponential function.
%
The first sentence is

\begin{quote}
This is undoubtedly the most important function in mathematics.
\end{quote}
%
Rudin goes on
%
\index{exp@@|exp|{}}
%
\begin{quote}
It is defined, for every complex number |z|, by the formula
\[
         exp(z)  =  \sum_{n=0}^{\infty} \frac{z^n}{n!}
\]
\end{quote}
%
We, on the other hand, have defined the exponential function as the
solution of a differential equation, which can be represented by
a power series:
\begin{code}
expx :: Field a => PowerSeries a
expx = integ 1 expx
\end{code}

and approximated by

\begin{code}
expf :: Field a => a -> a
expf = evalPS 100 expx
\end{code}

It is easy to see, using the definition of |integ| that the power
series |expx| is, indeed
\begin{spec}
expx = Poly [1, 1/1, 1/2, 1/6, ..., 1 / (1*2*3* ... *n), ..]
\end{spec}

We can compute the exponential for complex values if we can give an
instance of |Field| for complex numbers.
%
We use the datatype |Data.Complex| from the Haskell standard library,
which is isomorphic to the implementation from \cref{sec:DSLComplex}.
%
In |Data.Complex| a complex value |z| is represented by two values,
the real and the imaginary part, connected by an infix constructor: |z
= re :+ im|.
%
\begin{code}
i :: Ring a => Complex a
i = zero :+ one
\end{code}

Therefore we can define, for example, the exponential of the imaginary
unit:
%
\begin{code}
ex1 :: Field a => Complex a
ex1 = expf i
\end{code}
%
And we have |ex1 == 0.5403023058681398 :+ 0.8414709848078965|.
%
Observe at the same time:
%
\begin{spec}
cosf 1  =  0.5403023058681398
sinf 1  =  0.8414709848078965
\end{spec}
%
and therefore |expf i == cosf 1 :+ sinf 1|.
%
This is no coincidence, as we shall see.

First we define a helper function |compScale| which scales the input
to a function by a factor.
%
It is specified by 
%
|eval (compScale c as) x = eval as (c*x)|.
% eval (compScale c as) = (eval as) . (c*)
% eval (compScale c as) = (.(c*)) (eval as)
% eval . compScale c = (.(c*)) . eval
%
Then we define the power series of 
%
\(f(x) = e^{i x}\) as |compScale i expx|.
\begin{code}
compScale :: Ring a => a -> PowerSeries a -> PowerSeries a
compScale c (Poly as) = Poly (zipWith (*) as (iterate (c*) 1))

type PSC a = PowerSeries (Complex a)
expix :: Field a => PSC a
expix = compScale i expx

cosxisinx :: Field a => PSC a
cosxisinx = cosx + Poly [i] * sinx
\end{code}
%
As the code is polymorphic in the underlying number type, we can use
rationals to be able to test for equality without rounding problems.
%
We can see that every second coefficient is real and every second is
imaginary:
\begin{code}
ex2, ex2' :: Field a => PSC a
ex2   = takePoly 8 expix
ex2'  = takePoly 8 cosxisinx

test2 :: Bool
test2 = ex2 == (ex2' :: PSC Rational)

check2 :: Bool
check2 = ex2 == coeff2
\end{code}
\begin{code}
coeff2 ::  PSC Rational
coeff2 =   Poly [      1            :+ 0, {-"\qquad"-}  0 :+     1,
                   (-  1  /  2)     :+ 0, {-"\qquad"-}  0 :+ (-  1  /  6),
                       1  /  24     :+ 0, {-"\qquad"-}  0 :+     1  /  120,
                   (-  1  /  720)   :+ 0, {-"\qquad"-}  0 :+ (-  1  /  5040)]
\end{code}

We can see that the real part of this series is the same as
\begin{code}
ex2R :: Poly Rational
ex2R = takePoly 8 cosx
\end{code}
and the imaginary part is the same as
\begin{code}
ex2I :: Poly Rational
ex2I = takePoly 8 sinx
\end{code}
%
Therefore, the coefficients of |cosx| are
\begin{spec}
[1, 0, -1/2!, 0, 1/4!, 0, -1/6!, ...]
\end{spec}
In other words, the power series representation of the coefficients for |cos| is
%
\begin{spec}
cosa (2 * n)  = (-1)^n / (2 * n)!
cosa (2*n+1)  = 0
\end{spec}
and the terms of |sinx| are
\begin{spec}
[0, 1, 0, -1/3!, 0, 1/5!, 0, -1/7!, ...]
\end{spec}
i.e., the corresponding function for |sin| is
\begin{spec}
sina (2 * n)  = 0
sina (2*n+1)  = (-1)^n / (2*n+1)!
\end{spec}
This can be proven from the definitions of |cosx| and |sinx|.

\paragraph{Euler's formula and periodic functions}

From this we obtain \emph{Euler's formula}:

\begin{spec}
exp (i*x) = cos x + i*sin x
\end{spec}


One thing which comes out of Euler's formula is the fact that the
exponential is a \emph{periodic function} along the imaginary axis.
%
A function |f : A -> B| is said to be periodic if there exists |T ∈ A|
such that
\begin{spec}
f x = f (x + T)  --  |∀ x ∈ A|
\end{spec}
Therefore, for this definition to make sense, we need addition on
|A|; in fact we normally assume at least |AddGroup A|.
%
\index{AddGroup@@|AddGroup| (type class)}%

Since |sin| and |cos| are periodic, with period |tau = 2 * pi|, we have,
using the standard notation |a+i*b| for some |z = a :+ b|:
%
\begin{spec}
  exp(z + i*tau)                              = {- Def. of |z| -}

  exp((a + i * b) + i*tau)                    = {- Assoc. + distrib. -}

  exp(a + i * (b + tau))                      = {- |H2(exp, (+), (*))| -}

  exp a * exp (i * (b + tau)  )               = {- Euler's formula -}

  exp a * (cos (b+tau) + i * sin (b+tau))     = {- |cos| and |sin| are |tau|-periodic -}

  exp a * (cos b + i * sin b)                 = {- Euler's formula -}

  exp a * exp (i*b)                           = {- |exp| is a homomorphism -}

  exp (a + i * b)                             = {- Def. of |z| -}

  exp z
\end{spec}
%\jp{Was it ever proven that exp is a homomorphism in this way? No - but not all proofs are included anyway.}
Thus, we see that |exp| is periodic, because |exp z = exp (z + T)|
with |T = i*tau|, for all |z|.
%\jp{Question: are all periodic functions also periodic in the taylor series space?}

\paragraph{Taylor meets transcendental functions}
In \cref{sec:expPS} we saw that we could make power series instances
for |Transcendental|, including |exp|, |sin|, and |cos|.
%
We can do the same for the Taylor series representation: |[f, f', f'',
  ...]|.
%
With this representation we have very easy implementations of
derivative, integral, and value at zero:
\begin{code}
derivT :: Taylor a -> Taylor a
derivT = tail

integT :: a -> Taylor a -> Taylor a
integT = (:)

val ::  Additive a => Taylor a  ->  a
val (a:_)   =   a
val _       =   zero
\end{code}
%
We can borrow the same derivation of the differential equations
defining |expT|, |sinT|, and |cosT| recursively, just replacing
|integ| with |integT|, etc.
%
\begin{code}
expT, sinT, cosT :: Transcendental a => Taylor a -> Taylor a
expT  as  = integT  (exp  (val as))  (expT as   * derivT as)
sinT  as  = integT  (sin  (val as))  (cosT as   * derivT as)
cosT  as  = integT  (cos  (val as))  (-sinT as  * derivT as)
\end{code}
%
With this in place we can assemble the instance declaration:
\begin{code}
instance Transcendental a => Transcendental (Taylor a) where
   pi   =  [pi]
   exp  =  expT
   sin  =  sinT
   cos  =  cosT
\end{code}

Only one instance remains: we have not yet defined |recip| for Taylor
series.
%
Here we can use the easy access to the derivative as a help in the
implementation.
%
We want to compute |g == recip f| which is specified by |g*f==one|.
%
We can write |f == head f : tail f == f0 : f'| where |f' :: Taylor a|.
%
If we compute the derivative of the specification we get |derivT (g*f) ==
derivT one| which simplifies to |g'*f+g*f'==0|.
%
From this we can extract |g' == -g*f'/f == -f'*g*g| because dividing
by |f| is the same as multiplying by |recip f == g|.
% 
Thus we can implement the last remaining instance:
\begin{code}
instance Field a => MulGroup (Taylor a) where
  recip = recipStream

recipStream :: Field a => Taylor a -> Taylor a
recipStream [] = error "recipStream: divByZero"
recipStream (f0:f') = g
  where  g   = g0:g'
         g0  = recip f0
         g'  = negate (f' * g*g)
\end{code}
Just to check we can now compute the first few terms in |expT x :
Taylor REAL|:
\begin{code}
expTx :: Taylor REAL
expTx = expT [0,1]
testExpT = take 5 expTx == [1.0, 1.0, 1.0, 1.0, 1.0]
\end{code}
All derivatives are one, as expected.

We can also combine this with the complex number example above:
%
\begin{code}
expix3 :: Taylor (Complex Double)
expix3 = exp (i * x)            where  i  = [0 :+ 1]; x  = [0,1]

ex2alt3 = take 10 expix3
testex2alt3 = ex2alt3 ==  take 10 (concat (repeat [1, i, -1, -i]))

cosxisinx3 :: Taylor (Complex Double)
cosxisinx3 = cos x + i * sin x  where  i  = [0 :+ 1];  x  = [0,1]

ex2'alt3 = take 10 cosxisinx3
\end{code}
%
As with |expTx|, the coefficients in these series are also really
simple: the four values |[1, i, -1, -i]| just repeat forever.

\section{The Laplace transform}

This material was inspired by \citet{quinn2008discovering}, which is
highly recommended reading.
%{
%format * = "\mathbin{\cdot}"
%%format convolution = "\mathbin{\ast}"
%format convolution = "\mathbin{\circledast}"
%
In this section we typeset multiplication as |(*)| to avoid visual
confusion with the convolution operator |(convolution)|.


Consider the differential equation

\begin{spec}
f'' x - 3 * f' x + 2 * f x = exp (3 * x),  f 0 = 1,  f' 0 = 0
\end{spec}

We can solve such equations with the machinery of power series:

\begin{code}
fs :: Field a => PowerSeries a
fs = integ 1 fs'
   where  fs'   = integ 0 fs''
          fs''  = exp3x + 3 * fs' - 2 * fs
exp3x :: Field a => PowerSeries a
exp3x = compScale 3 expx
\end{code}
We have done this by ``zooming in'' on the function |f| and representing
it by a power series, |f x = Sigma an * x^n|.
%
This allows us to reduce the problem of finding a function |f : ℝ → ℝ|
to that of finding a list of coefficients |an|, or equivalently a
function |a : ℕ → ℝ|.
%
Or even, if one wants an approximation only, finding a list of
sufficiently many |a|-values for a good approximation.

Still, recursive equations are not always easy to solve (especially
without a computer), so it's worth looking for alternatives.

When ``zooming in'' we go from |f| to |a|, but we can also look at it
in the other direction: we have ``zoomed out'' from |a| to |f| via an
infinite series:

\newcommand{\sumanxn}{|{-"\sum"-} an * pow x n|}
\newcommand{\intftxt}{|{-"\int"-} (f t) * pow x t dt|}
\begin{tikzcd}
  |a : ℕ → ℝ| \arrow{r}{\sumanxn} & |f : ℝ → ℝ|
\end{tikzcd}

We would like to go one step further

\begin{tikzcd}
  |a : ℕ → ℝ| \arrow{r}{\sumanxn} & |f : ℝ → ℝ| \arrow{r}{??} & |F : ??|
\end{tikzcd}

That is, we are looking for a transformation from |f| to some |F| in a
way which resembles the transformation from |a| to |f|.
%
The analogue of ``sum of an infinite series'' for a continuous
function is an integral:

\begin{tikzcd}
  |a : ℕ → ℝ| \arrow{r}{\sumanxn} & |f : ℝ → ℝ| \arrow{r}{\intftxt} & |F : ??|
\end{tikzcd}

We note that, for the integral |Integ (f t) * pow x t dt| to converge
for a larger class of functions (say, bounded functions\footnote{A
  function is bounded if there exists a bound |B| such that for all
  |x|, |absBar (f x) <= B|.}), we have to limit ourselves to |absBar x
< 1|.
%
Both this condition and the integral make sense for |x ∈ ℂ|, so we
could take

\begin{tikzcd}
  |a : ℕ → ℝ| \arrow{r}{\sumanxn} & |f : ℝ → ℝ| \arrow{r}{\intftxt} & |F : {z || absBar z < 1} → ℂ|
\end{tikzcd}

but let us stick to |ℝ| for now.

Writing, somewhat optimistically
%
\begin{spec}
ℒ f x = Integ (f t) * x^t dt
\end{spec}
%
we can ask ourselves what |ℒ f'| looks like.
%
After all, we want to solve \emph{differential} equations by ``zooming
out''.
%
We have
%
\begin{spec}
ℒ f' x = Integ (f' t) * x^t dt
\end{spec}

Remember that |D (f * g) = D f * g + f * D g|, which we use with |g t
= x^t| so that |D g t = log x * x^t| (note that |t| is the variable
here, not |x|).
%
\begin{spec}
  ℒ f' x                                                       =  {- Def. of |ℒ|-}

  Integ (D f t) * x^t dt                                       =  {- Derivative of product -}

  Integ (D (f t * x^t)) - f t * log x * x^t dt                 =  {- Linearity of \(\int\) -}

  Integ (D (f t * x^t)) dt  -  log x * Integ f t * x^t dt      =  {- Def. of integral to \(\infty\). -}

  {-"\lim_{t \to \infty} "-} (f t * x^t) - (f 0 * x^0)
             - log x * Integ f t * x^t dt                      =  {- |absBar x < 1| -}

  -f 0 - log x * Integ f t * x^t dt                            =  {- Def. of |ℒ| -}

  -f 0 - log x * ℒ f x
\end{spec}

The factor |log x| is somewhat awkward.
%
Let us therefore return to the definition of |ℒ| and operate a change
of variables.
%
First some rewriting:
%
\begin{spec}
  ℒ f x = Integ (f t) * x^t dt                <=>  {- |x = exp (log x)| -}

  ℒ f x = Integ (f t) * (exp (log x))^t dt    <=>  {- |(a^b)^c = a^(b*c)| -}

  ℒ f x = Integ (f t) * exp (log x *t) dt
\end{spec}
%
Since |log x < 0| for |absBar x < 1|, we make the substitution |-s = log
x|.
%
The condition |absBar x < 1| becomes |s > 0| (or, in |ℂ|, |real s > 0|),
and we have

\begin{spec}
ℒ f s = Integ (f t) * exp (-s * t) dt
\end{spec}

This is the definition of the Laplace transform of the function |f|.
%
Going back to the problem of computing |ℒ f'|, we now have

\begin{spec}
  ℒ f' s                      = {- The computation above with |s = -log x|. -}

  - f 0 + s * ℒ f s           {-" "-}
\end{spec}

We have obtained

\begin{spec}
ℒ f' s  =  s * ℒ f s - f 0     -- The "Laplace-D" law
\end{spec}

% ℒ (D f) = id * ℒ f - const (f 0)     -- "Laplace-D" in point-free form
%TODO: Note that we cannot directly express this as a homomorphism:
% not quite H1(ℒ, D, \F -> id * F - const (f 0))  -- we don't have access to |f| here, only |F|
% A minor variation is fine: L0 f = (ℒ f, allD0 f) where allD0 f = f 0 : allD0 (D f)
%   H1(L0, D, \(F,f0:f0s) -> (id * F - const f0,f0s))
% But this would take us too far off track for the course.


From this, we can deduce

\begin{spec}
  ℒ f'' s                               = {- Laplace-D for |f'| -}
  s * ℒ f' s - f' 0                     = {- Laplace-D for |f| -}
  s * (s * ℒ f s - f 0) - f' 0          = {- Simplification -}
  s^2 * ℒ f s - s * f 0 - f' 0
\end{spec}

Exercise~\ref{exc:LaplaceDk}: what is the general formula for |ℒ {-"f^{(k)} "-} s|?

Returning to our differential equation, we have

\begin{spec}
  f'' x - 3 * f' x + 2 * f x = exp (3 * x),  f 0 = 1,  f' 0 = 0

<=>  {- point-free form -}

  f'' - 3 * f' + 2 * f  = exp ∘ (3*),  f 0 = 1,  f' 0 = 0

=>   {- applying |ℒ| to both sides -}

  ℒ (f'' - 3 * f' + 2 * f)  =  ℒ (exp ∘ (3*)), f 0 = 1, f' 0 = 0  -- Eq. (1)
\end{spec}

%


\textbf{Remark:} Note that this is a necessary condition, but not a
sufficient one.
%
The Laplace transform is not injective.
%
For one thing, it does not take into account the behaviour of |f| for
negative arguments.
%
Because of this, we often assume that the domain of definition for
functions to which we apply the Laplace transform is $ℝ_{≥ 0}$.
%
For another, it is known that changing the values of |f| for a
countable number of its arguments does not change the value of the
integral.

According to the definition of |ℒ| and because of the linearity of the
integral, we have that, for any |f| and |g| for which the
transformation is defined, and for any constants |alpha| and |beta|

\begin{spec}
ℒ (alpha *^ f + beta *^ g)  =  alpha *^ ℒ f  + beta *^ ℒ g
\end{spec}

Note that this is an equality between functions.
%
Indeed, recalling \cref{sec:LinAlg}, in particular
\cref{sec:functions-vector-space}, we are working here with the vector
space of functions (|f| and |g| are elements of it).
%
Let us call the space of real functions |V = X -> REAL| (for some |X|)
so that we have |f, g : V|.
%
And let us call the space of functions that |ℒ| returns |W = S -> CC|
for some suitable type |S|.
%
The operator |(*^)| refers to scaling in a vector space --- here
scaling functions in |V| on the LHS and functions in |W| on the RHS.
%
The above equation says that |ℒ| is a linear transformation from |V|
to~|W|.
%
\index{linear transformation}%

Applying this linearity property to the left-hand side of (1), we have
for any |s|:

\begin{spec}
  ℒ (f'' - 3 * f' + 2 * f) s

= {- |ℒ| is linear -}

  ℒ f'' s - 3 * ℒ f' s + 2 * ℒ f s

= {- re-writing |ℒ f''| and |ℒ f'| in terms of |ℒ f| -}

  s^2 * ℒ f s - s * f 0 - f' 0 - 3 * (s * ℒ f s - f 0) + 2 * ℒ f s

= {- |f 0 = 1|, |f' 0 = 0| -}

  (s^2 - 3 * s + 2) * ℒ f s - s + 3

= {- Factoring -}

  (s - 1) * (s - 2) * ℒ f s - s + 3
\end{spec}


For the right-hand side, we apply the definition:

\begin{spec}
  ℒ (exp ∘ (3*)) s                                                                                  = {- Def. of |ℒ| -}

  Integ exp (3 * t)  *  exp (-s * t) dt                                                             =

  Integ exp ((3 - s) * t)) dt                                                                       =

  {-"lim_{t \to \infty} "-}  frac (exp ((3 - s) * t)) (3 - s)  -  frac (exp ((3 - s) * 0)) (3 - s)  = {- for |s > 3| -}

  frac 1 (s - 3)
\end{spec}

Therefore, we have, writing |F| for |ℒ f|:
%
\begin{spec}
(s - 1) * (s - 2) * F s - s + 3 = frac 1 (s-3)
\end{spec}
%
and therefore, by solving for |F s| we get
%
\[
  |F s| = |frac (frac 1 (s - 3) + s - 3) ((s - 1)*(s - 2)| =
  |frac (10 - 6 * s + s^2) ((s-1)*(s-2)*(s-3))|
\]

We now have the problem of ``recovering'' the function |f| from its
Laplace transform.
%
The standard approach is to use the linearity of |ℒ| to write |F| as a
sum of functions with known inverse transforms.
%
We know one such function:

\begin{spec}
\t -> exp (alpha * t)  {- is the inverse Laplace transform of -} \s -> (frac 1 (s - alpha))
\end{spec}

In fact, in our case, this is all we need.

The idea is to write |F s| as a sum of three fractions with
denominators |s - 1|, |s - 2|, and |s - 3| respectively, i.e., to find
|A|, |B|, and |C| such that

\begin{spec}
frac A (s - 1) + frac B (s - 2) + frac C (s - 3) =  frac (10 - 6 * s + s^2) ((s - 1) * (s - 2) * (s - 3))

=> {- Multiply both sides by |(s - 1) * (s - 2) * (s - 3)| -}

A * (s - 2) * (s - 3) + B * (s - 1) * (s - 3) + C * (s - 1) * (s - 2)
  = 10 - 6 * s + s^2                                                     -- (2)
\end{spec}

We need this equality (2) to hold for values |s > 3|.
%
A \emph{sufficient} condition for this is for (2) to hold for \emph{all} |s|.
%
A \emph{necessary} condition for this is for (2) to hold for the
specific values |1|, |2|, and |3|.

\begin{spec}
{-"\text{For }"-} s = 1:    A * (-1) * (-2)  = 10 - 6 + 1   =>  A = frac 5 2
{-"\text{For }"-} s = 2:    B * 1 * (-1)     = 10 - 12 + 4  =>  B = -2
{-"\text{For }"-} s = 3:    C * 2 * 1        = 10 - 18 + 9  =>  C = frac 1 2
\end{spec}

It is now easy to check that, with these values, (2) does indeed hold,
and therefore that we have

\begin{spec}
F s = frac 5 2 * frac 1 (s - 1) - 2 * frac 1 (s - 2) + frac 1 2 * frac 1 (s - 3)
\end{spec}

The inverse transform is now easy:

\begin{spec}
f t = frac 5 2 * exp t - 2 * exp (2 * t) + frac 1 2 * exp (3 * t)
\end{spec}

Our mix of necessary and sufficient conditions makes it necessary to
check that we have, indeed, a solution for the differential equation.
%
To do this we compute the first and second derivatives of |f|:
\begin{spec}
f' t   = frac 5 2 * exp t - 4 * exp (2 * t) + frac 3 2 * exp (3 * t)
f'' t  = frac 5 2 * exp t - 8 * exp (2 * t) + frac 9 2 * exp (3 * t)
\end{spec}
We then check the main equation:
\begin{spec}
  LHS
=
  f'' x - 3 * f' x + 2 * f x
= {- Fill in the computed definitions of |f|, |f'|, and |f''|. -}
        frac 5 2 * exp x - 8 * exp (2 * x) + frac 9 2 * exp (3 * x)
  -3*(  frac 5 2 * exp x - 4 * exp (2 * x) + frac 3 2 * exp (3 * x)  )
  +2*(  frac 5 2 * exp x - 2 * exp (2 * x) + frac 1 2 * exp (3 * x)  )
= {- Collect common terms -}
     (1-3+2)*              * frac 5 2  *  exp x
  +  (-8-3*(-4)+2*(-2))    *              exp (2 * x)
  +  (9-3*3+2*1)           * frac 1 2  *  exp (3 * x)
= {- Arithmetics -}
  frac 2 2 * exp (3 * x)
=
  RHS
\end{spec}
Finally, we check the initial conditions: |f 0 = 1| and |f' 0 = 0|.
%
Here we use that |exp (alpha * 0) = exp 0 = 1| for all |alpha|:
\begin{spec}
  f 0   = frac 5 2 * exp 0 - 2 * exp (2 * 0) + frac 1 2 * exp (3 * 0)
        = frac 5 2 - 2 + frac 1 2
        = 1

  f' 0  = frac 5 2 * exp 0 - 4 * exp (2 * 0) + frac 3 2 * exp (3 * 0)
        = frac 5 2 - 4 + frac 3 2
        = 0
\end{spec}
Thus we can conclude that our |f| does indeed solve the differential equation.
%
The checking may seem overly pedantic, but when solving these
equations by hand it is often these last checks which help catching
mistakes along the way.

\subsection{Some standard Laplace transforms}
%
Now when we have tools to calculate solutions to ODEs we can use this
to compute some Laplace transforms for common functions specified by
ODEs, like |exp|, |sin|, and |cos|.
%

\paragraph{Deriving |ℒ exp|:}
The easiest one is |exp| which is the unique solution to the ODE |e' =
e|, |e 0 = 1|.
%
Here we can use the "Laplace-D" law:
%
|ℒ e' s = s * ℒ e s - e 0| in combination with the initial condition
and the application of |ℒ| to our characterising ODE
%
|ℒ e' s = ℒ e s| to eliminate |ℒ e' s|.
%
We get |ℒ e s = s * ℒ e s - 1| which is now an equation we can solve
for |ℒ e s| (abbreviated to |E s|).
%
First collect the terms, |(s-1)*E s = 1|, then divide by |s-1| to get 
%
|E s = 1/(s-1)|.
%
Thus |ℒ exp s = 1/(s-1)|.

It is instructive to see what happens when we make minor changes to
the ODE, for example |f' = alpha * f|, |f 0 = 1| for some constant
|alpha|.
%
The derivation is very similar but we get the equation
%
|alpha * ℒ f s = s * ℒ f s - 1| to solve, which gives us
%
|ℒ f s = 1/(s-alpha)|.
%
It is reassuring to see that we recover |exp| if we let |alpha = 1|.
%
But what function is |f|?
%
We know that |f x = A*exp (alpha*x)| has derivative |f' x =
alpha*A*exp (alpha*x) = alpha * f x|, and with |A=1| we satisfy the
initial condition |f 0 = 1|.
%
Thus |ℒ (\x->exp(alpha*x)) s = 1/(s-alpha)|.
%

From this we can also see that the ODE |f' = alpha * f|, |f 0 = A| has
the transform |A/(s-alpha)|.

\paragraph{Deriving |ℒ sin| and |ℒ cos|:}
%
%{
%format si = "\Varid{si}"
With the same method we can compute the transforms of |sin| and |cos|.
%
First the ODEs: |sin| and |cos| are the solutions to the the coupled
equations |si' = co|, |si 0 = 0|, |co' = -si|, |co 0 = 1|.
%
We apply Laplace to both ODEs (and linearity in the second):
%
\begin{spec}
 ℒ si'  s = ℒ co     s
 ℒ co'  s = ℒ (-si)  s = -ℒ si s
\end{spec}  
%
As for |exp| we can use the Laplace-D-law to simplify the derivatives:
%
\begin{spec}
  ℒ si'  s = s * ℒ si  s - si  0 = s * ℒ si  s
  ℒ co'  s = s * ℒ co  s - co  0 = s * ℒ co  s - 1
\end{spec}  
%
We combine our equations to eliminate |si'| and |co'|:
%
\begin{spec}
    ℒ co  s = s * ℒ si  s
 -  ℒ si  s = s * ℒ co  s - 1
\end{spec}  
and introduce names for the transformed functions: |S = ℒ si| and |C =
ℒ co|:
\begin{spec}
    C  s = s * S  s
 -  S  s = s * C  s - 1
\end{spec}  
%
Use the first to eliminate |C s| in the second to get |- S s = s^2 *
S s - 1| which means |1 = (s^2 + 1) * S s| and finally
\begin{spec}
  S s =            1 / (s^2 + 1)
  C s = s * S s =  s / (s^2 + 1)
\end{spec}  
%}

\section{Laplace and other transforms}
\label{sec:LaplaceSum}

To sum up, we have defined the Laplace transform and shown that it can
be used to solve differential equations.
%
It can be seen as a continuous version of the transform between the
infinite sequence of coeeficients |a : Nat -> REAL| and the functions
behind formal power series.

The Laplace transform is a close relative of the Fourier transform.
%
Both transforms are used to express functions as a sum of ``complex
frequencies'', but Laplace allows a wider range of functions to be
transformed.
%
A nice overview and comparison is B.\ Berndtsson's ``Fourier and
Laplace Transforms''\footnote{Available from
  \url{http://www.math.chalmers.se/Math/Grundutb/CTH/mve025/1516/Dokument/F-analys.pdf}.}.
%
Fourier analysis is a common tool in courses on Transforms, Signals
and Systems.

The Fourier transform can be seen as a generalisation of the Fourier
series presented in \cref{sec:inner-product-fourier}.
%
The Fourier series is a way of expressing functions on a \emph{closed}
interval (or, equivalently, periodic functions on the real line) as a
linear combination of dicrete frequency components rather than as a
function of time.
%
The Fourier transform (like the Laplace transform) also handles
non-period functions, and the result is a continuous linear
combination of frequency components (an integral rather than a sum).




%include E8.lhs
%}
