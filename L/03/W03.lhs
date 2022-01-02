\chapter{Types in Mathematics}
\label{sec:types}
%if False
\begin{code}
{-# LANGUAGE FlexibleInstances, PartialTypeSignatures #-}
module DSLsofMath.W03 where
import Prelude hiding (Num(..),Fractional(..), Floating(..))
import DSLsofMath.Algebra (Algebraic(..),Transcendental(..),
                           Additive(..),AddGroup(..),(-),
                           Multiplicative(..), MulGroup(..))
import DSLsofMath.W01 (Env, evalEnv)
import DSLsofMath.SimpleFunExp (FunExp(..))
type REAL = Double
type ℝ = REAL
type ℤ = Integer
powTo n = (^n)
powTo' = powTo . fromIntegral
\end{code}
%endif
%
% (Based on ../../2016/Lectures/Lecture05 )
% Show "Functional Differential Geometry" p16: Lagrange_example.pdf

In this chapter we continue the quest to assign types to mathematical
concepts.
% 
In \cref{sec:typeDerivative} through \cref{sec:incremental} we go
through several examples of short snippets from mathematical texts,
with different kinds of derivatives at the centre.
%
We also show (in \cref{sec:typeclasses}) how to collect related types
in Haskell's type classes and define several classes for the
``numerical hierarchy'': |Additive|, |AddGroup|, |Ring|, |Field|, etc.
%
These classes provide generalised versions of the standard arithmetic
operations like |(+)|, |(-)|, |(*)|, |(/)|, etc.\ in preparation for
the coming chapters.
%
Finally, in \cref{sec:computingDerivatives}, we use equational
reasoning to calculate a Haskell version of the classical derivative
rules (for sums, products, etc.).

\section{Typing Mathematics: derivative of a function}
\label{sec:typeDerivative}

Consider the classical definition of the derivative of
\citet{adams2010calculus}:
%
\begin{quote}
  The \textbf{derivative} of a function |f| is another function |f'|
  defined by
%
  \[
    f'(x) = \lim_{h \to 0} \frac{f(x+h) - f(x)}{h}
  \]
%
  at all points |x| for which the limit exists (i.e., is a finite real
  number).
  %
  If \(f'(x)\) exists, we say that |f| is \textbf{differentiable} at
  |x|.
\end{quote}
%
We can start by assigning types to the expressions in the definition.
%
Let's write |X| for the domain of |f| so that we have |f : X -> REAL|
and |X included REAL| (or, equivalently, |X : PowSet REAL|).
%
If we denote with |Y| the subset of |X| for which |f| is
differentiable we get |f' : Y -> REAL|.
%
Thus, the operation which maps |f| to |f'| has type |(X->REAL) ->
(Y->REAL)|.
%
Unfortunately, the only notation for this operation given (implicitly)
in the definition is a prime symbol (apostrophe), written postfix.
%
To make it easier to see we use a prefix |D| instead and we can
thus write |D : (X->REAL) -> (Y->REAL)|.
%
% {Why |Y| is typeset rather strangely? (calligraphic font?)  Leading idea: this is an effect of the mathpazo package. No format directive, see: |XY|}
We will often assume that |X = Y| (|f| is differentiable everywhere)
so that we can see |D| as preserving the type of its argument.

\index{derivative (|D|)||textbf}%
%
Now, with the type of |D| sorted out, we can turn to the actual
definition of the function |D f|.
%
The definition is given for a fixed (but arbitrary) |x|.
%
(At this point the reader may want to check the definition of
``limit of a function'' in \refSec{sec:LimitOfFunction}.)
%
\index{lim@@|lim| (limit)}%
%
The |lim| expression is using the
%
\index{anonymous function}%
(anonymous) function |g h = frac (f(x+h) - f x) h| and that the limit
of |g| is taken at |0|.
%
Note that |g| is defined in the scope of |x| and that its definition
uses |x| so it can be seen as having |x| as an implicit, first
argument.
%
To be more explicit we write |phi x h = frac (f(x+h) - f x) h| and take
the limit of |phi x| at 0.
%
So, to sum up, |D f x = lim 0 (phi x)|.%
%
\footnote{We could go one step further by noting that |f| is in the
scope of |phi| and used in its definition.
%
Thus the function |psi f x h = phi x h|, or |psi f = phi|, is used.
%
With this notation we obtain a point-free definition that can come in
handy:
%
|D f = lim 0 . psi f|.}
%
The key here is that we name, type, and specify the operation of
computing the derivative (of a one-argument function).
%
We will use this operation quite a bit in the rest of the book, but
here are just a few examples to get used to the notation.
%
With the following definitions:
\begin{spec}
  sq x      =  x^2
  double x  =  2*x
  c2 x      =  2
\end{spec}
we have the following equalities:
\begin{spec}
  sq'   ==  D sq   == D (\x -> x^2) == D ({-"{}"-}^2) == (2*) == double
  sq''  ==  D sq'  == D double == c2 == const 2
\end{spec}

What we cannot do at this stage is to actually \emph{implement} |D| in
Haskell.
%
If we only have a function |f : REAL -> REAL| as a ``black box'' we
cannot really compute the actual derivative |f' : REAL -> REAL|, but only
numerical approximations.
%
However if we also have access to the ``source code'' of |f|, then we can
apply the usual rules we have learnt in calculus.
%
We will get back to this question in \refSec{sec:computingDerivatives}.

\section{Typing Mathematics: \addtoindex{partial derivative}}
\label{sec:typePartialDerivative}
% https://books.google.com/ngrams/graph?year_end=2019&year_start=1800&corpus=26&content=quest+of%2C+quest+to&smoothing=3&direct_url=t1%3B%2Cquest%20of%3B%2Cc0%3B.t1%3B%2Cquest%20to%3B%2Cc0
Armed with our knowledge of functions of more than one variable, we
can continue on our quest to type the elements of mathematical
textbook definitions.
%
Our example here is by
\citet[page~169]{maclane1986mathematics}, where we read

\begin{linenumbers}
\begin{quote}
  [...] a function $z = f (x, y)$ for all points |(x, y)| in some open
  set |U| of the Cartesian |(x, y)|-plane.
%
  [...] If one holds |y| fixed, the quantity |z| remains just a
  function of |x|; its derivative, when it exists, is called the
  \emph{partial derivative} with respect to |x|.
%
  Thus at a point |(x, y)| in |U| this derivative for |h ≠ 0| is
\end{quote}
\begin{linenomath*}
\[
∂ z / ∂ x  =  f'_{x} (x, y) =
              \lim_{h \to 0} (f (x + h, y) - f (x, y)) / h
\]
\end{linenomath*}
\end{linenumbers}

What are the types of the elements involved?
%
We have

%{
%format f'x = "f''_{x}"
\begin{spec}
U    ⊆  cross ℝ ℝ          -- Cartesian plane

f    :  U -> ℝ

z    :  U -> ℝ             -- but see below

f'x  :  U -> ℝ
\end{spec}
%
The |x| in the subscript of |f'| is \emph{not} a real number, but a symbol
(we used |String| for similar purposes in \cref{sec:multiple-variables}).

The expression |(x, y)| has six occurrences.
%
The first two (on line 1) denote variables of type |U|, the third (on
line 2) is just a name (|(x, y)|-plane).
%
The fourth (at line 4) denotes a variable of type |U| bound by a
universal quantifier: ``a point |(x, y)| in |U|'' as text which would
translate to
%
|∀ (x, y) ∈ U| as a formula fragment.

The variable |h| is a non-zero real number.
%
The use of the word ``for'' might lead one to believe that it is bound
by a universal quantifier (``for |h ≠ 0|'' on line 4), but that is
incorrect.
%
In fact, |h| is used as a local variable introduced in the subscript
of $\lim$.
%
This variable |h| is a parameter of an \addtoindex{anonymous
  function}, whose limit is then taken at |0|.

That function, which we can name |phi|, has the type |phi : U ->
(ℝ\\{0}) -> ℝ| and is defined by
%
\begin{spec}
phi (x, y) h = (f (x + h, y) - f (x, y)) / h
\end{spec}
%
The limit is then written |lim 0 (phi (x, y))|.
%
Note that |0| is a limit point of |ℝ\\{0}|, so the type of |lim| is the
one we have discussed:
%
\begin{spec}
lim : {p | p ∈ ℝ, Limp p X } -> (X -> ℝ) -> ℝ
\end{spec}

On line 1, |z = f (x, y)| probably does not mean that we let |z| be a
fixed value in |ℝ|, although the phrase ``the quantity |z|'' (on line
2) suggests this.
%
Rather, a possible interpretation is that |z| is used to abbreviate
the expression |f(x, y)|.
%
That is, |z| stands for an expression which depends on |x| and |y|;
%
thus, it can be enlightening to replace |z| with |f(x, y)| everywhere.
%
In particular, |∂ z / ∂ x| becomes |∂ f (x, y) / ∂ x|, which we can
interpret as the operator |∂ / ∂ x| applied to |f| and |(x, y)|
(remember that |(x, y)| is bound in the context by a universal
quantifier on line 4).
%
There is the added difficulty that, just like the subscript in |f'x|,
the |x| in |∂ x| is not the |x| bound by the universal quantifier, but
just a symbol.
%}

To sum up, partial derivative operators which mention symbols (such as
|∂ / ∂ x| or ``prime subscript $x$'') do act on an representation of
functions which uses symbols for the variables (not positions), such
as presented in \cref{sec:ArithExp}.
%
This is why we mostly see \(∂f/∂x\), \(∂f/∂y\), \(∂f/∂z\) etc.\ when,
in the context, the function \(f\) has been given a definition of the
form \(f (x, y, z) = \ldots\).
%
This kind of approach presents several difficulties:
%
\begin{enumerate}
\item it makes it hard to rename variables (for example for the
  purpose of integration)
\item Further confusion can be created when a variable (such as $z$
  above) depends on other variables.
  %
  Tracing dependencies can become daunting and it is easy to make
  errors of name when doing calculations.
\item it makes it difficult to assign a higher-order type to the
  partial derivatives.
  %
  Indeed, as we have seen in \cref{sec:big-operators}, the \(∂f/∂x\)
  style means that the operator binds the name of the variable.
  %
  % But it is often awkward to make partial differentiation bind a variable. % But, we are precisely listing reason why it's awkward here. This sentence is circular reasoning.
\end{enumerate}

One possibility would be to use the following type:
%
\(∂/∂x_i : (ℝⁿ → ℝ) → (ℝⁿ → ℝ)\), but it still assumes as input a
vector of variables $x$--- even though the type assumes independence
with respect to the variable names.
%
Hence we prefer a notation which doesn't rely on the names given to
the arguments whatsoever.
%
It was popularised by \citet{landau1934einfuhrung} (English edition
\cite{landau2001differential}): \(D₁\) for the partial derivative with
respect to the first argument, \(D₂\) for the partial derivative with
respect to the second argument, etc.

\begin{exercise}
  \label{exc:D1usingD}\textbf{Partial Derivatives}
  \index{partial derivative}%
  For \(f : ℝ² → ℝ\) define \(D₁\) and \(D₂\) using only \(D\).
%
  In more detail: let the type \(F2 = ℝ² → ℝ\) and \(F1 = ℝ → ℝ\).
%
  Then both \(D₁\) and \(D₂\) have type |F2 -> F2| and |D : F1 -> F1|.
%
  Start by defining helper functions:
\begin{spec}
  fstFixed  :: a  -> (b  -> (a, b))
  sndFixed  :: b  -> (a  -> (a, b))
\end{spec}
  Hint: there is only one type-correct definition of each.
% 
  Then use |D| and the helpers in the definitions of \(D₁\) and \(D₂\).
\end{exercise}

%TODO: perhaps mention "total derivative" at this stage. That could serve as an intermediate step towards the Langrangian, or could be added after it.

\section{Typing Mathematics: Lagrangian case study}
\label{sec:Lagrangian}

From \citet{sussman2013functional}:
%\index{Lagrangian}%
\index{Lagrange equations}%
\begin{quote}
  A mechanical system is described by a Lagrangian function of the
  system state (time, coordinates, and velocities).
%
  A motion of the system is described by a path that gives the
  coordinates for each moment of time.
%
  A path is allowed if and only if it satisfies the Lagrange
  equations.
%
  Traditionally, the Lagrange equations are written
%
\[
  \frac{d}{dt} \frac{∂L}{∂\dot{q}} - \frac{∂L}{∂q} = 0
\]
%
What could this expression possibly mean?
\end{quote}

To start answering the question of Sussman and Wisdom, we start typing
the elements involved:

%{
%format dotq = "\dot{q}"
%format / = "\mathbin{\!/\!}"
%format ddotq =  ∂ dotq
%format juxtapose f x = f "\," x
\begin{enumerate}
\item First, note that the ``system state'' mentioned can be modelled
  as a triple (of type |S = T × Q × V|) and we can call the three
  components |t : T| for time, |q : Q| for coordinates, and |v : V|
  for velocities.

\item If we let ``coordinates'' be just one coordinate, then there is
  also a single velocity.
  %
  (A bit of physics domain knowledge is useful here: if \(q\) is a
  position of a particle, then |v| is its velocity.)
  % 
  Thus we can use |T = Q = V = ℝ| in this example but it can help the
  reading to remember the different uses of |ℝ| --- this would help
  for example to generalise to more than one coordinate.

\item Also the use of notation for ``partial derivative'', \(∂L / ∂q\),
  suggests that |L| is a function of at least a pair of arguments:
\begin{spec}
  L : ℝⁱ → ℝ,    i ≥ 2
\end{spec}
%
This is consistent with our plan so far if we take |i = 3|:
%
\begin{spec}
  L : ℝ³ → ℝ
\end{spec}
%
\item Looking again at the same derivative, \(∂L / ∂q\) suggests that
  \(q\) is the name of a real variable, one of the three arguments to
  \(L\).
%
  In the context, which we do not have, we would expect to find
  somewhere the definition of the Lagrangian as a function of the
  system state:
  \begin{spec}
    L  :  T × Q × V  ->  ℝ
    L (t, q, v)  =  ...
  \end{spec}

\item Consequently the type of the partial derivatives get specialised
  as follows:

\begin{spec}
  (∂/∂q) : (T × Q × V  ->  ℝ) -> (T × Q × V  ->  ℝ)
\end{spec}
%  (∂/∂v) : (T × Q × V  ->  ℝ) -> (T × Q × V  ->  ℝ)

The notation |∂L / ∂q| is equivalent to |(∂/∂q) L|, and |D₂ L|;
applying the partial derivative with respect to the second
argument (named |q|) of~|L|.

\item Therefore, |∂L / ∂q| should also be a function of the same
  triple of arguments as |L|:
%
  \begin{spec}
    (∂L / ∂q) : T × Q × V -> ℝ
  \end{spec}
%
  It follows that the equation expresses a relation between
  \emph{functions}, thus the |0| on the right-hand side of the
  Lagrange equation(s) is \emph{not} the real number |0|, but rather
  the constant function |const 0|:
%
  \begin{spec}
    const 0  :  T × Q × V  →  ℝ
    const 0     (t, q, v)  =   0
  \end{spec}
%
\item We now have a problem: |d / dt| can only be applied to functions
  of \emph{one} real argument |t|, and the result is a function of one
  real argument:
%
  \begin{spec}
    juxtapose (d / dt) (∂L / ∂dotq)  :  T → ℝ
  \end{spec}
%
  Since we subtract from this the function \(∂L / ∂q\), it follows
  that this, too, must be of type |T -> ℝ|.
%
  But we already typed it as |T × Q × V → ℝ|, contradiction!
%
  \label{item:L:contra}

\item The expression |∂L / ∂dotq| appears to also be malformed.
%
  We would expect a variable name where we find |dotq|, but
  |dotq| is the same as |dq / dt|, a function.
%
  But, with some knowledge from physics we can guess that |dotq|, the
  rate of change of the position with time, is the same as the |v|,
  the velocity.
  %
  Thus |∂L / ∂dotq = ∂L / ∂v = D₃ L| --- now well-formed, but still
  ill-typed.

\item Looking back at the description above, we see that the only
  immediate candidate for an application of \(d/dt\) is ``a path that
  gives the coordinates for each moment of time''.
%
  Thus, the path is a function of time, let us say
%
  \begin{spec}
    w  :  T → Q  -- with |T| for time and |Q| for coordinates (|q : Q|)
  \end{spec}
%
  We can now guess that the use of the plural form ``equations'' might
  have something to do with the use of ``coordinates'' in the plural.
%
  In an |n|-dimensional space, a position is given by |n|
  coordinates.
%
  A path would then be a function
%
  \begin{spec}
    w  :  T → Q  -- with |Q = ℝⁿ|
  \end{spec}
%
  which is equivalent to |n| functions of type |T → ℝ|, each computing
  one coordinate as a function of time.
%
  We would then have an equation for each of them.
%
  But we will come back to use |n=1| for the rest of this example.

\item Now that we have a path, the coordinates at any time are given
  by the path.
  %
  And because the time derivative of a coordinate is a velocity, we
  can actually compute the trajectory of the full system state |(T, Q,
  V)| starting from just the path.
%
  \begin{spec}
    q  :  T → Q
    q t  =  w t        -- or, equivalently, |q = w|

    dotq : T → V
    dotq t = dw /dt    -- or, equivalently, |dotq = D w|
  \end{spec}
%
  We combine these in the ``combinator'' |expand|, given by
  %
  \begin{spec}
    expand : (T → Q) → (T → T × Q × V)
    expand w t  =  (t, w t, D w t)
  \end{spec}

\item With |expand| in our toolbox we can fix the typing problem in
  item \ref{item:L:contra} above.
  %
  The Lagrangian is a ``function of the system state (time,
  coordinates, and velocities)'' and the ``expanded path'' (|expand
  w|) computes the state from just the time.
  %
  By composing them we get a function
  %
  \begin{spec}
    L . (expand w)  :  T -> ℝ
  \end{spec}
  %
  which describes how the Lagrangian would vary over time if the
  system would evolve according to the path |w|.

  This particular composition is not used in the equation, but we do
  have
  %
  \begin{spec}
    (∂L / ∂q) . (expand w)  :  T -> ℝ
  \end{spec}

  which is used inside |d / dt| (and which now type-checks).

\item We now move to using |D| for |d / dt|, |D₂| for |∂ / ∂q|, and
  |D₃| for |∂ / ∂dotq|.
  %
  The type of the partial derivatives |D₂| and |D₃| is |(S -> REAL)
  -> (S -> REAL)|, and here |D : (T -> ℝ) -> (T -> ℝ)|.
  %
  In combination with |expand w| we find these type correct
  combinations for the two terms in the equation:
  %
  \begin{spec}
    D (  (D₂ L)  ∘  (expand w))  :  T → ℝ
         (D₃ L)  ∘  (expand w )  :  T → ℝ
  \end{spec}

  The equation becomes
  %
  \begin{spec}
    D ((D₃ L) ∘ (expand w))  -  (D₂ L) ∘ (expand w)  =  const 0
  \end{spec}
  or, after simplification:
  \begin{spec}
    D (D₃ L ∘ expand w)  =  D₂ L ∘ expand w
  \end{spec}
  %
  where both sides are functions of type |T → ℝ|.

\item ``A path is allowed if and only if it satisfies the Lagrange
  equations'' means that this equation is a predicate on paths (for a
  particular |L|):
  %
  \begin{spec}
    Lagrange(L, w) =  D (D₃ L ∘ expand w) == D₂ L ∘ expand w
  \end{spec}
  %
  where we use |(==)| in the equation to avoid confusion with the
  equality sign (|=|) used for the definition of the predicate.
  \index{Lagrange equations}%
\end{enumerate}

So, we have figured out what the equation means in terms of
operators that we recognise.
%
If we zoom out slightly we see that the quoted text means something
like:
%
If we can describe the mechanical system in terms of a ``Lagrangian''
(|L : S -> ℝ| where |S = T × Q × V|), then we can use the equation to
check if a particular candidate path |w : T → ℝ| qualifies as an
allowed ``motion of the system'' or not.
%
The unknown of the equation is the path |w|, and as the equation
involves partial derivatives it is an example of a \addtoindex{partial
  differential equation} (a PDE).
%
We will not dig into how to solve such PDEs, but they are widely used
in physics.
%} %end of formatting for the Lagrangian example

In \cref{exc:Lagrange} you can practice checking under which
conditions the Lagrange equations are satisfied for some candidates
paths in a simple case of an object moving in constant gravity.
%
If you have the right background in physics it is really instructive
to try out a few more advanced examples of Lagrangians, or check the
similar method of Hamiltonian dynamics described in
\cref{exc:Hamiltonian} (which includes links to blog posts and Haskell
libraries helping with simulations of the modelled systems).

\section{Incremental analysis with types}
\label{sec:incremental}
So far in this chapter we have worked on typing mathematics, but
without the help of any tool.
%
However we can in fact get the Haskell interpreter to help a bit even
when we are still at the specification stage --- before we have any
code running.
%
It is often useful to collect the known (or assumed) facts about types
in a Haskell file and regularly check if the type-checker agrees.
%
This is a form of \addtoindex{type-driven development} and can help
avoiding wrong turns even for concepts which cannot be fully
implemented.
%
\newpage
Consider the following quote from \citet[page
182]{maclane1986mathematics}:
%
\begin{quote}
  \begin{linenumbers}[1]
     In these cases one tries to find not the values of
     |x|\linelabel{line:exam1603_x} which make a given function |y =
     f(x)| a minimum, but the values of a given function |f(x)| which
     make a given quantity a minimum.  Typically, that quantity is
     usually measured by an integral whose integrand is some
     expression |F|\linelabel{line:exam1603_F} involving both |x|,
     values of the function |y = f(x)| at interest and the values of
     its derivatives --- say an integral
  \begin{linenomath*}
    \[\int_a^b F(y, y', x)dx,\quad y = f(x).\]\linelabel{line:exam1603_int}%
  \end{linenomath*}%
  \end{linenumbers}%
\end{quote}
%
\lnOnly{Typing the variables and the integration operators in this text was an
  exam question in 2016.}
%
We will use the quote as an example of getting feedback from a type
checker.
%
We start by declaring two types, |X| and |Y|, and a function |f|
between them:
%
\begin{code}
data X   -- |X| must include the interval |[a,b]| of the reals
data Y   -- another subset of the reals

f :: X -> Y
f = undefined
\end{code}
%
\index{data@@|data| (keyword)}%
%
To the Haskell interpreter, such empty |data|-declarations mean that
there is no way to construct any element for them, as we saw in
\cref{sec:curry-howard}.
%
But at this stage of the specification, we will use this notation to
indicate that we do not know anything about values of those types.
%
Similarly, |f| has a type, but no proper implementation.
%
We will declare types for the rest of the variables as well, and
because we are not implementing any of them right now, we can just
make provide a dummy definition for a few of them in one go:
%
\begin{code}
(x, deriv, ff, a, b, int) = undefined
\end{code}
%
We write |ff| for the capital |F| (to satisfy Haskell rules for
variable names), |deriv| for the derivation operator (|D| above), and
|int| for the integral operator.
%
On line~\ref{line:exam1603_x} ``values of |x|'' hints at the type |X|
for |x| and the way |y| is used indicates that it is to be seen as an
alias for |f| (and thus must have the same type).

As we have discussed above, the derivative normally preserves the type
and thus we can write:
%
\begin{code}
x   :: X
y   :: X -> Y;    y   =  f
y'  :: X -> Y;    y'  =  deriv f
deriv :: (X -> Y) -> (X -> Y)
\end{code}
%
Next up (on line~\ref{line:exam1603_F}) is the ``expression |F|''
(which we write |ff|).
%
It should take three arguments: |y|, |y'|, |x|, and return ``a
quantity''.
%
We can invent a new type |Z| and write:
%
\begin{code}
data Z   -- Probably also some subset of the real numbers
ff :: (X -> Y) -> (X -> Y) -> X -> Z
\end{code}
%
Then we have the operation of definite integration, which we know
should take two limits |a, b :: X| and a function |X -> Z|.
%
The traditional mathematics notation for integration uses an
expression (in |x|) followed by |dx|, but we can treat that as a
function |expr| binding |x|:
%
\begin{code}
a, b :: X
integral = int a b expr
  where  expr x = ff y y' x

int :: X -> X -> (X -> Z) -> Z
\end{code}
%
Now we have reached a stage where all the operations have types and
pthe type-checker is happy with them.
%
At this point it is possible to experiment with variations based on
alternative interpretations of the text.
%
For this kind of ``refactoring'' is very helpful to have the type
checker to make sure the types still make sense.
%
For example, we could write |ff2 :: Y -> Y -> X -> Z| as a variant of
|ff| as long as we also change the expression in the integral:
%
\begin{code}
ff2 :: Y -> Y -> X -> Z
ff2 = undefined
integral2 = int a b expr
  where  expr x = ff2 y y' x
           where  y   = f x
                  y'  = deriv f x
\end{code}
%
Both versions (and a few more minor variations) would be fine as exam
solutions, but something where the types don't match up would not be OK.

The kind of type inference we presented so far in this chapter becomes
automatic with experience in a domain, but is very useful in the
beginning.

\section{Type classes}
\label{sec:typeclasses}

One difficulty when reading (and implementing) mathematics is
\emph{\addtoindex{overloading}}.
%
For our purposes, we say that a symbol is \emph{overloaded} when its
meaning depends on the type of the expressions that it applies to.

Consider, for example, the operator |(+)|.
%
According to usual mathematical notation, one can use it to add
integers, rational numbers, real numbers, complex numbers, etc.\ and it
poses no difficulty.
%
We explore the mathematical reasons in more detail in
\cref{sec:AlgHomo}, but for now we will concentrate on the view of
functional programming of this problem: one way to understand
overloading is via \emph{\addtoindex{type class}es}.

In Haskell both |4 == 3| and |3.4 == 3.2| typecheck because both
integers and floating point values are member of the |Eq| class, which
we can safely assume to be defined as follows:
%
\index{class@@|class| (keyword)}%
\index{Eq@@|Eq| (type class)||textbf}%
\begin{spec}
class Eq a where   (==) :: a -> a -> Bool
\end{spec}
%
The above declaration does two things.
%
First, it names a set of types which have equality test.
%
One can tell the Haskell compiler that certain types belong to this
set by using instance declarations, which additionally provide an
implementation for the equality test.
%
For example, we can make |Bool| member of the |Eq| using the following
declaration:
\begin{spec}
eqBool :: Bool -> Bool -> Bool
eqBool  True   True   = True
eqBool  False  False  = True
eqBool  _      _      = False
instance Eq Bool where  (==) = eqBool
\end{spec}
\index{instance@@|instance| (keyword)||textbf}%
%
(The Haskell compiler will in fact provide instances for primitive
types ).

Second, the |Eq| class declaration provides an operator |(==)| of type
|Eq a => a -> a -> Bool|.
%
One can use the operator on any type |a| which belongs to the |Eq|
set.
%
\index{constraint (type)}%
%
This is expressed in general by a constraint |Eq a| occurring before
the |=>| symbol.

Instance declarations can also be parameterised on another
instance.
%
Consider for example:
\begin{spec}
instance Eq a => Eq [a] where  (==) = ... -- exercise
\end{spec}
%
In the above, the expression |Eq a => Eq [a]| means that for any type
|a| which is already an instance of |Eq| we also make the type |[a]|
an instance of Eq.
%
Thus, for example, by recursion we now have an infinite collection of
instances of |Eq|: |Char|, |[Char]|, |[[Char]]|, etc.
%

\subsection{Numeric operations}
\index{Num@@|Num| (type class)}%
%
Haskell also provides a |Num| class, containing various numeric types
(|Int|, |Double|, etc.) with several operators (|+|,|*|, etc.).
%
Unfortunately, the |Num| class was designed with more regard for
implementation quirks than mathematical structure, and thus it is a
poor choice for us.
%
We take a more principled approach instead, and define the following
classes, which together serve a similar role as |Num|, and which we
study in more detail in \cref{sec:ring-like-classes}:

\label{sec:numeric-classes}
%*TODO: Make sure the code matches ../DSLsofMath/Algebra.hs.
\index{Additive@@|Additive| (type class)}%
\index{AddGroup@@|AddGroup| (type class)}%
\begin{spec}
class Additive a where
  zero  :: a
  (+)   :: a -> a -> a

class Additive a => AddGroup a where
  negate :: a -> a  -- specified as |x + negate x == zero|

class Multiplicative a where
  one  :: a
  (*)  :: a -> a -> a

class Multiplicative a => MulGroup a where
  recip :: a -> a   -- reciprocal, specified as  |x * recip x == one|
\end{spec}
\index{Multiplicative@@|Multiplicative| (type class)}%
\index{MulGroup@@|MulGroup| (type class)}%
%
The operator names clash with the |Num| class, which we will avoid
from now on in favour |Additive| and |Multiplicative|.
%
In \cref{sec:ring-like-classes} we will get back to these classes and
present a comparison in \cref{fig:CompNum}.

\begin{exercise}
  Consider the exponentiation operator, which we can write |(^)|.
  %
  Taking advantage of the above classes, propose a possible type for
  it and sketch an implementation.
\end{exercise}
\begin{solution}
  One possibility is |(^) :: MulGroup a => a -> Int -> a|.
  %
  For positive exponents, one can use repeated multiplication.
  %
  For negative exponents, one can use repeated division.
\end{solution}
% %
% It is similar to the type-casts one does automatically in expressions such
% as \(4 + 2.5\).
% TODO: here we also use defaulting at the same time, which is 1. not principled and 2. does not work with custom classes.

%TODO: Perhaps end subsect with some connecting back to the start
%Given these definitions, both expressions $4 + 3$ and $3.4 + 3.2$ typecheck.


\subsection{Overloaded integer literals}
\label{sec:overloaded-integer-literals}
\index{overloading}%
We will spend some time explaining a convenient Haskell-specific
syntactic shorthand which is very useful but which can be confusing:
overloaded integers.
%
In Haskell, every use of an integer literal like |2|, |1738|, etc., is
actually implicitly an application of |fromInteger| to the literal
typed as an |Integer|.
%
\index{fromInteger@@|fromInteger|}%

But what is |fromInteger|?
%
It is a function that converts integers to any type that supports
|zero|, |one|, |(+)|, and |(-)|.
%
We can implement it by the following three cases depending on the sign
of |n|:
\begin{code}
fromInteger :: (AddGroup a, Multiplicative a) => Integer -> a
fromInteger n  | n < 0      = negate (fromInteger (negate n))
               | n == 0     = zero
               | otherwise  = one + fromInteger (n - 1)
\end{code}
\begin{exercise}
  Define |fromRational| which does the same but also handles rational
  numbers and has the |MulGroup a| constraint.
\end{exercise}

%
This means that the same program text can have various meanings
depending on the type of the context (but see also
\cref{ex:fromInteger}):
%
The literal |three = 3|, for example, can be used as an integer, a
real number, a complex number, or anything which belongs both to
|AddGroup| and |Multiplicative|.

%if False
% The below refers to commented-out stuff; commented as well.
The same pattern appeared already in \refSec{sec:firstFromInteger},
which near the end included roughly the following lines:
%
\begin{spec}
instance Num r => Num (ComplexSyn r) where
  -- ... several other methods and then
  fromInteger = toComplexSyn . fromInteger
\end{spec}
%
To see why this is not a recursive definition we need to expand the
type and to do this I will introduce a name for the right hand side
(RHS): |fromIntC|.

\begin{verbatim}
--          ComplexSyn r <---------- r <---------- Integer
fromIntC =              toComplexSyn . fromInteger
\end{verbatim}

I have placed the types in the comment, but with left-pointing function types
arrows, indicating that
%
|fromInteger :: Integer -> r| and
%
|toComplexSyn :: r -> ComplexSyn r|
%
while the resulting function is
%
|fromIntC :: Integer -> ComplexSyn r|.
%
The use of |fromInteger| at type |r| means that the full type of
|fromIntC| must refer to the |Num| class.
%
Thus we arrive at the full type:
%
\begin{spec}
fromIntC :: Num r =>   Integer -> ComplexSyn r
\end{spec}

As an example we have that
\begin{spec}
  3 :: ComplexSyn Double                 ==  {- |Integer| literals have an implicit |fromInteger| -}
  (fromInteger 3) :: ComplexSyn Double   ==  {- |Num| instance for |ComplexSyn| -}
  toComplexSyn   (fromInteger 3)         ==  {- |Num| instance for |Double| -}
  toComplexSyn   3.0                     ==  {- Def. of |toComplexSyn| from \refSec{sec:toComplexSyn} -}
  FromCartesian  3.0  0                  ==  {- |Integer| literals have an implicit |fromInteger| -}
  FromCartesian  3.0  (fromInteger 0)    ==  {- |Num| instance for |Double|, again -}
  FromCartesian  3.0  0.0
\end{spec}
%endif

\subsection{Structuring DSLs around type classes}
Type classes are related to mathematical structures which, in turn,
are related to DSLs.
%
As an example, consider again the DSL of expressions of one variables.
%
We saw that such expressions can be represented by the type |REAL ->
REAL| in the shallow embedding.
%
Using type classes, we can use the usual operators names instead of
|funAdd|, |funMul|, etc.
%
We could write:
\begin{spec}
instance Additive (REAL -> REAL) where
  (+)        =  funAdd
  zero       =  funConst zero
\end{spec}
%
The instance declaration of the method |zero| above looks recursive,
but is not: |zero| is used at a different type on the left- and
right-hand-side of the equal sign, and thus refers to two different
definitions.
%
One the left-hand-side we define |zero :: REAL -> REAL|, while on the
right-hand-side we use |zero :: REAL|.

%
\begin{figure}[htpb]
\begin{spec}
instance Additive a        => Additive        (x -> a) where
   (+)        =  funAdd
   zero       =  funConst zero

instance Multiplicative a  => Multiplicative  (x -> a) where
   (*)        =  funMul
   one        =  funConst one

instance AddGroup a        => AddGroup        (x -> a) where
   negate f   =  negate . f

instance MulGroup a        => MulGroup        (x -> a) where
   recip f    =  recip . f

instance Algebraic a       => Algebraic       (x -> a) where
   sqrt f     =  sqrt . f

instance Transcendental a  => Transcendental  (x -> a) where
   pi =  const pi
   sin f = sin . f; {-"\quad"-}    cos f = cos . f; {-"\quad"-}  exp f = exp . f
\end{spec}
\caption{Numeric instances lifted to functions. Full definitions can
  be found in |module Algebra| in the repo. (Sometimes referred to as
  |FunNumInst|.)}
  \label{fig:FunNumInst}
\end{figure}
\label{sec:FunNumInst}
%
However, as one may suspect, for functions, we can use any domain and
any numeric co-domain in place of |REAL|.
%
Therefore we prefer to define the more general instances in
\cref{fig:FunNumInst}.
%
Here we extend our set of type-classes to cover algebraic and
transcendental numbers.
%
A simplified version, which is sufficient for our purposes, looks as
follows:
%
\index{Algebraic@@|Algebraic| (type class)}%
\index{sqrt@@square root (|sqrt|)||textbf}%
%
\begin{spec}
class Field a => Algebraic a where
  sqrt  :: a -> a

class Field a => Transcendental a where
  pi :: a
  exp   :: a -> a
  sin   :: a -> a
  cos   :: a -> a
\end{spec}
%
\index{Transcendental@@|Transcendental| (type class)}%
%\index{sin@@|sin|{}||textbf}%
%\index{cos@@|cos|{}||textbf}%
\index{exp@@|exp|{}||textbf}%
\index{pi@@|pi|{}||textbf}%
%
While classes up to |Field| follow mathematical conventions very
closely, for |Algebraic| and |Transcendental| we take the pragmatic
approach and list only the methods which are necessary for our
development.

Together, these type classes represent an abstract language of
abstract and standard operations, abstract in the sense that the exact
nature of the elements involved is not important from the point of
view of the type class, only from that of its implementation.
%
What does matter for the class (but is not captured in the Haskell
definition of the class), is the relationship between various
operations (for example addition should distribute over
multiplication).

These instances for functions allow us to write expressions which are
very commonly used in maths books, such as |f+g| for the sum of two
functions |f| and |g|, say |sin + cos :: REAL -> REAL|.
%
Somewhat less common notations, like |sq * double :: ZZ -> ZZ| are
also possible.
%
They have a consistent meaning: the same argument is passed to all
functions in an expression.
%
As another example, we can write |sin^2|, which the above instance
assigns the following meaning:
\begin{spec}
  sin^2 = \x -> (sin x)^(const 2 x) = \x -> (sin x)^2
\end{spec}
%
thus the typical maths notation \(\sin^2\) can work fine in Haskell,
provided the above instances for functions, assuming a fixed argument.
%
(Note that there is a clash with another common use of superscript for
functions in mathematical texts: sometimes |f^n| means
\emph{composition} of |f| with itself |n| times.
%
With that reading \(sin^2\) would mean |\x->sin (sin x)|.)

%
\begin{exercise}
  Experiment with this feature using GHCi, for example by evaluating
  |sin + cos| at various points.
\end{exercise}

Something which may not be immediately obvious, but is nonetheless
useful, is that all the above instances are of the form |C a => C (x
-> a)| and are therefore parametric.
%
This means that, for example, given the instance |Additive a =>
Additive (x -> a)| and the instance |Additive REAL|, we have that the
types |a->REAL|, |a->(b->REAL)|, etc.\ are all instances of |Additive|.
%
Consequently, we can use the usual mathematical operators for
functions taking any number of arguments --- provided that they match
in number and types.

\section{Computing derivatives}
\label{sec:computingDerivatives}
% What is the purpose of the section?

%   Elements of answer:
%   - we have laws at the semantic level (coming from the definition of limit), and we want to derive laws at a syntactic level
%   - point out that such laws are effectively used as computational rules (even though they are not really)

%   It is a stepping stone towards showing an example of non-trivial
%   compositionality for the next chapter.

An important part of calculus is the collection of laws, or rules, for
computing derivatives.
%
They are provided by \citet{adams2010calculus} as a series of
theorems, starting at page 108 of their book.
%
We we can summarise those as follows:
\begin{align*}
  (f + g)'(x)  &=   f'(x) + g'(x)            \\
  (f * g)'(x)  &=   f'(x)*g(x) + f(x)*g'(x)  \\
  (C * f)'(x)  &=   C*f'(x)                  \\
  (f ∘ g)'(x)  &=   f' (g (x)) * g'(x)       |-- chain rule|
\end{align*}
(After a while, \citeauthor{adams2010calculus} switch to
differential notation, so we omit corresponding rules for
trigonometric and exponential functions.)
%
Using the notation |D f| for the derivative of |f| and applying the
numeric operations to functions directly, we can fill in a table of
examples which can be followed to compute derivatives of many
functions:
%
\index{derivative (|D|)||textbf}%
%
\begin{spec}
    D (f + g)        =  D f + D g
    D (f * g)        =  D f * g + f * D g
    D id             =  const 1
    D (const a)      =  const 0

    D (f . g)        =  (D f . g) * D g     -- chain rule
    D sin            =  cos
    D cos            =  - sin
    D exp            =  exp
\end{spec}
% D (powTo n)      =  (n*) . (powTo (n-1))  % commented out because it has difficulties (n = 0) and it's taken care of by the product rule and we don't refer to it anyway.
%
and so on.
%

If we want to get a bit closer to actually implementing |D| we quickly
notice a problem:
%
if |D| has type |(REAL -> REAL) -> (REAL -> REAL)|, we have no way to
turn the above specification into a program, because the program has
no way of telling which of these rules should be applied.
%
That is, given an extensional (semantic, shallow) function |f|, the
only thing that we can ever do is to evaluate |f| at given points, and
thus we cannot know if this function was written using a |(+)|, or |sin|
or |exp| as outermost operation.
%
The only thing that a derivative operator could do would be to
numerically approximate the derivative, and that is not what we are
exploring in this \course{}.
%
Thus we need to take a step back and change the type that we work on.
%
Even though the rules in the table are obtained by reasoning
semantically, using the definition of limit for functions (of type |ℝ
→ ℝ|), they are really intended to be used on \emph{syntactic}
functions or expressions: abstract syntax trees \emph{representing}
the (semantic) functions.

We observe that we can compute derivatives for any expression made out
of arithmetic functions, trigonometric functions, the exponential and
their compositions.
%
In other words, the computation of derivatives is based on a domain
specific language of expressions (representing functions in
one variable).
%
\index{FunExp@@|FunExp| (type)}%
%
This means that we can in fact implement the derivative of |FunExp|
expressions (from \cref{sec:FunExp}), using the rules of derivatives.
%
Because the specification of derivation rules is already in the right
format, the way to obtain this implementation may seem obvious, but we
will go through the steps as a way to show the process in a simple
case.

\label{sec:derive}
Our goal is to implement a function |derive :: FunExp -> FunExp| which
makes the following diagram commute:
%
\index{derive@@|derive|{}}%

\quad%
\begin{tikzcd}
  |FunExp| \arrow[r, "|eval|"] \arrow[d, "|derive|"]  & |Func| \arrow[d, "D"] \\
  |FunExp| \arrow[r, "|eval|"]                        & |Func|
\end{tikzcd}

That is, we want the following equality to hold:
%
\begin{spec}
     eval . derive  =  D . eval
\end{spec}
%
In turn, this means that for any expression |e :: FunExp|, we want
%
\begin{spec}
     eval (derive e)  =  D (eval e)
\end{spec}
%

As an example of using equational reasoning we will calculate the
definition of |derive| for a specific constructor.
%
We have added |Exp :: FunExp -> FunExp| to the datatype |FunExp| for
this example.
%
We start from the left-hand side of the specification, in the case
when the expression is of the form |Exp e|, and use equalities from
the mathematical side in combination with our instances and functions
definitions to ``push'' the call of |derive| to the subexpression |e|:
%
\index{equational reasoning}%
%
\begin{spec}
     eval (derive (Exp e))                          =  {- specification of |derive| above -}

     D (eval (Exp e))                               =  {- def. |eval| -}

     D (exp (eval e))                               =  {- def. |exp| for functions -}

     D (exp . eval e)                               =  {- chain rule -}

     (D exp . eval e) * D (eval e)                  =  {- |D| rule for |exp| -}

     (exp . eval e) * D (eval e)                    =  {- specification of |derive| -}

     (exp . eval e) * (eval (derive e))             =  {- def. of |eval| for |Exp| -}

     (eval (Exp e)) * (eval (derive e))             =  {- def. of |eval| for |:*:| -}

     eval (Exp e  :*:  derive e)
\end{spec}
%
Therefore, the specification is fulfilled by taking
%
\begin{spec}
derive (Exp e) = Exp e :*: derive e
\end{spec}

Similarly, we obtain
%
\index{derive@@|derive|{}}%
%
\begin{joincode}%
\begin{code}
derive :: FunExp -> FunExp
derive     (Const alpha)  =  Const 0
derive     X              =  Const 1
derive     (e1 :+: e2)    =  derive e1  :+:  derive e2
derive     (e1 :*: e2)    =  (derive e1  :*:  e2)  :+:  (e1  :*:  derive e2)
\end{code}
\begin{spec}
derive     (Exp e)        =  Exp e :*: derive e
\end{spec}
\end{joincode}
%
\begin{exercise}
  Complete the |FunExp| type and the |eval| and |derive| functions.
\end{exercise}

%include E3.lhs
