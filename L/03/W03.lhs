
\section{Types in Mathematics}
\label{sec:types}
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module DSLsofMath.W03 where
import Data.Char (toUpper)
\end{code}

% (Based on ../../2016/Lectures/Lecture05 )
% Show "Functional Differential Geometry" p16: Lagrange_example.pdf

\subsection{Types in mathematics}\label{types-in-mathematics}

Types are sometimes mentioned explicitly in mathematical texts:

\begin{itemize}
\item \(x ∈ ℝ\)
\item \(\sqrt{\phantom{x}} : ℝ_{≥0} → ℝ_{≥0}\)
\item \((\_)² : ℝ → ℝ\) or, alternatively but \emph{not} equivalently
\item \((\_)² : ℝ → ℝ_{≥0}\)
\end{itemize}

The types of ``higher-order'' operators are usually not given
explicitly:

\begin{itemize}
\item |lim : (ℕ → ℝ) → ℝ| for \(lim_{n → ∞} \{a_n\}\)
\item \(d/dt : (ℝ → ℝ) → (ℝ → ℝ)\)
\item sometimes, instead of \(df/dt\) one sees \(f'\) or \(\dot{f}\) or |D f|
\item \(∂f/∂x_i : (ℝⁿ → ℝ) → (ℝⁿ → ℝ)\)
\item we mostly see \(∂f/∂x\), \(∂f/∂y\), \(∂f/∂z\) etc. when, in the
  context, the function \(f\) has been given a definition of the form
  \(f (x, y, z) = \ldots\)
\item a better notation (by Landau) which doesn't rely on the names
  given to the arguments was popularised in
  \cite{landau1934einfuhrung} (English edition
  \cite{landau2001differential}): \(D₁\) for the partial derivative with
  respect to \(x₁\), etc.
\item
  Exercise: for \(f : ℝ² → ℝ\) define \(D₁\) and \(D₂\) using only \(D\).
\end{itemize}

\subsection{Typing Mathematics: derivative of a function}
\label{sec:typeDerivative}

Let's start simple with the classical definition of the derivative
from \citet{adams2010calculus}:
%
\begin{quote}
  The \textbf{derivative} of a function |f| is another function |f'| defined by
%
  \[
    f'(x) = \lim_{h \to 0} \frac{f(x+h) - f(x)}{h}
  \]
%
  at all points |x| for which the limit exists (i.e., is a finite real
  number). If \(f'(x)\) exists, we say that |f| is \textbf{differentiable}
  at |x|.
\end{quote}
%
We can start by assigning types to the expressions in the definition.
%
Let's write |X| for the domain of |f| so that we have |f : X -> REAL|
and |X included REAL| (or, equivalently, |X : PS REAL|).
%
If we denote with |Y| the subset of |X| for which |f| is
differentiable we get |f' : Y -> REAL|.
%
Thus, the operation which maps |f| to |f'| has type |(X->REAL) ->
(Y->REAL)|.
%
Unfortunately, the only notation for this operation given (implicitly)
in the definition is a postfix prime.
%
To make it easier to see we we use a prefix |D| instead and we can
thus write |D : (X->REAL) -> (Y->REAL)|.
%
We will often assume that |X = Y| so that we can can see |D| as
preserving the type of its argument.

Now, with the type of |D| sorted out, we can turn to the actual
definition of the function |D f|.
%
The definition is given for a fixed (but arbitrary) |x|.
%
(At this point it is useful to briefly look back to the definition of
``limit of a function'' in Section~\ref{sec:LimitOfFunction}.)
%
The |lim| expression is using the (anonymous) function |g h = frac
(f(x+h) - f x) h| and that the limit of |g| is taken at |0|.
%
Note that |g| is defined in the scope of |x| and that its definition
uses |x| so it can be seen as having |x| as an implicit, first
argument.
%
To be more explicit we write |phi x h = frac (f(x+h) - f x) h| and take
the limit of |phi x| at 0.
%
So, to sum up, |D f x = lim (phi x) 0|.
%
\footnote{We could go one step further by noting that |f| is in the scope of |phi| and used in its definition.
%
Thus the function |psi f x h = phi x h|, or |psi f = phi|, is used.
%
With this notation, and |limAt x f = lim f x|, we obtain a point-free
definition that can come in handy:
%
|D f = limAt 0 . psi f|.}

The key here is that we name, type, and specify the operation of
computing the derivative (of a one-argument function).
%
We will use this operation quite a bit in the rest of the book, but
here are just a few examples to get used to the notation.

\begin{spec}
  sq x =  x^2
  double x = 2*x
  c2 x = 2
  sq' =  D sq = D (\x -> x^2) = D ({-"{}"-}^2) = (2*) = double
  sq'' = D sq' = D double = c2 = const 2
\end{spec}

What we cannot do at this stage is to actually \emph{implement} |D| in
Haskell.
%
If we only have a function |f : REAL -> REAL| as a ``black box'' we
cannot really compute the actual derivative |f' : REAL -> REAL|, only
numerical approximations.
%
But if we also have access to the ``source code'' of |f|, then we can
apply the usual rules we have learnt in calculus.
%
We will get get back to this question in section
\ref{sec:computingDerivatives}.

\subsection{Typing Mathematics: partial derivative}
\label{sec:typePartialDerivative}

Continuing on our quest of typing the elements of mathematical
textbook definitions we now turn to a functions of more than one
argument.
%
Our example here will is from page 169 of
\cite{maclane1986mathematics}, where we read

\begin{linenumbers}
\begin{quote}
  [...] a function |z = f (x, y)| for all points |(x, y)| in some open
  set |U| of the cartesian |(x, y)|-plane.
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
U    ⊆  cross ℝ ℝ          -- cartesian plane

f    :  U -> ℝ

z    :  U -> ℝ             -- but see below

f'x  :  U -> ℝ
\end{spec}
%
The |x| in the subscript of |f'| is \emph{not} a real number, but a symbol
(a |Char|).

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

The variable |h| appears to be a non-zero real number, bound by a
universal quantifier (``for |h ≠ 0|'' on line 4), but that is incorrect.
%
In fact, |h| is used as a local variable introduced in the subscript
of $lim$.
%
This variable |h| is a parameter of an anonymous function, whose limit
is then taken at |0|.

That function, which we can name |phi|, has the type |phi : U ->
(ℝ-{0}) -> ℝ| and is defined by

\begin{spec}
phi (x, y) h = (f (x + h, y) - f (x, y)) / h
\end{spec}

The limit is then |lim (phi (x, y)) 0|.
%
Note that |0| is a limit point of |ℝ-{0}|, so the type of |lim| is the
one we have discussed:

\begin{spec}
lim : (X -> ℝ) -> {p | p ∈ ℝ, Limp p X } -> ℝ
\end{spec}

On line 1, |z = f (x, y)| probably does not mean that |z ∈ ℝ|,
although the phrase ``the quantity |z|'' (on line 2) suggests this.
%
A possible interpretation is that |z| is used to abbreviate the
expression |f(x, y)|;
%
thus, everywhere we can replace |z| with |f(x, y)|.
%
In particular, |∂ z / ∂ x| becomes |∂ f (x, y) / ∂ x|, which we can
interpret as |∂ f / ∂ x| applied to |(x, y)| (remember that |(x, y)|
is bound in the context by a universal quantifier on line 4).
%
There is the added difficulty that, just like the subscript in |f'x|,
the |x| in |∂ x| is not the |x| bound by the universal quantifier, but
just a symbol.
%}

%TODO: perhaps mention "total derivative" at this stage. That could serve as an intermediate step towards the Langrangian, or could be added after it.

\subsection{Type inference and understanding: Lagrangian case study}
\label{sec:Lagrangian}

From (Sussman and Wisdom 2013):

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

\[
  \frac{d}{dt} \frac{∂L}{∂\dot{q}} - \frac{∂L}{∂q} = 0
\]

What could this expression possibly mean?

\end{quote}

To start answering the question, we start typing the elements involved:

\begin{enumerate}
\item The use of notation for ``partial derivative'', \(∂L / ∂q\),
  suggests that |L| is a function of at least a pair of arguments:
\begin{spec}
  L : ℝⁱ → ℝ,    i ≥ 2
\end{spec}

This is consistent with the description: ``Lagrangian function of the
system state (time, coordinates, and velocities)''.
%
So, if we let ``coordinates'' be just one coordinate, we can take |i =
3|:
%
\begin{spec}
  L : ℝ³ → ℝ
\end{spec}
%
The ``system state'' here is a triple (of type |S = (T, Q, V) = ℝ³|)
and we can call the the three components |t : T| for time, |q : Q| for
coordinate, and |v : V| for velocity.
%
(We use |T = Q = V = ℝ| in this example but it can help the reading to
remember the different uses of |ℝ|.)

\item Looking again at the same derivative, \(∂L / ∂q\) suggests that
  \(q\) is the name of a real variable, one of the three arguments to
  \(L\).
%
  In the context, which we do not have, we would expect to find
  somewhere the definition of the Lagrangian as
  %
  \begin{spec}
    L  :  (T, Q, V)  ->  ℝ
    L     (t, q, v)  =   ...
  \end{spec}

\item therefore, \(∂L / ∂q\) should also be a function of the same
  triple of arguments:

  \begin{spec}
    (∂L / ∂q) : (T, Q, V) -> ℝ
  \end{spec}

  It follows that the equation expresses a relation between
  \emph{functions}, therefore the \(0\) on the right-hand side is
  \emph{not} the real number \(0\), but rather the constant function
  |const 0|:

  \begin{spec}
    const 0  :  (T, Q, V)  →  ℝ
    const 0     (t, q, v)  =   0
  \end{spec}

\item We now have a problem: |d / dt| can only be applied to functions
  of \emph{one} real argument |t|, and the result is a function of one
  real argument:

%format dotq = "\dot{q}"
%format ddotq =  ∂ dotq
%format juxtapose f x = f "\," x
\begin{spec}
    juxtapose (d / dt) (∂L / ∂dotq)  :  T → ℝ
\end{spec}

Since we subtract from this the function \(∂L / ∂q\), it follows that
this, too, must be of type |T -> ℝ|.
%
But we already typed it as |(T, Q, V) → ℝ|, contradiction!
%
\label{item:L:contra}

\item The expression \(∂L / ∂\dot{q}\) appears to also be malformed.
%
  We would expect a variable name where we find \(\dot{q}\), but
  \(\dot{q}\) is the same as \(dq / dt\), a function.

\item Looking back at the description above, we see that the only
  immediate candidate for an application of \(d/dt\) is ``a path that
  gives the coordinates for each moment of time''.
%
  Thus, the path is a function of time, let us say
%
  \begin{spec}
    w  :  T → Q  -- with |T = ℝ| for time and |Q = ℝ| for coordinates (|q : Q|)
  \end{spec}

  We can now guess that the use of the plural form ``equations'' might
  have something to do with the use of ``coordinates''.
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
  We will use |n=1| for the rest of this example.

\item Now that we have a path, the coordinates at any time are given
  by the path.
  %
  And as the time derivative of a coordinate is a velocity, we can
  actually compute the trajectory of the full system state |(T, Q, V)|
  starting from just the path.
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
    expand : (T → Q) → (T → (T, Q, V))
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
  %
  which is used inside |d / dt|.

\item We now move to using |D| for |d / dt|, |D₂| for |∂ / ∂q|, and
  |D₃| for |∂ / ∂dotq|.
  %
  In combination with |expand w| we find these type correct
  combinations for the two terms in the equation:
  %
  \begin{spec}
    D ((D₂ L)  ∘  (expand w))  :  T → ℝ
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
  where we use |(==)| to avoid confusion with the equlity sign (|=|)
  used for the definition of the predicate.
\end{enumerate}

So, we have figured out what the equation ``means'', in terms of
operators we recognise.
%
If we zoom out slightly we see that the quoted text means something
like:
%
If we can describe the mechanical system in terms of ``a Lagrangian''
(|L : S -> ℝ|), then we can use the equation to check if a particular
candidate path |w : T → ℝ| qualifies as a ``motion of the system'' or
not.
%
The unknown of the equation is the path |w|, and as the equation
involves partial derivatives it is an example of a partial
differential equation (a PDE).
%
We will not dig into how to solve such PDEs, but they are widely used
in physics.

%TODO (by DaHe) There's two more things I think should be added in this
% chapter:
% * Typing the conditional probability notation. The notation P(A | B) is
%   something that I and others were confused during the statistics course. In one
%   assignment during that course, my solution claimed somthing along the
%   lines of that {P | A} was an event, that had a certain probability. So I
%   think many would agree that this is indeed a very confusing notation, so it
%   is a great idea to cover it in this book. Cezar had a very good rant about
%   this during his guest lecture last year.
%TODO: [Include the problem from E3.lhs and the solution from ../../Exam/2016-03/Ex2.hs]
% * Using Haskell to type mathematical expressions. I leared a lot about typing
%   maths by playing around with mathematical expressions in Haskell. A good
%   example is Exam/2016-03/Ex2.hs, where the solution is a haskell program
%   which compiles and type checks. This has a similar advantage to the 'typed
%   hole' method in the previous chapter, in that it encourages students to
%   take the trial-end-error approach that can often be used when solving
%   programming problems, and apply it to a mathematical context. I think this
%   chapter should include at least one example of using this method to type a
%   mathematical expression.

\subsection{Type in Mathematics (Part II)}

\subsubsection{Type classes}

The kind of type inference we presented in the last lecture becomes
automatic with experience in a domain, but is very useful in the
beginning.

The ``trick'' of looking for an appropriate combinator with which to
pre- or post-compose a function in order to makes types match is often
useful.
%
It is similar to the casts one does automatically in expressions such
as \(4 + 2.5\).

One way to understand such casts from the point of view of functional
programming is via \emph{type classes}.
%
As a reminder, the reason \(4 + 2.5\) works is because floating point
values are members of the class |Num|, which includes the member
function

\begin{spec}
  fromInteger   ::  Integer  ->  a
\end{spec}

which converts integers to the actual type |a|.

Type classes are related to mathematical structures which, in turn,
are related to DSLs.
%
The structuralist point of view in mathematics is that each
mathematical domain has its own fundamental structures.
%
Once these have been identified, one tries to push their study as far
as possible \emph{on their own terms}, i.e., without introducing other
structures.
%
For example, in group theory, one starts by exploring the consequences
of just the group structure, before one introduces, say, an order
structure and monotonicity.

The type classes of Haskell seem to have been introduced without
relation to their mathematical counterparts, perhaps because of
pragmatic considerations.
%
For now, we examine the numerical type classes |Num|, |Fractional|, and
|Floating|.

\begin{spec}
class  (Eq a, Show a) => Num a  where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs, signum    :: a -> a
    fromInteger    :: Integer -> a
\end{spec}

This is taken from the Haskell documentation\footnote{Fig. 6.2 in
  \href{https://www.haskell.org/onlinereport/haskell2010/haskellch6.html}{section
    6.4 of the Haskell 2010 report}: \cite[Sect.~6.4]{haskell2010}.}
but it appears that |Eq| and |Show| are not necessary, because there
are meaningful instances of |Num| which don't support them:
%
\begin{code}
instance Num a => Num (x -> a) where
  f + g        =  \x -> f x + g x
  f - g        =  \x -> f x - g x
  f * g        =  \x -> f x * g x
  negate f     =  negate . f
  abs f        =  abs . f
  signum f     =  signum . f
  fromInteger  =  const . fromInteger
\end{code}

This instance for functions allows us to write expressions like |sin +
cos :: Double -> Double| or |sq * double :: Integer -> Integer|.
%
As another example:
\begin{spec}
  sin^2 = \x -> (sin x)^(const 2 x) = \x -> (sin x)^2
\end{spec}
%
thus the typical math notation \(\sin^2\) works fine in Haskell.
%
(Note that there is a clash with another use of superscript for functions: sometimes |f^n| means \emph{composition} of |f| with itself |n| times.
%
With that reading \(sin^2\) would mean |\x->sin (sin x)|.)

%
Exercise: play around with this a bit in ghci.


\subsubsection{Overloaded integers literals}

As an aside, we will spend some time explaining a convenient syntactic
shorthand which is very useful but which can be confusing: overloaded
integers.
%
In Haskell, every use of an integer literal like |2|, |1738|, etc., is
actually implicitly an application of |fromInteger| to the literal.
%
This means that the same program text can have different meaning
depending on the type of the context.
%
The literal |three = 3|, for example, can be used as an integer, a real
number, a complex number, or even as a (constant) function (by the
instance |Num (x -> a)|).

The instance declaration of the method |fromInteger| above looks
recursive, but is not.
%
The same pattern appeared already in section
\ref{sec:firstFromInteger}, which near the end included roughly the
following lines:

\begin{spec}
instance Num r => Num (ComplexSyn r) where
  -- ... several other methods and then
  fromInteger = toComplexSyn . fromInteger
\end{spec}

To see why this is not a recursive definition we need to expand the
type and to do this I will introduce a name for the right hand side
(RHS): |fromIntC|.

\begin{verbatim}
--          ComplexSyn r <---------- r <---------- Integer
fromIntC =              toComplexSyn . fromInteger
\end{verbatim}

I have placed the types in the comment, with ``backwards-pointing''
arrows indicating that
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
  toComplexSyn   3.0                     ==  {- Def. of |toComplexSyn| from Section \ref{sec:toComplexSyn} -}
  FromCartesian  3.0  0                  ==  {- |Integer| literals have an implicit |fromInteger| -}
  FromCartesian  3.0  (fromInteger 0)    ==  {- |Num| instance for |Double|, again -}
  FromCartesian  3.0  0.0
\end{spec}

\subsubsection{Back to the numeric hierarchy instances for functions}

Back to the main track: defining numeric operations on functions.
%
We have already defined the operations of the |Num| class, but we can
move on to the neighbouring classes |Fractional| and |Floating|.

The class |Fractional| is for types which in addition to the |Num|
operations also supports division:
%
\begin{spec}
class  Num a => Fractional a  where
    (/)           :: a -> a -> a
    recip         :: a -> a             -- |\x -> 1/x|
    fromRational  :: Rational -> a      -- similar to |fromInteger|
\end{spec}
and the |Floating| class collects the ``standard'' functions from
calculus:
\begin{spec}
class  Fractional a => Floating a  where
    pi                   :: a
    exp, log, sqrt       :: a -> a
    (**), logBase        :: a -> a -> a
    sin, cos, tan        :: a -> a
    asin, acos, atan     :: a -> a
    sinh, cosh, tanh     :: a -> a
    asinh, acosh, atanh  :: a -> a
\end{spec}

We can instantiate these type classes for functions in the same way we
did for |Num|:

\begin{code}
instance Fractional a => Fractional (x -> a) where
  recip  f         =  recip . f
  fromRational     =  const . fromRational
\end{code}

\begin{code}
instance Floating a => Floating (x -> a) where
  pi       =  const pi
  exp f    =  exp . f
  f ** g   =  \ x -> (f x)**(g x)
  -- and so on
\end{code}

Exercise: complete the instance declarations.

These type classes represent an abstract language of algebraic and
standard operations, abstract in the sense that the exact nature of
the elements involved is not important from the point of view of the
type class, only from that of its implementation.


\subsection{Type classes in Haskell}

We now abstract from |Num| and look at what a type class is and how it
is used.
%
One view of a type class is as a set of types.
%
For |Num| that is the set of ``numeric types'', for |Eq| the set of
``types with computable equality'', etc.
%
The types in this set are called instances and are declared by
|instance| declarations.
%
When a class |C| is defined, there are no types in this set (no
instances).
%
In each Haskell module where |C| is in scope there is a certain
collection of instance declarations.
%
Here is an example of a class with just two instances:
%
\begin{code}
class C a where
  foo :: a -> a
instance C Integer where
  foo = (1+)
instance C Char where
  foo = toUpper
\end{code}
%
Here we see the second view of a type class: as a collection of
overloaded methods (here just |foo|).
%
Overloaded here means that the same symbol can be used with different
meaning at different types.
%
If we use |foo| with an integer it will add one, but if we use it with
a character it will convert it to upper case.
%
The full type of |foo| is |C a => a -> a| and this means that it can
be used at any type |a| for which there is an instance of |C| in
scope.

Instance declarations can also be parameterised:
\begin{code}
instance C a => C [a] where
  foo xs = map foo xs
\end{code}
This means that for any type |a| which is already an instance of |C|
we also make the type |[a]| an instance (recursively).
%
Thus, we now have an infinite collection of instances of |C|: |Char|,
|[Char]|, |[[Char]]|, etc.
%
Similarly, with the function instance for |Num| above, we immediately
make the types |x->Double|, |x->(y->Double)|, etc.\ into instances
(for all |x|, |y|, \ldots).

%TODO: parhaps make the ``looks recursive'' |fromInteger| example talk
% about |foo| instead? (And then just mention |fromInteger| shortly.)

\subsection{Computing derivatives}
\label{sec:computingDerivatives}

An important part of calculus is the collection of laws, or rules, for
computing derivatives.
%
Using the notation |D f| for the derivative of |f| and lifting the
numeric operations to functions we can fill in a nice table of
examples which can be followed to compute derivatives of many
functions:
%
\begin{spec}
    D (f + g)         =  D f + D g
    D (f * g)         =  D f * g + f * D g

    D (f ∘ g) x       =  D f (g x) * D g x     -- the chain rule

    D (const a)       =  const 0
    D id              =  const 1
    D (powTo n)  x    =  (n - 1) * (x^(n-1))
    D sin   x         =  cos x
    D cos   x         =  - (sin x)
    D exp   x         =  exp x
\end{spec}
%
and so on.
%

If we want to get a bit closer to actually implementing |D| we quickly
notice a problem: if |D| has type |(REAL -> REAL) -> (REAL -> REAL)|
we have no way of telling which of these rules we should apply.
%
Given a real (semantic) function |f| as an argument, |D| cannot know
if this function was written using a |+|, or |sin| or |exp| as
outermost operation.
%
The only thing |D| could do would be to numerically approximate the
derivative, and that is not what we are exploring in this course.
%
Thus we need to take a step back and change the type that we work on.
%
All the rules in the table seem to work on \emph{syntactic} functions:
abstract syntax trees \emph{representing} the real (semantic)
functions.

We observe that we can compute derivatives for any expressions made
out of arithmetical functions, standard functions, and their
compositions.
%
In other words, the computation of derivatives is based on a domain
specific langauge (a DSL) of expressions (representing functions in
one variable).
%
Here is the start of a grammar for this little language:
%
\begin{spec}
   expression  ∷=  const ℝ
               |   id
               |   expression + expression
               |   expression * expression
               |   exp expression
               |   ...
\end{spec}

We can implement this in a datatype:
\label{sec:FunExp}
\begin{code}
data FunExp  =  Const Double
             |  Id
             |  FunExp  :+:  FunExp
             |  FunExp  :*:  FunExp
             |  Exp FunExp
             -- and so on
             deriving Show
\end{code}

The intended meaning of elements of the |FunExp| type is functions:

\begin{code}
eval  ::  FunExp         ->  (Double -> Double)
eval      (Const alpha)  =   const alpha
eval      Id             =   id
eval      (e1 :+: e2)    =   eval e1  +  eval e2    -- note the use of ``lifted |+|'',
eval      (e1 :*: e2)    =   eval e1  *  eval e2    -- ``lifted |*|'',
eval      (Exp e1)       =   exp (eval e1)          -- and ``lifted |exp|''.
-- and so on
\end{code}
%
An example:
\begin{code}
f1 :: Double -> Double
f1 x = exp (x^2)
e1 :: FunExp
e1 = Exp (Id :*: Id)
\end{code}

We can implement the derivative of |FunExp| expressions using the
rules of derivatives.
%
We want to implement a function |derive :: FunExp -> FunExp| which
makes the following diagram commute:

\quad%
\begin{tikzcd}
  |FunExp| \arrow[r, "|eval|"] \arrow[d, "|derive|"]  & |Func| \arrow[d, "D"] \\
  |FunExp| \arrow[r, "|eval|"]                        & |Func|
\end{tikzcd}

In other words we want
%
\begin{spec}
     eval . derive e  =  D . eval
\end{spec}
or, in other words, for any expression |e|, we want
%
\begin{spec}
     eval (derive e)  =  D (eval e)
\end{spec}

For example, let us derive the |derive| function for |Exp e|:
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

Therefore, the specification is fulfilled by taking
%
\begin{spec}
derive (Exp e) = Exp e :*: derive e
\end{spec}

Similarly, we obtain
%
\begin{code}
derive     (Const alpha)  =  Const 0
derive     Id             =  Const 1
derive     (e1 :+: e2)    =  derive e1  :+:  derive e2
derive     (e1 :*: e2)    =  (derive e1  :*:  e2)  :+:  (e1  :*:  derive e2)
derive     (Exp e)        =  Exp e :*: derive e
\end{code}

Exercise: complete the |FunExp| type and the |eval| and |derive|
functions.

\subsection{Shallow embeddings}
\label{sec:evalD}

The DSL of expressions, whose syntax is given by the type |FunExp|,
turns out to be almost identical to the DSL defined via type classes
in the first part of this lecture.
%
The correspondence between them is given by the |eval| function.

The difference between the two implementations is that the first one
separates more cleanly from the semantical one.
%
For example, |:+:| \emph{stands for} a function, while |+| \emph{is}
that function.

The second approach is called ``shallow embedding'' or ``almost
abstract syntax''.
%
It can be more economical, since it needs no |eval|.
%
The question is: can we implement |derive| in the shallow embedding?

Note that the reason the shallow embedding is possible is that the
|eval| function is a \emph{fold}: first evaluate the sub-expressions
of |e|, then put the evaluations together without reference to the
sub-expressions.
%
This is sometimes referred to as ``compositionality''.

We check whether the semantics of derivatives is compositional.
%
The evaluation function for derivatives is

\begin{code}
eval'  ::  FunExp -> Double -> Double
eval'  =   eval . derive
\end{code}

For example:
%
\begin{spec}
     eval' (Exp e)                      =  {- def. |eval'|, function composition -}

     eval (derive (Exp e))		=  {- def. |derive| for |Exp| -}

     eval (Exp e :*: derive e)		=  {- def. |eval| for |:*:| -}

     eval (Exp e) * eval (derive e)	=  {- def. |eval| for |Exp| -}

     exp (eval e) * eval (derive e)	=  {- def. |eval'| -}

     exp (eval e) * eval' e             =  {- let |f = eval e|, |f' = eval' e| -}

     exp f * f'
\end{spec}
%
Thus, given only the derivative |f' = eval' e|, it is impossible to
compute |eval' (Exp e)|.
%
(There is no way to implement |eval'Exp :: (REAL -> REAL) -> (REAL ->
REAL)|.)
%
Thus, it is not possible to directly implement |derive| using shallow
embedding; the semantics of derivatives is not compositional.
%
Or rather, \emph{this} semantics is not compositional.
%
It is quite clear that the derivatives cannot be evaluated without, at
the same time, being able to evaluate the functions.
%
So we can try to do both evaluations simultaneously:

\begin{code}
type FD a = (a -> a, a -> a)

evalD ::  FunExp  ->  FD Double
evalD     e       =   (eval e, eval' e)
\end{code}
%
(Note: At this point, you are adviced to look up and solve exercise
\ref{exc:tuplingE1} on the ``tupling transform'' in case you have not
done so already.)

Is |evalD| compositional?

We compute, for example:
%
\begin{spec}
     evalD (Exp e)                           =  {- specification of |evalD| -}

     (eval (Exp e), eval' (Exp e))	     =  {- def. |eval| for |Exp| and reusing the computation above -}

     (exp (eval e), exp (eval e) * eval' e)  =  {- introduce names for subexpressions -}

     let  f   = eval e
          f'  = eval' e
     in (exp f, exp f * f')		     =  {- def. |evalD| -}

     let (f, f') = evalD e
     in (exp f, exp f * f')
\end{spec}

This semantics \emph{is} compositional and the |Exp| case is:
%
\begin{code}
evalDExp ::  FD Double  ->  FD Double
evalDExp     (f, f')  =   (exp f, exp f * f')
\end{code}
%
We can now define a shallow embedding for the computation of
derivatives, using the numerical type classes.

\begin{code}
instance Num a => Num (a -> a, a -> a) where
  (f, f')  +  (g, g')  =  (f  +  g,  f'      +  g'      )
  (f, f')  *  (g, g')  =  (f  *  g,  f' * g  +  f * g'  )
  fromInteger n        =  (fromInteger n, const 0)
\end{code}

Exercise: implement the rest

\subsection{Exercises}

%include E3.lhs
