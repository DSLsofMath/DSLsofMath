\section{Week 3: Types in Mathematics}
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module DSLsofMath.W03 where
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
\item \(d/dt : (ℝ → ℝ) → ℝ → ℝ\)
\item sometimes, instead of \(df/dt\) one sees \(f'\) or \(\dot{f}\) or |D f|
\item \(∂f/∂x_i : (ℝⁿ → ℝ) → ℝⁿ → ℝ\)
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

\subsection{Typing Mathematics: partial derivative}

As as an example we will try to type the elements of a mathematical
definition.

For example, on page 169 of \cite{maclane1986mathematics}, we read

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

\[
∂ z / ∂ x  =  f'_{x} (x, y) =
              \lim_{h \to 0} (f (x + h, y) - f (x, y)) / h
\]

What are the types of the elements involved?  We have

%{
%format f'x = "f'_{x}"
\begin{spec}
U    ⊆  cross ℝ ℝ          -- cartesian plane

f    :  U -> ℝ

z    :  U -> ℝ             -- but see below

f'x  :  U -> ℝ
\end{spec}
%}

The |x| in the subscript of |f'| is \emph{not} a real number, but a symbol
(a |Char|).

The expression |(x, y)| has several occurrences.
%
The first two denote variables of type |U|, the third is just a name
(|(x, y)|-plane).
%
The third denotes a variable of type |U|, it is bound by a universal
quantifier

\begin{spec}
∀ (x, y) ∈ U
\end{spec}

The variable |h| appears to be a non-zero real number, bound by a
universal quantifier, but that is incorrect.
%
In fact, |h| is used as a variable to construct the arguments of a
function, whose limit is then taken at |0|.

That function, which we can denote by |phi| has the type |phi : U ->
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

|z = f (x, y)| probably does not mean that |z ∈ ℝ|, although the
phrase ``the quantity |z|'' suggests this.
%
A possible interpretation is that |z| is used to abbreviate the
expression |f(x, y)|;
%
thus, everywhere we can replace |z| with |f(x, y)|.
%
In particular, |∂ z / ∂ x| becomes |∂ f (x, y) / ∂ x|, which we can
interpret as |∂ f / ∂ x| applied to |(x, y)| (remember that |(x, y)|
is bound in the context by a universal quantifier).
%
There is the added difficulty that, just like \(_{x}\), the |x| in |∂ x|
is not the |x| bound by the universal quantifier, but just a symbol.

\subsection{Type inference and understanding: Lagrangian case study}
\label{type-inference-and-understanding}

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
\item \(∂L / ∂q\) suggests that |L| is a function of at least a pair
  of arguments:
\begin{spec}
  L : ℝⁿ → ℝ,    n ≥ 2
\end{spec}

This is consistent with the description: ``Lagrangian function of the
system state (time, coordinates, and velocities)''.
%
So we can take |n = 3|:

\begin{spec}
  L : ℝ³ → ℝ
\end{spec}

\item \(∂L / ∂q\) suggests that \(q\) is the name of a real variable,
  one of the three arguments to \(L\).
%
  In the context, which we do not have, we would expect to find
  somewhere the definition of the Lagrangian as

\begin{spec}
  L (t, q, v) = ...
\end{spec}
\item therefore, \(∂L / ∂q\) should also be a function of a triple of
  arguments:

\begin{spec}
  ∂L / ∂q : ℝ³ → ℝ
\end{spec}

It follows that the equation expresses a relation between
\emph{functions}, therefore the \(0\) on the right-hand side is
\emph{not} the real number \(0\), but rather the constant function
\(0\):

\begin{spec}
  const 0  :  ℝ³ → ℝ
  const 0 (t, q, v) = 0
\end{spec}

\item We now have a problem: \(d / dt\) can only be applied to
  functions of \emph{one} real argument \(t\), and the result is a
  function of one real argument:

\[
\frac{d}{dt} \frac{∂L}{∂\dot{q}}  :  ℝ → ℝ
\]

Since we subtract from this the function \(∂L / ∂q\), it follows that
this, too, must be of type \(ℝ → ℝ\), contradiction.

\item The expression \(∂L / ∂\dot{q}\) appears to also be malformed.
%
  We would expect a variable name where we find \(\dot{q}\), but
  \(\dot{q}\) is the same as \(dq / dt\), a function.

\item Looking back at the description above, we see that the only
  candidate for an application of \(d/dt\) is ``a path that gives the
  coordinates for each moment of time''.
%
  Thus, the path is a function of time, let us say

\[
w  :  ℝ → ℝ, \mbox{where \(w(t)\) is a coordinate at time \(t\)}
\]

We can now guess that the use of the plural form ``equations'' might
have something to do with the use of ``coordinates''.
%
In an \(n\)-dimensional space, a position is given by \(n\)
coordinates.
%
A path would be a function

\begin{spec}
    w  :  ℝ → ℝⁿ
\end{spec}

which is equivalent to \(n\) functions of type \(ℝ → ℝ\).
%
We would then have an equation for each of them.
%
We will use |n=1| for the rest of this example.

\item The Lagrangian is a ``function of the system state (time,
  coordinates, and velocities)''.
%
  If we have a path, then the coordinates at any time are given by the
  path.
%
  The velocity is the derivative of the path, also fixed by the path:

%format dotq = "\dot{q}"
\begin{spec}
q  :  ℝ → ℝ
q t  =  w t

dotq : ℝ → ℝ
dotq t = dw / dt
\end{spec}

The equations do not use a function \(L : ℝ³→ ℝ\), but rather

\begin{spec}
  L ∘ expand w  :  ℝ → ℝ
\end{spec}

where the ``combinator'' |expand| is given by

\begin{spec}
  expand      :  (ℝ → ℝ) → ℝ → ℝ³
  expand w t  =  (t, w t, D w t)
\end{spec}

\item Similarly, using |D₁|, |D₂|, |D₃| instead of |∂L/∂t| etc., we
  have that, instead of |∂L/∂q| what is meant is

\begin{spec}
  D₂ L ∘ expand w  :  ℝ → ℝ
\end{spec}

and instead of |∂L/∂dotq|

\begin{spec}
  D₃ L ∘ expand w  : ℝ → ℝ
\end{spec}

The equation becomes

\begin{spec}
  D (D₃ L ∘ expand w)  -  D₂ L ∘ expand w  =  0
\end{spec}

a relation between functions of type |ℝ → ℝ|.
%
In particular, the right-hand |0| is the constant function

\begin{spec}
  const 0  :  ℝ → ℝ
\end{spec}

\end{enumerate}

\subsection{Types in Mathematics (Part II)}

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

TODO: insert proper citation \cite[Sect.~6.4]{haskell2010}

This is taken from the Haskell
documentation\footnote{Fig. 6.2 in section 6.4 of the Haskell 2010 report: \url{https://www.haskell.org/onlinereport/haskell2010/haskellch6.html}.}
but it appears that |Eq| and |Show| are not necessary, because there
are meaningful instances of |Num| which don't support them:

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

Next we have |Fractional| for when we also have division:
\begin{spec}
class  Num a => Fractional a  where
    (/)          :: a -> a -> a
    recip        :: a -> a
    fromRational :: Rational -> a
\end{spec}
and |Floating| when we can implement the ``standard'' funtions from calculus:
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


\subsection{Computing derivatives}

The ``little language'' of derivatives:

\begin{spec}
    D (f + g)    =  D f + D g
    D (f * g)    =  D f * g + f * D g

    D (f ∘ g) x  =  D f (g x) * D g x     -- the chain rule

    D (const a)  =  const 0
    D id         =  const 1
    D (^n)  x    =  (n - 1) * (x^(n-1))
    D sin   x    =  cos x
    D cos   x    =  - (sin x)
    D exp   x    =  exp x
\end{spec}

and so on.

We observe that we can compute derivatives for any expressions made
out of arithmetical functions, standard functions, and their
compositions.
%
In other words, the computation of derivatives is based on a DSL of
expressions (representing functions in one variable):

\begin{spec}
   expression  ∷=  const ℝ
               |   id
               |   expression + expression
               |   expression * expression
               |   exp expression
               |   ...
\end{spec}

etc.

We can implement this in a datatype:

\begin{code}
data FunExp  =  Const Double
             |  Id
             |  FunExp :+: FunExp
             |  FunExp :*: FunExp
             |  Exp FunExp
             -- and so on
             deriving Show
\end{code}

The intended meaning of elements of the |FunExp| type is functions:

\begin{code}
eval  ::  FunExp         ->  Double -> Double
eval      (Const alpha)  =   const alpha
eval      Id             =   id
eval      (e1 :+: e2)    =   eval e1  +  eval e2    -- note the use of ``lifted |+|''
eval      (e1 :*: e2)    =   eval e1  *  eval e2    -- ``lifted |*|''
eval      (Exp e1)       =   exp (eval e1)          -- and ``lifted |exp|''
-- and so on
\end{code}

We can implement the derivative of such expressions using the rules of
derivatives.
%
We want to implement a function |derive :: FunExp -> FunExp| which
makes the following diagram commute:

\quad%
\begin{tikzcd}
  |FunExp| \arrow[r, "|eval|"] \arrow[d, "|derive|"]  & |Func| \arrow[d, "D"] \\
  |FunExp| \arrow[r, "|eval|"]                        & |Func|
\end{tikzcd}

In other words, for any expression |e|, we want
%
\begin{spec}
     eval (derive e) = D (eval e)
\end{spec}

For example, let us derive the |derive| function for |Exp e|:

\begin{spec}
     eval (derive (Exp e))

  =  {- specification of |derive| above -}

     D (eval (Exp e))

  =  {- def. |eval| -}

     D (exp (eval e))

  =  {- def. |exp| for functions -}

     D (exp . eval e)

  =  {- chain rule -}

     (D exp . eval e) * D (eval e)

  =  {- |D| rule for |exp| -}

     (exp . eval e) * D (eval e)

  =  {- specification of |derive| -}

     (exp . eval e) * (eval (derive e))

  =  {- def. of |eval| for |Exp| -}

     (eval (Exp e)) * (eval (derive e))

  =  {- def. of |eval| for |:*:| -}

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

\subsection{Shallow embeddings}\label{sec:evalD}

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

\begin{spec}
     eval' (Exp e)

  =  {- def. |eval'|, function composition -}

     eval (derive (Exp e))

  =  {- def. |derive| for |Exp| -}

     eval (Exp e :*: derive e)

  =  {- def. |eval| for |:*:| -}

     eval (Exp e) * eval (derive e)

  =  {- def. |eval| for |Exp| -}

     exp (eval e) * eval (derive e)

  =  {- def. |eval'| -}

     exp (eval e) * eval' e
\end{spec}
%
and the first |e| doesn't go away.
%
The semantics of derivatives is not compositional.

Or rather, \emph{this} semantics is not compositional.
%
It is quite clear that the derivatives cannot be evaluated without, at
the same time, being able to evaluate the functions.
%
So we can try to do both evaluations simultaneously:
%
\begin{code}
type FD a = (a -> a, a -> a)

evalD ::  FunExp  ->  FD Double
evalD     e       =   (eval e, eval' e)
\end{code}

Is |evalD| compositional?

We compute, for example:

\begin{spec}
     evalD (Exp e)

  =  {- specification of |evalD| -}

     (eval (Exp e), eval' (Exp e))

  =  {- def. |eval| for |Exp| and reusing the computation above -}

     (exp (eval e), exp (eval e) * eval' e)

  =  {- introduce names for subexpressions -}

     let  f   = eval e
          f'  = eval' e
     in (exp f, exp f * f')

  =  {- def. |evalD| -}

     let (f, f') = evalD e
     in (exp f, exp f * f')
\end{spec}

This semantics \emph{is} compositional.
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
