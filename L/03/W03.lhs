\chapter{Types in Mathematics}
\label{sec:types}
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module DSLsofMath.W03 where
import Prelude hiding (Num(..),Fractional(..), Floating(..))
import DSLsofMath.Algebra (Algebraic(..),Transcendental(..))
type REAL = Double
type ℝ = REAL
type ℤ = Int
\end{code}
%
% (Based on ../../2016/Lectures/Lecture05 )
% Show "Functional Differential Geometry" p16: Lagrange_example.pdf

\section{Types of functions, expressions and big operators}
\label{sec:functions-and-scoping}
\paragraph{Examples of types in mathematics}

Simple types are sometimes mentioned explicitly in mathematical texts:

\begin{itemize}
\item \(x ∈ ℝ\)
\item \(\sqrt{\phantom{x}} : ℝ_{≥0} → ℝ_{≥0}\)
\item \((\_)² : ℝ → ℝ\) or, alternatively but \emph{not} equivalently
\item \((\_)² : ℝ → ℝ_{≥0}\)
\end{itemize}

However the types of big operators (sums, limits, integrals, etc.) are
usually not given explicitly. In fact, it may not be clear at first
sight that the summing operator ($\sum$) should be assigned a type at
all! Yet this is exactly what we will set out to do, dealing with a
dangerous pitfall of mathematical notation
(\cref{sec:scoping-pitfall}). However, to be able to do so
convincingly we shall clarify the relationship between functions and
expressions first.

\subsection{Expressions and functions of one variable}
\label{sec:expressions-of-one-var}
\begin{itemize}
\item $f(x) = x - 1$
\item $g(x) = 2*x^2 + 3$
\item $h(y) = 2*y^2 + 3$
\end{itemize}

As the reader may guess by now, we can assign to |f|, |g|, |h| the
type |ℝ -> ℝ|. But other choices could work, such as |ℤ -> ℤ|,
etc. For sure, they are functions. Additionally, the name of the
variable appears to play no role in the meaning of the functions, and
we can say, for example, |g = h|.

Consider now:
\begin{itemize}
\item $x - 1$
\item $2*x^2 + 3$
\item $2*y^2 + 3$
\end{itemize}

These are all expressions of one (free) variable. We could say that
they type is ℝ --- but this is assuming that the free variable also
has type ℝ. Furthermore, it is less clear now if |2*x + 3 = 2*y +
3|. In general one cannot simply change a variable name by another
without making sure that 1. the renaming is applied everywhere
uniformly and 2. the new variable name is not used for another purpose
in the same scope (otherwise one informally says that there is a
``clash'').

To clarify this situation, we will now formalise expressions of one
variables as a DSL. For simplicity we will focus on arithmetic
expressions only. Therefore we have constructors for addition,
multiplication and constants, as in \cref{sec:complex-arithmetic}.
Additionally, we have the all-important constructor for variables,
which we will call |X| here. We can implement all this in a datatype
as follows:\jp{Rename FunExp -> Exp1V or similar?}
\subsubsection{Deep embedding}
\label{sec:FunExp}
\begin{code}
data FunExp  =  Const REAL
             |  X
             |  FunExp  :+:  FunExp
             |  FunExp  :*:  FunExp
             |  Exp FunExp
\end{code}

We could encode our examples as follows:
\begin{itemize}
\item |X :+: Const (-1)|
\item |Const 2 :*: (X :*: X)  :+: Const 3|
\end{itemize}
We no longer have a third example: we can only ever represent one
variable, as |X|, and thus we skip the last example, equal to the second.

We can now evaluate the value of these expressions. The meaning of
operators and constants is as in \cref{sec:complex-arithmetic}. But,
to be able to evaluate |X|, the variable, we need its value --- and we
simply take it as a parameter.
\begin{code}
eval  ::  FunExp         ->  REAL  -> REAL
eval      (Const alpha)      x     = alpha
eval      X                  x     = x
eval      (e1 :+: e2)        x     = eval e1 x  +  eval e2 x
eval      (e1 :*: e2)        x     = eval e1 x  *  eval e2 x
\end{code}

However, we can make an equivalent interpretation of the above type as
\begin{spec}
eval  ::  FunExp         ->  (REAL  -> REAL)
\end{spec}
That is, |FunExp| can be interpreted as a function! This is perhaps
surprising, but the reason is that we used a fixed Haskell symbol
(constructor) for the variable. There is ever a single variable in
|FunExp|, and thus they are really equivalent to functions of a single
variable.

\subsubsection{Shallow embedding}
\label{sec:funexp-shallow}
Thus the above was a deep embedding for functions of a single
variable.  A shallow embedding would be a using functions as the
representation, say:

\begin{code}
type FunExpS = REAL -> REAL
\end{code}

Then we can define the operators directly on functions, as follows:

\begin{spec}
funConst alpha = \x -> alpha
funX = \x -> x
funPlus f g = \x -> f x + g x
funTimes f g = \x -> f x * g x
\end{spec}

Again, we have two possible intuitive readings of the above
equations. The first reading, as expressions of a single variable, is
that the variable ($x$) is interpreted as the identity function; a
constant |alpha| is interpreted as a constant function; the sum of two
expressions is interpreted as the sum of the evaluation of the
operands, etc.

The second reading is that we can define an arithmetic structure
(|*|,|+|, etc.) on functions, by lifting the operators to work
pointwise.\jp{This vocabulary is probably unknown at this point.}

To wrap it up, if we're so inclined, we can re-define the evaluator of
the deep-embedding using the operators of the shallow embedding:
\begin{spec}
eval  ::  FunExp         ->  REAL -> REAL
eval      (Const alpha)  =   funConst alpha
eval      X              =   funX
eval      (e1 :+: e2)    =   funPlus   (eval e1) (eval e2)
eval      (e1 :*: e2)    =   funTimes  (eval e1) (eval e2)
\end{spec}


Representing expressions of one variable as functions (of one
argument) is a recurring technique in this \course{} (\jp{Link back
  references to FunExp and function instances.}).  To start, we can
use it to assign types to big operators.

\subsection{Scoping and Typing big operators}
\label{sec:big-operators}
Consider the mathematical expression
\[
  \sum_{i=1}^n {i^2}
\]
To be able to see which type is appropriate for $\sum$, we have to
consider the type of the summand ($i^2$ in the example) first. As you
may have guessed, it is an expression of one variable ($i$). You may
ask: but surely the body of the summation operator can use other
variables? You'd be entirely correct. However, \emph{from the point of
  view of the summation}, it is as if such other variables were
constant. Accepting this assertion as a fact until we can show a more
complicated example, we can now assign a type to the summation
operator. For simplicity, we will be using the shallow embedding; thus
the operand can be typed as, say, $ℤ → ℝ$. The other arguments will be
the limit points ($1$ and $n$ in our example). The variable, $i$ shall
not be represented as an argument: indeed, the variable name is
\emph{fixed by the representation of functions}.  There is no choice
to make at the point of summation. Thus, we write:

\begin{code}
summation :: ℤ -> ℤ -> (ℤ -> ℝ) -> ℝ
\end{code}
Conveniently, we can even provide a simple implementation:
\begin{code}
summation low high f = sum [f i | i <- [low..high]]
\end{code}

As another example, let us represent the following nested sum
\[
  \sum_{i=1}^n \sum_{j=1}^m {i^2 + j^2}
\]
using the shallow embedding of summation. This representation can be written simply as follows:
\begin{spec}
exampleSum m n = summation 1 m (\i -> summation 1 n (\j -> i^2 + j^2))
\end{spec}
Aren't we cheating though? Surely we said that only one variable could
occur in the summand, but we see both |i| and |j|? Well, we are not
cheating as long as we use the \emph{shallow embedding} for functions of one
variables. Doing so allows us to 1. use lambda notation to bind (and
name) the variable name of the summation however we wish (in this case |i| and |j|) and 2. we can
freely use any haskell function of type |ℤ → ℝ| as the summand. In particular, the this function
can be any lambda-expression returing |ℝ|, and this expression can include summation itself. This freedom is an advantage of shallow embeddings:
if we were to use the deep embedding, then
we'd need a whole lot more work to ensure that we can represent
summation within the deep embedding. In particular we need a way to
embed variable binding itself. And we shall not be opening this
can of worms just yet, even though we take a glimpse in \cref{sec:multiple-variables}.


Sticking conveniently to the shallow embedding, we can apply the same
kind of reasoning to other big operators, and obtain the following typings:

\begin{itemize}
\item |lim : (ℕ → ℝ) → ℝ| for \(lim_{n → ∞} \{a_n\}\)
\item \(d/dt : (ℝ → ℝ) → (ℝ → ℝ)\)
  \begin{itemize}
  \item sometimes, instead of \(df/dt\) one sees \(f'\) or \(\dot{f}\) or |D f|
  \end{itemize}
\end{itemize}

In sum, the chief difficulty to overcome when assigning types for
mathematical operators is that they often introduce (bind) variable
names. To take another example from the above, $\lim_{n → ∞}$ binds
$n$ in $a_n$. In this book our stance is to make this binding obvious
by letting the body of the limit ($a_n$ in the example) be a
function. Thus we assign it the type $ℕ → ℝ$. Therefore the limit
operator has a higher order type. A similar line of reasoning
justifies the types of derivatives. We study in detail how these play
out first.

\section{Detour: expressions of several variables}
\label{sec:multiple-variables}

In first reading this section can be skipped, however it is natural to
to extend the study of expressions from single variables to multiple
variables.


\subsection{Partial functions}
\jp{move this in chapter 1 together with the discussion of functions.}
As an warmup, and for reasons which will become obvious soon (in
\cref{sec:ArithExp}), we begin by presenting a DSL for partial
functions with a finite domain.  The type |Env v s| will be the
\emph{syntax} for the type of partial functions from |v| to |s|, and
defined as follows:
%
\begin{code}
type Env v s = [(v,s)]
\end{code}
%
%
As an example value of this type we can take:
%
\begin{code}
env1 :: Env String Int
env1 = [("hey", 17), ("you", 38)]
\end{code}

The intended meaning is that |"hey"| is mapped to |17|, etc.  The
semantic domain is the set of partial functions, and, as discussed
above, we represent those as the Haskell type |v -> Maybe s|.

Our evaluation function, |evalEnv|, maps the syntax to the semantics,
and as such has the following type:
%
\begin{code}
evalEnv :: Eq v =>  Env v s -> (v -> Maybe s)
\end{code}
%
This type signature deserves some more explanation.
%
The first part (|Eq v =>|) is a constraint which says that the
function works, not for \emph{all} types |v|, but only for those who
support a boolean equality check (|(==) :: v -> v -> Bool|).
%
The rest of the type signature (|Env v s -> (v -> Maybe s)|) can be
interpreted in two ways: either as the type of a one-argument function
taking an |Env v s| and returning a function, or as the type of a
two-argument function taking an |Env v s| and a |v| and maybe
returning an |s|.

The implementation proceeds by searching for the first occurence of
|x| In the list of pairs |(v,s)| such that |x==v|, and
return |Just s| if one is found, and |Nothing| otherwise.
%**TODO: Explain |where| clause syntax
%**TODO: Explain boolean guards
\begin{code}
evalEnv vss x  =  findFst vss
  where  findFst ((v,s):vss)
           | x == v         =  Just s
           | otherwise      =  findFst vss
         findFst []         =  Nothing
\end{code}
%
Another equivalent definition is |evalEnv = flip lookup|, where
|lookup| is defined in the Haskell Prelude:
%
\begin{spec}
lookup :: Eq a => a -> [(a, b)] -> Maybe b
\end{spec}

\subsection{The data type of multiple variables expressions}
\label{sec:ArithExp}
Let us define the following type, describing a deep embedding for
simple arithmetic expressions. Compared to single variable
expressions, we add one argument for variables, giving the \emph{name}
of the variable. Here we use a string, so we have an infinite supply
of variables.  \jp{rename type/constructors to match single variable
  expressions}
\jp{There does not seem to be sense or rhyme in the name of data types and constructors.}
\begin{code}
data AE = V String | P AE AE | T AE AE
\end{code}


The above declaration introduces:\jp{move this kind of consideration much earlier. Haskell primer?}
\begin{itemize}
\item a new type |AE| for simple arithmetic expressions,
\item a constructor |V :: String -> AE| to represent variables,
\item a constructor |P :: AE -> AE -> AE| to represent plus, and
\item a constructor |T :: AE -> AE -> AE| to represent times.
\end{itemize}

Example values include |x = V "x"|, |e1 = P x x|, and |e2 = T e1 e1|.

If you want a constructor to be used as an infix operator you need to use
symbol characters and start with a colon:

\begin{spec}
data AE' = V' String | AE' :+ AE' | AE' :* AE'
\end{spec}

Example values are then |y = V' "y"|, |e1 = y :+ y| and |e2 = x :* e1|.

Finally, you can add one or more type parameters to make a whole family
of datatypes in one go:\jp{move this kind of consideration much earlier. Haskell primer?}

\begin{code}
data AE' v = V' v | AE' v :+ AE' v | AE' v :* AE' v
\end{code}
%
The purpose of the parameter |v| here is to enable a free choice of
type for the variables (be it |String| or |Int| or something else).

The careful reader will note that the same Haskell module cannot
contain both these definitions of |AE'|.\jp{move this kind of consideration much earlier. Haskell primer?}
%
This is because the name of the type and the names of the constructors
are clashing.
%
The typical ways around this are either to define the types in different
modules, or rename one of them (often by adding primes as in |AE'|).
%
In this \course{} we often take the liberty of presenting more than one
version of a datatype without changing the names, to avoid multiple
modules or too many primed names.


Together with a datatype for the syntax of arithmetic expressions we
 want to define an evaluator of the expressions.

In the evaluator for |AE| we take this idea one step further: given an
environment |env| and the syntax of an arithmetic expression |e| we
compute the value of that expression. Hence, the semantics of |AE| is
a function of type |Env String Integer -> Maybe Integer|.
%
%*TODO: perhaps switch Times to Div to further "motivate" the use of |Maybe|. This would require changing the type (above) and a few lines below.
%**TODO explain more for those not used to Haskell
\begin{code}
evalAE :: AE -> (Env String Integer -> Maybe Integer)
evalAE (V x)     env  =  evalEnv env x
evalAE (P e1 e2) env  =  mayP  (evalAE e1 env)  (evalAE e2 env)
evalAE (T e1 e2) env  =  mayT  (evalAE e1 env)  (evalAE e2 env)

mayP :: Maybe Integer -> Maybe Integer -> Maybe Integer
mayP (Just a) (Just b)  =  Just (a+b)
mayP _        _         =  Nothing

mayT :: Maybe Integer -> Maybe Integer -> Maybe Integer
mayT (Just a) (Just b)  =  Just (a*b)
mayT _        _         =  Nothing
\end{code}

The corresponding code for |AE'| is more general and you don't need to
understand it at this stage, but it is left here as an example for
those with a stronger Haskell background.\jp{Actually the AE/AE'  generalisation has nothing to do with the change in code.}
%
\begin{code}
evalAE' :: (Eq v, _) =>  (Env v sem) -> (AE' v -> Maybe sem)
evalAE' env (V' x)      =  evalEnv env x
evalAE' env (e1 :+ e2)  =  liftM (+)   (evalAE' env e1)  (evalAE' env e2)
evalAE' env (e1 :* e2)  =  liftM (*)   (evalAE' env e1)  (evalAE' env e2)

liftM :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
liftM op   (Just a)  (Just b)  =  Just (op a b)
liftM _op  _         _         =  Nothing
\end{code}

The approach taken above is to use a |String| to name each variable:
indeed, |Env String REAL| is like a tuple of several variables values.
However, other situations, it is better to refer to variables by
position.

For example, we can pick out any variable and make it a function of
said variable like so:

\begin{code}
fun1 :: (Env String REAL -> REAL) -> Env String REAL -> String -> (REAL -> REAL)
fun1 funMultiple env variable value = funMultiple ((variable,value):env)
\end{code}

\begin{exercise}
  Assume a function |f| of 3 variables, named |"x"|,|"y"| and |"y"|,
  and given the type |Env String REAL -> REAL|. Turn it into a
  function |g| of type | REAL -> REAL -> REAL -> REAL| with the same
  intended meaning.
\end{exercise}

\jp{Talk some about variable capture?}

%*TODO: Perhaps add simple exercises on renaming and variable capture

\section{Typing Mathematics: derivative of a function}
\label{sec:typeDerivative}

Let's start with the classical definition of the derivative
of \citet{adams2010calculus}:
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
To make it easier to see we use a prefix |D| instead and we can
thus write |D : (X->REAL) -> (Y->REAL)|.
%
We will often assume that |X = Y| (|f| is differentiable everywhere) so that we can can see |D| as
preserving the type of its argument.

Now, with the type of |D| sorted out, we can turn to the actual
definition of the function |D f|.
%
The definition is given for a fixed (but arbitrary) |x|.
%
(At this point the reader may want to check the definition of
``limit of a function'' in \refSec{sec:LimitOfFunction}.)
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
\jp{But in chapter 2 lim was a predicate? Check.}
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
We will get get back to this question in \refSec{sec:computingDerivatives}.



\section{Typing Mathematics: partial derivative}
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

The variable |h| is a non-zero real number. The use of the word ``for'' might lead one to believe that it is bound by a
universal quantifier (``for |h ≠ 0|'' on line 4), but that is incorrect.
%
In fact, |h| is used as a local variable introduced in the subscript
of $\lim$.
%
This variable |h| is a parameter of an anonymous function, whose limit
is then taken at |0|.

That function, which we can name |phi|, has the type |phi : U ->
(ℝ-{0}) -> ℝ| and is defined by
%
\begin{spec}
phi (x, y) h = (f (x + h, y) - f (x, y)) / h
\end{spec}
%
The limit is then written |lim (phi (x, y)) 0|.
%
Note that |0| is a limit point of |ℝ-{0}|, so the type of |lim| is the
one we have discussed:
%
\begin{spec}
lim : (X -> ℝ) -> {p | p ∈ ℝ, Limp p X } -> ℝ
\end{spec}

On line 1, |z = f (x, y)| probably does not mean that we let |z| be a fixed value in |ℝ|,
although the phrase ``the quantity |z|'' (on line 2) suggests this.
%
Rather, a possible interpretation is that |z| is used to abbreviate the
expression |f(x, y)|. That is, |z| stands for an expression which depends on |x| and |y|;
%
thus, it can be enlightening to replace |z| with |f(x, y)| everywhere.
%
In particular, |∂ z / ∂ x| becomes |∂ f (x, y) / ∂ x|, which we can
interpret as the operator |∂ / ∂ x| applied to |f(x, y)| (remember that |(x, y)|
is bound in the context by a universal quantifier on line 4).
%
There is the added difficulty that, just like the subscript in |f'x|,
the |x| in |∂ x| is not the |x| bound by the universal quantifier, but
just a symbol.
%}

To sum up, partial derivative operators which mention symbols (such as
|∂ / ∂ x| or ``prime subscript $x$'') do act on an representation of
functions which uses symbols for the variables (not positions), such
as presented in \cref{sec:ArithExp}.
This is why we mostly see \(∂f/∂x\), \(∂f/∂y\), \(∂f/∂z\) etc. when, in the
  context, the function \(f\) has been given a definition of the form
  \(f (x, y, z) = \ldots\).
This kind of approach presents
several difficulties:

\begin{enumerate}
\item it makes it hard to rename variables (which can be a problem if
  one is renaming variables, for example for the purpose of
  integration)
\item Further confusion can be created when a variable (such as $z$
  above) depends on other variables. Tracing dependencies can
  become daunting and it is easy to make errors of name when doing
  calculations.
\item it makes it difficult to assign a higher order type to the
  partial derivatives. Indeed, as we have seen in
  \cref{sec:big-operators}, doing this means that the operator binds
  the name of the variable. But it is often awkward to make partial
  differentiation bind a variable.
\end{enumerate}

One possibility would be to use the following type:
\(∂/∂x_i : (ℝⁿ → ℝ) → (ℝⁿ → ℝ)\) But it still assume as input a vector
of variables $x$.  Hence we prefer a notation which doesn't rely on
the names given to the arguments whatsoever. It was popularised by
\citet{landau1934einfuhrung} (English edition
\cite{landau2001differential}): \(D₁\) for the partial derivative with
respect to the the first argument, \(D_2\) for the partial derivative with
respect to the the second argument, etc.

Exercise~\ref{exc:D1usingD}: for \(f : ℝ² → ℝ\) define \(D₁\) and \(D₂\) using only \(D\).

%TODO: perhaps mention "total derivative" at this stage. That could serve as an intermediate step towards the Langrangian, or could be added after it.

\section{Type inference and understanding: Lagrangian case study}
\label{sec:Lagrangian}

From (Sussman and Wisdom 2013):\jp{fix citation}

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
%
What could this expression possibly mean?
\end{quote}

To start answering the question of Sussman and Wisdom, we start typing the elements involved:

\begin{enumerate}
\item The use of notation for ``partial derivative'', \(∂L / ∂q\),
  suggests that |L| is a function of at least a pair of arguments:
\begin{spec}
  L : ℝⁱ → ℝ,    i ≥ 2
\end{spec}
%
This is consistent with the description: ``Lagrangian function of the
system state (time, coordinates, and velocities)''.
%
So, if we let ``coordinates'' be just one coordinate, then there is
also a single velocity\footnote{A bit of domain knowledge is necessary
  here} and so we can take |i = 3|:
%
\begin{spec}
  L : ℝ³ → ℝ
\end{spec}
%
The ``system state'' here is a triple (of type |S = (T, Q, V) = ℝ³|)
and we can call the three components |t : T| for time, |q : Q| for
coordinate, and |v : V| for velocity.
%
(We use |T = Q = V = ℝ| in this example but it can help the reading to
remember the different uses of |ℝ| --- this would help for example to
generalise to more than one coordinate.)

\item Looking again at the same derivative, \(∂L / ∂q\) suggests that
  \(q\) is the name of a real variable, one of the three arguments to
  \(L\).
%
  In the context, which we do not have, we would expect to find
  somewhere the definition of the Lagrangian as
  %**TODO: Perhaps write T x Q x V on the type level instead of (T, Q, V) to avoid confusion. If so there needs to be an explanation of the relation to Haskell syntax somewhere and several places need changes below for consistency.
  \begin{spec}
    L  :  (T, Q, V)  ->  ℝ
    L     (t, q, v)  =   ...
  \end{spec}

\item therefore, \(∂L / ∂q\) should also be a function of the same
  triple of arguments:
%
  \begin{spec}
    (∂L / ∂q) : (T, Q, V) -> ℝ
  \end{spec}
%
  It follows that the equation expresses a relation between
  \emph{functions}, therefore the \(0\) on the right-hand side of the Lagrange equation(s) is
  \emph{not} the real number \(0\), but rather the constant function
  |const 0|:

  \begin{spec}
    const 0  :  (T, Q, V)  →  ℝ
    const 0     (t, q, v)  =   0
  \end{spec}

\item We now have a problem: |d / dt| can only be applied to functions
  of \emph{one} real argument |t|, and the result is a function of one
  real argument:
%
%format dotq = "\dot{q}"
%format ddotq =  ∂ dotq
%format juxtapose f x = f "\," x
\begin{spec}
    juxtapose (d / dt) (∂L / ∂dotq)  :  T → ℝ
\end{spec}
%
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
  And because the time derivative of a coordinate is a velocity, we can
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
  where we use |(==)| to avoid confusion with the equality sign (|=|)
  used for the definition of the predicate.
\end{enumerate}

So, we have figured out what the equation ``means'', in terms of
operators that we recognise.
%
If we zoom out slightly we see that the quoted text means something
like:
%
If we can describe the mechanical system in terms of ``a Lagrangian''
(|L : S -> ℝ|)\jp{what is S?}, then we can use the equation to check if a particular
candidate path |w : T → ℝ| qualifies as a possible ``motion of the system'' or
not.
%
The unknown of the equation is the path |w|, and as the equation
involves partial derivatives it is an example of a partial
differential equation (a PDE).
%
We will not dig into how to solve such PDEs, but they are widely used
in physics.

\section{Incremental analysis with types}
So far we have worked on typing mathematics ``by hand'', but we can
actually get the Haskell interpreter to help a bit even when we are
still at the specification stage.
%
It is often useful to collect the known (or assumed) facts about types
in a Haskell file and regularly check if the type checker agrees.
%
Consider the following text from \citeauthor{maclane1986mathematics}'s
\textit{Mathematics Form and Function} (page 182):
%

\begin{linenumbers}
   \begin{quote}
     In these cases one tries to find not the values of
     |x|\linelabel{line:exam1603_x} which make a given function |y =
     f(x)| a minimum, but the values of a given function |f(x)| which
     make a given quantity a minimum.  Typically, that quantity is
     usually measured by an integral whose integrand is some
     expression |F|\linelabel{line:exam1603_F} involving both |x|,
     values of the function |y = f(x)| at interest and the values of
     its derivatives --- say an integral
   \end{quote}
  \begin{linenomath*}
    \[\int_a^b F(y, y', x)dx,\quad y = f(x).\] \linelabel{line:exam1603_int}
  \end{linenomath*}
\end{linenumbers}
%
\lnOnly{Typing the variables and the integration operators in this text was an
  exam question in 2016.}
%
We will use the above example as an example of getting
feedback from a type checker.
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
To the Haskell interpreter, such empty |data|-declarations mean that
there is no way to construct any element for them, as we saw in \cref{sec:curry-howard}. But at this stage
of the specfication, we will use this notation to indicate that we do
not know anything about values of those types.
%
Similarly, |f| has a type, but no proper implementation.
%
We will declare types for the rest of the variables as well, and as we
are not implementing any of them right now, we can just make one
``dummy'' implementation of a few of them in one go:
%
\begin{code}
(x, deriv, ff, a, b, int) = undefined
\end{code}
%
We write |ff| for the capital |F| (to satisfy Haskell rules for
variable names), |deriv| for the postfix prime, and |int| for the
integral operator.
%
On line~\ref{line:exam1603_x} ``values of |x|'' hints at the type |X|
for |x| and the way |y| is used indicates that it is to be seen as an
alias for |f| (and thus must have the same type)
%
As we have discussed above, the derivative normally preserves the type
and thus we can write:
%
\begin{code}
x   :: X
y   :: X -> Y
y   =  f
y'  :: X -> Y
y'  =  deriv f
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
the type checker is happy with them.
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

One difficulty when reading mathematics (and applying them to
programming) is \emph{overloading}. For our purposes, we say that a
symbol is \emph{overloaded} when its meaning depends on the type of
the expressions that it applies to.

Consider for example the operator ($+$). According to usual
mathematical notation, one can typically use it to add integers,
rational numbers, real numbers, complex numbers, etc. and it poses no
difficulty. We explore the mathematical reasons in more detail in
\cref{sec:AlgHomo}, but for now we will concentrate on the
view of functional programming of this problem: one  way to understand overloading is
via \emph{type classes}.

In Haskell both $4 == 3$ and $3.4 == 3.2$ typecheck because both
integers and floating point values are member of the |Eq| class, which
we can safely assume to be defined as follows:

\begin{spec}
class Eq a where
  (==) :: a -> a -> Bool
\end{spec}
%
The above declaration does two things. First, it defines a set of
types which have equality test. One can tell the Haskell compiler that
certain types belong to this set by using instance declarations, which
additionally provide an implementation for the equality test. For example,
we can make |Bool| member of the |Eq| using
the following declaration:
\begin{spec}
instance Eq Bool where
  True == True = True
  False == False = True
  _ == _ = False
\end{spec}
(The Haskell compiler will in fact provide instances for primitive types).

Second, the |Eq| class declaration provides an operator |(==)| of type
|Eq a => a -> a -> Bool|. One can use the operator on any type |a|
which belongs to the |Eq| set. This is expressed in general by a
constraint |Eq a| occuring before the |=>| symbol.

Instance declarations can also be parameterised on another
instance. Consider for example:
\begin{spec}
instance Eq a => Eq [a] where
  (==) = ... -- exercise
\end{spec}
%
In the above, the expression |Eq a => Eq [a]| means that for any type |a| which is already an instance of |Eq|
we also make the type |[a]| an instance of Eq.
%
Thus, for example, by recursion we now have an infinite collection of instances of |Eq|: |Char|,
|[Char]|, |[[Char]]|, etc.
%

\subsection{Numeric operations}
Haskell also provides a |Num| class, containing various numeric types
(Int, Double, etc) with several operators (|+|,|*|, etc).
Unfortunately, the |Num| class was designed with more regard for
implementation quirks than mathematical structure, and thus it is a
poor choice for us. We take a more principled approach instead,
and define the following classes, which together, serve a similar
role as |Num|, and which we study in more detail in
\cref{sec:ring-like-classes}:

\label{sec:numeric-classes}
\begin{code}
class Additive a where
  zero :: a
  (+) :: a -> a -> a
class Additive a => AddGroup a where
  negate :: a -> a
class Multiplicative a where
  one :: a
  (*) :: a -> a -> a
class Multiplicative a => MulGroup a where
  recip :: a -> a -- reciprocal
\end{code}
The operator names clash with the |Num| class, which we will avoid
from now one in favour |Additive| and |Multiplicative|.

\begin{exercise}
  Consider the exponentiation operator, which we can write
  |(^)|. Taking advantage of the above classes, propose a possible
  type for it and sketch an implementation.
\end{exercise}
\begin{solution}
  One possibility is |(^) :: Field a => a -> Int -> a|. For positive
  exponents, one can use repeated multiplication. For negative
  exponents, one can use repeated division.
\end{solution}
\jp{I could not understand what this was referring to:
The ``trick'' of looking for an appropriate combinator with which to
pre- or post-compose a function in order to makes type match is often
useful.}
% %
% It is similar to the type-casts one does automatically in expressions such
% as \(4 + 2.5\).
% TODO: here we also use defaulting at the same time, which is 1. not principled and 2. does not work with custom classes.

Given these definitions, both expressions $4 + 3$ and $3.4 + 3.2$
typecheck.


\subsection{Overloaded integer literals}
\label{sec:overloaded-integer-literals}
We will spend some time explaining a convenient Haskell-specific
syntactic shorthand which is very useful but which can be confusing:
overloaded integers.  In Haskell, every use of an integer literal like
|2|, |1738|, etc., is actually implicitly an application of
|fromInteger| to the literal typed as an |Integer|.

But what is |fromInteger|? It is a function that maps every value and
operator in the numeric classes to an arbitrary type |a| in those
classes. We can implement it as follows:
\begin{code}
fromInteger :: (AddGroup a, Multiplicative a) => Integer -> a
fromInteger n  | n < 0 = negate (fromInteger (negate n))
               | n == 0 = zero
               | otherwise = one + fromInteger (n - 1)
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

\jp{Tikz-ify this}
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

As an example, consider again the DSL of expressions of one variables.
We saw that such expressions can be represented by the shallow
embedding |REAL -> REAL|.  Using type classes, we can use the usual
operators names instead of |evalPlus|, |evalTimes|, etc. We could write:
\begin{spec}
instance Additive (REAL -> REAL) where
  f + g        =  \x -> f x + g x
  zero         =  const zero
\end{spec}

The instance declaration of the method |zero| above looks recursive,
but is not: |zero| is used at a different type on the left- and
right-hand-side of the equal sign, and thus refers to two different
functions. One the left-hand-side we define |zero :: REAL -> REAL|,
while on the right-hand-side we use |zero :: REAL|.

However, as one may suspect, for functions, we can use any domain
and any numeric co-domain in place of |REAL|. Therefore we prefer to
define the following, more general instances:
\label{sec:FunNumInst}

\begin{code}
instance Additive a => Additive (x -> a) where
   f + g        =  \x -> f x + g x
   zero = const zero

instance Multiplicative a => Multiplicative (x -> a) where
   f * g        =  \x -> f x * g x
   one = const one

instance AddGroup a => AddGroup (x -> a) where
   negate f     =  negate . f

instance MulGroup a => MulGroup (x -> a) where
   recip f     =  recip . f

instance Algebraic a => Algebraic (x -> a) where
   sqrt f     =  sqrt . f

instance Transcendental a => Transcendental (x -> a) where
   pi = const pi
   sin f =  sin . f
   cos f =  cos . f
   exp f =  exp . f
\end{code}

Here we extend our set of type-classes to cover algebraic and
transcendental numbers.  Together, these type classes represent an
abstract language of abstract and standard operations, abstract in
the sense that the exact nature of the elements involved is not
important from the point of view of the type class, only from that of
its implementation. What does matter for the class (but is not
captured in the Haskell definition of the class), is the relationship
between various operations (for example addition should distribute
over multiplication).

These instances for functions allow us to write expressions which are
very commonly used in math books, such as |f+g| for the sum of two functions |f|
and |g|, say |sin + cos :: Double -> Double|. Somewhat less common
notations, like |sq * double :: Integer -> Integer| are also
possible. They have a consistent meaning: the same argument is passed
to all functions in an expression.
%
As another example, we can write |sin^2|, which the above instance
assigns the following meaning:
\begin{spec}
  sin^2 = \x -> (sin x)^(const 2 x) = \x -> (sin x)^2
\end{spec}
%
thus the typical math notation \(\sin^2\) can work fine in Haskell,
provided the above instances for functions, assuming a fixed argument.
%
(Note that there is a clash with another common use of superscript for
functions in mathematical texts: sometimes |f^n| means
\emph{composition} of |f| with itself |n| times.
%
With that reading \(sin^2\) would mean |\x->sin (sin x)|.)

%
\begin{exercise}
  Experiment with this feature using ghci, for example by evaluating
  |sin + cos| at various points.
\end{exercise}

Something which may not be immediately obvious, but is nonetheless
useful, is that all the above instances are of the form |C a => C (x
-> a)| and are therefore parametric. This means that, for example,
given the instance |Additive a => Additive (x -> a)| and the intstance
|Additive REAL|, we have that the types |a->REAL|, |a->(b->REAL)|,
etc. are all instances of |Additive|. Consequently, we can use the
usual mathematical operators for functions taking any number of
arguments --- provided that they match in number and types.


\section{Computing derivatives}
\label{sec:computingDerivatives}
% What is the purpose of the section?

%   Elements of answer:
%   - we have laws at the semantic level (coming from the definition of limit), and we want to derive laws at a syntactic level
%   - point out that such laws are effectively used as computational rules (even though they are not really)

%   It is a stepping stone towards showing an example of non-trivial
%   compositionality for the next chapter.

An important part of calculus is the collection of laws, or rules, for
computing derivatives. They are provided by \citet{adams2010calculus}
as a series of theorems, starting at page 108 of their book. We we
can summarize those as follows:
\begin{spec}
  (f+g)'(x)  =   f'(x) + g'(x)
  (f*g)'(x)  =   f'(x) g(x) + f(x) g'(x)
  (C*f)'(x)  =   C*f'(x)
  (f . g)(x) =   f' (g (x)) * g'(x)          -- chain rule
\end{spec}
(After a while, \citeauthor{adams2010calculus} switch to
differential notation, so we omit corresponding rules for
trigonometric and exponential functions.)
%
Using the notation |D f| for the derivative of |f| and applying the
numeric operations to functions directly, we can fill in a table of
examples which can be followed to compute derivatives of many
functions:
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
notice a problem: if |D| has type |(REAL -> REAL) -> (REAL -> REAL)|
have no way to turn the above specification into a program, because
the program has no way of telling which of these rules we should
apply.
%
That is, given an extensional (semantic\jp{shallow}) function |f|, the
only thing that we can ever do is to evaluate |f| at given points, and
thus we cannot know if this function was written using a |+|, or |sin|
or |exp| as outermost operation.
%
The only thing that a derivative operator could do would be to numerically approximate the
derivative, and that is not what we are exploring in this course.
%
Thus we need to take a step back and change the type that we work on.
%
Even though the rules in the table are obtained by reasoning
semantically, using the definition of limit for functions (of type |ℝ
→ ℝ|), they are really intended to be used on \emph{syntactic}
functions: abstract syntax trees \emph{representing} the (semantic)
functions.

We observe that we can compute derivatives for any expression made
out of arithmetical functions, standard functions, and their
compositions.
%
In other words, the computation of derivatives is based on a domain
specific language (a DSL) of expressions (representing functions in
one variable).
%
Hence we can then implement the derivative of |FunExp| expressions
using the rules of derivatives. Because the specification of
derivation rules is already in the right format, the way to obtain
this implementation may seem obvious, but we will go through the steps
as a way to show the process in a simple case.

Our goal is want to implement a function |derive :: FunExp -> FunExp| which
makes the following diagram commute:

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
in turn this means that for any expression |e :: FunExp|, we want
%
\begin{spec}
     eval (derive e)  =  D (eval e)
\end{spec}

For example, let us calculate the |derive| function for |Exp e|:
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
\begin{code}
derive     (Const alpha)  =  Const 0
derive     X              =  Const 1
derive     (e1 :+: e2)    =  derive e1  :+:  derive e2
derive     (e1 :*: e2)    =  (derive e1  :*:  e2)  :+:  (e1  :*:  derive e2)
derive     (Exp e)        =  Exp e :*: derive e
\end{code}
%
\begin{exercise}
Complete the |FunExp| type and the |eval| and |derive|
functions.
\end{exercise}

%include E3.lhs
