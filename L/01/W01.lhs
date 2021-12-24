%{
%format bi = "\Varid{bi}"
\chapter{Types, Functions, and DSLs for Expressions}
\label{sec:DSLComplex}

%**TODO: Better road-mapping needed for this chapter
In this chapter we exemplify our method by applying it to the
domain of types and functions first, and complex numbers second, which we
assume most readers will already be familiar with.
%
While doing this we also introduce some of the Haskell concepts needed
later.

%
We will implement certain concepts in Haskell and
%
\index{module (Haskell)@@|module| (Haskell)}%
%
the code for this chapter is placed in a module called
|DSLsofMath.W01| that starts here.
%
It is strongly recommended to try out the different examples, to play
with the code and to make different edits and tests in order to reach
a deeper understanding.
%
As mentioned earlier, the code is freely available on
\href{https://github.com/DSLsofMath/DSLsofMath}{GitHub} for this
purpose.

\begin{code}
module DSLsofMath.W01 where
import Numeric.Natural (Natural)
import Data.Ratio (Ratio, (%))
\end{code}
%
These lines constitute the module header which usually starts a
Haskell file.
%
We will not go into details of the module header syntax here but the
purpose is to ``name'' the module itself (here |DSLsofMath.W01|) and
to |import| (bring into scope) definitions from other modules.
%
\index{import (Haskell)@@|import| (Haskell)}%
%
As an example, the last line imports types for rational numbers and
the infix operator |(%)| used to construct ratios
(|1%7| is Haskell notation for $\frac{1}{7}$, etc.).

\section{Types of data and functions}

Dividing up the world (or problem domain) into values of different
types is one of the guiding principles of this \course{}.
%
We will see that keeping track of types can guide the development of
theories, languages, programs and proofs.
%
We follow a Type-Driven Development style of programming.
%
\index{type-driven development}%

\subsection{What is a type?}
\index{type}%

As mentioned in the introduction, we emphasise the dividing line
between \addtoindex{\emph{syntax}} (what mathematical expressions look like)
and \addtoindex{\emph{semantics}} (what they mean).

\index{DSL!type expressions}%
%
As an example of DSL we start with \emph{type expressions} --- first in
mathematics and then in Haskell.
%
To a first approximation one can think of types as sets.
%
\index{Bool@@|Bool| (|BB|)}%
%\index{Bool@@|Bool| (|BB|)!False@@|False|}%
%\index{Bool@@|Bool| (|BB|)!True@@|True|}%
%
The type of truth values, |False| and |True|, is often called |Bool|
or just |BB|.
%
Thus the name (syntax) is |BB| and the semantics (meaning) is the
two-element set |{False, True}|.
%
\index{Nat@@|Nat| (natural numbers)}%
%
Similarly, we have the type |Nat| whose semantics is the infinite set
of natural numbers |{0, 1, 2, ...}|.
%
Other common mathematical types are |ZZ| of integers, |QQ| of
rationals, and |REAL| of real numbers.
%
\index{type judgment (|e : t| or |e :: t|)}%
%\index{:!type judgment (|e : t|)}%
%
The judgment |e : t| states that the expression |e| has type |t|.
%
For example |False : Bool|, |2 : Nat|, and |sqrt 2 : REAL|.
%
In Haskell, double colon (|::|) is used for the typing judgment, but
we often use just single colon (|:|) in the mathematical text.

So far the syntax for types is trivial --- just names.
%
Every time, the semantic is a set.
%
But we can also combine these names to form more complex types.

\paragraph{Pairs and tuple types}
%
\index{pair type}%
\index{tuple types||see {pair type}}%
%
For a pair, like |(False, 2)|, the type is written |(Bool, Nat)| in
Haskell.
%
In general, for any types |A| and |B| we write |(A, B)| for the type
of pairs.
%
In mathematics, the type (or set) of pairs is usually called the
\emph{Cartesian product} and is written using an infix cross: |A×B|.
%
\index{Cartesian product||see {pair type}}%
%
We will sometimes use this notation as well.
%
The semantics of |Bool×Bool = (Bool, Bool)| is the set |{(F, F), (F,
  T), (T, F), (T, T)}| where we shorten |False| to |F| and |True| to
|T| for readability.
%
\index{triple||see {tuple types}}%
%
We can also form expressions and types for triples, four-tuples,
etc.\ and nest them freely: |((17, True), (sqrt 2, "hi", 38))| has type
|((Nat, BB), (REAL, String, Nat))|.

\paragraph{\addtoindex{List types}}
%
If we have a collection of values of the same type we can collect them
in a list.
%
Examples include |[1,2,3]| of type |[Nat]| and |[("x", 17), ("y",
  38)]| of type |[(String, Nat)]|.
%
The semantics of the type |[Bool]| is the infinite set

\begin{spec}
  {[], [F], [T], [F,F], [F, T], [T, F], [T, T], ...}
\end{spec}  

\subsection{Functions and their types}
%
For our purposes the most important construction is the
\addtoindex{function type}.
%note: Syntax for function types
For any two type expressions |A| and |B| we can form the function type
|A -> B|.
%note: semantics for functions types
Its semantics is the set of ``functions from |A| to |B|'' (formally:
functions from the semantics of |A| to the semantics of |B|).
%
%\pj{delay / rewrite footnote}
%\footnote{The only way to be precise about semantics is to use a
%  formal language, which itself comes with its own syntax. So semantics sometimes does not seem to be that
%  much different from syntax, as in this example.
%  %
%  Fortunately, in the rest of this \course{}, we will be describing
%  domains where the semantics is gives more insight than here.}
%note: examples of function values
As an example, the semantics of |BB -> BB| is a set of four functions:
|{const False, id, not, const True}| where |not : BB -> BB| is Boolean
negation.
%
\index{not@@\ensuremath{\Varid{not}} (|not : BB -> BB|)}%
%
The function type construction is very powerful, and can be used to
model a wide range of concepts in mathematics (and the real world).
%
But to clarify the notion it is also important to note what is
\emph{not} a function.

\paragraph{Pure and impure functions}
%
\index{pure function}%
%
Many programming languages provide so called ``functions'' which are
actually not functions at all, but rather procedures: computations
depending on some hidden state or exhibiting some other effect.
%{
%format rand (x) = rand "(" x ")"
%format fApp (x) = f "(" x ")"
A typical example is |rand(N)| which returns a random number in the
range |1..N|.
%
\index{impure function}%
%
Treating such an ``impure function'' as a mathematical ``pure''
function quickly leads to confusing results.
%
For example, we know that any pure function |f| will satisfy the property: if |x == y|
then |fApp(x) == fApp(y)|.
%
As a special case we certainly want |fApp(x) == fApp(x)| for every |x|.
%
But with |rand| this does not hold: |rand(6)==rand(6)| will only be
true occasionally (if we happen to draw the same random number twice).
%
Fortunately, in mathematics and in Haskell all functions are pure.
%}

Because function types are really important, we immediately introduce
a few basic building blocks to construct functions.
%
They are as useful for functions as zero and one are for numbers.

\paragraph{Identity function}
%
\index{id@@|id| (identity function)}%
%
For each type |A| there is an \emph{identity function} |idA : A -> A|.
%
In Haskell all of these functions are defined once and for all as
follows:
%
\begin{code}
id :: a -> a
id x = x
\end{code}
%
In Haskell, a type name starting with a lowercase letter is a
\emph{type variable}.
%
When a type variable (here |a|) is used in a type signature it is
implicitly quantified (bound) as if preceded by ``for any type |a|''.
%
\index{polymorphic}%
%
This use of type variables is called ``parametric polymorphism'' and
the compiler gives more help when implementing functions with such
types.
%
We have seen one example use of the identity function already, as one
of the four functions from |Bool| to |Bool|.
%
That instance of |id| has type |Bool -> Bool|.

\paragraph{Constant functions}
%
\index{const@@|const| (constant function)}%
%
Another building block for functions is |const|.
%
Its type mentions two type variables, and it is a function of two
arguments:
%
\begin{code}
const :: a -> b -> a
const x _ = x
\end{code}
%
The underscore (|_|) is here used instead of a variable name (like
|y|) which is not needed on the right hand side (RHS) of the equality
sign.
%
Above we saw the instance |const False : Bool -> Bool| where |a| and
|b| are both |Bool|.
%
Note that this is an example of \emph{partially applied} function:
|const| by itself expects two arguments, thus |const False| still
expects one argument.

%
The term ``\addtoindex{arity}'' is used to describe how many arguments
a function has.
%
An |n|-argument function has arity |n|.
%
\index{binary (arity 2)}%
%
For small |n| special names are often used: binary means arity 2 (like
|(+)|), \addtoindex{unary} means arity 1 (like |negate|) and
\addtoindex{nullary} means arity 0, thus not a function at all, just
any regular value (like |"hi!"|).
%*TODO: perhaps add something about tupling, currying and arity --- check Idris intro

\paragraph{Higher-order functions}
%
\index{higher-order function}%
%
We can also construct functions which manipulate functions.
%
They are called \emph{higher-order} functions and as a first example
we present |flip| which flips the order the two arguments of a binary
operator.
%
\index{flip@@|flip|}%
%
\begin{code}
flip :: (a -> b -> c) -> (b -> a -> c)
flip op x y = op y x
\end{code}
%
As an example |flip (-) 4 10 == 10 - 4 == 6| and |flip const x y ==
const y x == y|.

\paragraph{Lambda expressions}
\label{sec:lambda-expression}
%
\index{lambda expression}%
\index{anonymous function}%
%
It is possible to create values of a function type without naming
them, so called ``anonymous functions''.
%
The syntax is |\x -> b|, where |b| is any expression. For example, the
identity function can be written |\x -> x|, and the constant function
could also be defined as |const = \x _ -> x|.
%
The ASCII syntax uses backslash to start the lambda expression, but
we render it as a Greek lower case lambda.

%note: perhaps note that λ is a proper character, to be used in
%      identifiers, and cannot be used instead of backslash.

\paragraph{Function composition}
%
\index{function composition (|.|)}%
%
The composition of two functions |f| and |g|, written |f . g| and
sometimes pronounced ``|f| after |g|'' can be defined as follows:
%
\begin{spec}
f . g = \x -> f (g x)
\end{spec}
%
As an exercise it is good to experiment a bit with these building
blocks to see how they fit together and what types their combinations
have.

The type of function composition is perhaps best illustrated by a diagram (see
\cref{fig:funcomp}) with types as nodes and functions (arrows) as
directed edges.
%
\begin{figure}[htbp]
\hfill
\begin{tikzcd}
  |a| \arrow{d}[swap]{|g|} \arrow{rd}[dashed]{|f.g|} &  \\
  |b| \arrow{r}{|f|}            & |c|
\end{tikzcd}
\hfill
\begin{tikzcd}
  |REAL| \arrow{d}[swap]{|round|} \arrow{rd}[dashed]{|even . round|} &  \\
  |ZZ|   \arrow{r}{|even|}            & |Bool|
\end{tikzcd}
\hfill
\begin{tikzcd}
  |ZZ| \arrow{d}[swap]{|(%1)|} \arrow{rd}[dashed]{|inv . (%1)|} &  \\
  |QQ|   \arrow{r}{|inv|}            & |Maybe QQ|
\end{tikzcd}
\caption{Function composition diagrams: in general, and two examples}
\label{fig:funcomp}
\end{figure}
In Haskell we get the following type:

\begin{spec}
(.) :: (b->c) -> (a->b) -> (a->c)
\end{spec}
%
which may take a while to get used to.

%TODO: check usage of single : and double :: colon
\index{operator section}%
%
In the figure we use ``operator sections'': |(%1) :: ZZ -> QQ| is the
function that embeds an integer |n| as the ratio |frac n 1|.
%
Other convenient examples include |(+1) :: ZZ -> ZZ| for the ``add
one'' function, and |(2*)| for the ``double'' function.

\subsection{Partial and total functions}
\label{sec:partial-and-total-functions}
%
\index{partial function}%
\index{total function}%
%
There are some differences between functions in the usual mathematical
sense, and Haskell functions.
%
Some Haskell ``functions'' are not defined for all inputs --- they are
\emph{partial} functions.
%
Simple examples include |head :: [a] -> a| which is not defined for
the empty list and |(1/) :: REAL -> REAL| which is not defined for
zero.
%
A proper mathematical function is said to be \emph{total}: it is
defined for all its inputs.
%
In Haskell totality can be compromised by omitting cases (like
|head|), by raising exceptions (like division) or by non termination
(like |inf = 1 + inf|).

There are two ways of turning a partial function into a total
function.
%
One can limit the type of the inputs (the domain) to avoid the inputs
where the function is undefined (or non-terminating, etc.), or extend
the type of the output (the range) to represent ``default'' or
``exceptional'' values explicitly.
%\footnote{or better yet, meaningful values, as we shall see later}

\index{sqrt@@square root (|sqrt|)}%
%
As an example, |sqrt|, the square root function, is partial if
considered as a function from |REAL| to |REAL|.
%
It can be made total if the domain is restricted to |RPosz|, or if the
range is extended to complex numbers.
%
In most programming languages the range is extended in another way.
%
The type is |Double -> Double| and |sqrt (-1)| returns the value |NaN
: Double| (Not a Number).
%
\index{NaN@@Not a number (|NaN|)}%
%
Similarly, |(1/) :: Double -> Double| returns |Infinity :: Double|
when given zero as an input.
%
Thus |Double| is a mix of (many, but not all) rational numbers and
some special quantities like |NaN| and |Infinity|.
%

\index{Maybe@@|Maybe| type}%
\index{option type||see {|Maybe| type}}%
\index{Maybe@@|Maybe| type!Nothing@@|Nothing|}%
\index{Maybe@@|Maybe| type!Just@@|Just|}%
%
Often the type |Maybe a| with values |Nothing| and |Just a| (for all
|x::a|) is used as the target of functions which would otherwise be
partial: any undefined input is mapped to |Nothing|.
%
The definition of |Maybe| is given in full in \cref{sec:Maybe}.

There are also mathematical functions which cannot be implemented at
all (uncomputable functions).
%
We will only briefly encounter such a case in
\cref{sec:fol-undecidability}.

\paragraph{Partial functions with finite domain}

\index{partial function}%
Later on (in \cref{sec:ArithExp}), we will use partial functions for
looking up values in an environment.
%
Here we prepare this by presenting a minimal DSL for partial functions
with a finite domain.
%
The type |Env v s| will be the \emph{syntax} for the type of partial
functions from |v| to |s|, and defined as follows:
%
\index{Env@@|Env| (environment type)}%
%
\begin{code}
type Env v s = [(v,s)]
\end{code}
%
As an example value of this type we can take:
%
\begin{code}
env1 :: Env String Int
env1 = [("x", 17), ("y", 38)]
\end{code}

The intended meaning is that |"x"| is mapped to |17|, etc.
%
The semantic domain is the set of partial functions, and, as discussed
above, we represent those as the Haskell type |v -> Maybe s|.
%
\index{Maybe@@|Maybe| type}%

Our evaluation function, |evalEnv|, maps the syntax to the semantics,
and as such has the following type:
%
\index{evalEnv@@|evalEnv|}%
%
\begin{code}
evalEnv :: Eq v =>  Env v s -> (v -> Maybe s)
\end{code}
%
This type signature deserves some more explanation.
%
\index{constraint (type)}%
\index{Eq@@|Eq| (type class)}%
%
The first part (|Eq v =>|) is a constraint which says that the
function works, not for \emph{all} types |v|, but only for those who
support a Boolean equality check (|(==) :: v -> v -> Bool|).
%
The next part of the type signature (|Env v s|) shows the type of
the first argument (|env|) to the function |evalEnv|.
%
The final part of the type, |(v -> Maybe s)|), shows that |evalEnv
env| is also a function, now taking a |v| and maybe returning an |s|.

The implementation proceeds by searching for the first occurrence of
|x| in the list of pairs |(v,s)| such that |x==v|, and return |Just s|
if one is found, and |Nothing| otherwise.
%**TODO: Explain |where| clause syntax
%**TODO: Explain Boolean guards
\begin{code}
evalEnv vss x  =  findFst vss
  where  findFst ((v,s):vss)   | x == v         =  Just s     
                               | otherwise      =  findFst vss
         findFst [] =  Nothing
\end{code}
%
Another equivalent definition is |evalEnv = flip lookup|, where
|lookup| is defined in the Haskell Prelude with the following type:
%
\index{lookup@@|lookup|}%
%
\begin{spec}
lookup :: Eq a => a -> [(a, b)] -> Maybe b
\end{spec}

% \begin{tikzcd}
%   |[Int]| \arrow[d, "|sort|"] \arrow[rd, "|head.sort|", dashed] &  \\
%   |[Int]| \arrow[r, "|head|"]            & |Int|
% \end{tikzcd}
% \hfill
% \begin{tikzcd}
%   |Env v s| \arrow[d, "|head|"] \arrow[rd, "|fst.head|", dashed] &  \\
%   |(v, s)| \arrow[r, "|fst|"]            & |v|
% \end{tikzcd}
% \hfill{}
% %\includegraphics[width=0.4\textwidth]{../E/FunComp.jpg}

%**TODO: Perhaps more about Cartesian product, etc.
%*TODO explain the e : t syntax (and mention e `elem` t)

%\subsection{Functions}

\subsection{Variable names as type hints}
%
\index{type hints}%
%
In mathematical texts there are often conventions about the names used
for variables of certain types.
%
Typical examples include |f, g| for functions, |i, j, k| for natural
numbers, |x, y| for real numbers and |z, w| for complex
numbers.

The absence of explicit types in mathematical texts can sometimes lead
to confusing formulations.
%
Here, and in many places in later chapters, we will analyse a quote
from a mathematical textbook.
%
You do \emph{not} need to understand the mathematics behind at this
point (we only get to the Laplace transform in \cref{sec:Laplace}).
% 
For example, a standard text on differential equations by
\citet*{edwards2008elementary} contains at page 266 the following
remark:

\newcommand{\Lap}[1]{\ensuremath{|Lap|\{#1\}}}
\begin{quote}
  The differentiation operator $D$ can be viewed as a transformation
  which, when applied to the function $f(t)$, yields the new function
  $D\{f(t)\} = f'(t)$.
  %
  The \addtoindex{Laplace} transformation |Lap| involves the operation
  of integration and yields the new function $\Lap{f(t)} = F(s)$ of a
  new independent variable $s$.
\end{quote}

This is meant to introduce a distinction between ``operators'', such
as differentiation, which take functions to functions of the same
type, and ``transforms'', such as the Laplace transform, which take
functions to functions of a new type.
%
To the logician or the computer scientist, the way of phrasing this
difference in the quoted text sounds strange:
%
surely the \emph{name} of the independent variable does not matter;
%
the Laplace transformation could very well return a function of the
``old'' variable |t|.
%
We can understand that the name of the variable is used to carry
semantic meaning about its type (this is also common in functional
programming, for example with the conventional use of a plural "s"
suffix, as in the name |xs|, to denote a list of values.).
%
% Moreover, by using this (implicit!)\ convention, it is easier to deal
% with cases such as that of the Hartley transform (a close relative of
% the Fourier transform), which does not change the type of the input
% function, but rather the \emph{interpretation} of that type.
% %

Rather than relying on lexical or syntactical conventions in the
variable names, we prefer to explicitly use different types.
%
When there are several interpretations of the same type, we can define
a type synonym for each interpretation.
%
In the example of the Laplace transform, this leads to
%
\begin{spec}
Lap : (T -> CC) -> (S -> CC)
\end{spec}
%
where the types |T = REAL| and |S = CC|.
%
Note that the function type constructor |(->)| is used three times
here: once in |T -> CC|, once in |S -> CC| and finally at the top
level to indicate that the transform maps functions to functions.
%
This means that |Lap| is an example of a \addtoindex{higher-order function},
and we will see many uses of this idea in this \course{}.

Now we move to introducing some of the ways types are defined in
Haskell, our language of choice for the implementation (and often also
specification) of mathematical concepts.

%TODO: perhaps use more from Expr.lhs

\section{Types in Haskell: |type|, |newtype|, and |data|}

There are three keywords in Haskell involved in naming and creating
types: |type|, |newtype|, and |data|.

\paragraph{|type| -- abbreviating type expressions}
\index{type@@|type| (keyword)}%
%
The |type| keyword is used to create a type synonym -- just another
name for a type expression.
%
\index{left-hand side (LHS)}%
%
The new name is written on the left-hand side (LHS) of an equal sign,
and the type expression on the right-hand side (RHS).
%
\index{right-hand side (RHS)}%
%
The semantics is unchanged: the set of values of type |Number| is
exactly the same as the set of values of type |Integer|, etc.

\begin{joincode}% The |Env| definition was earlier
\begin{code}
type Number   =  Integer
type Foo      =  (Maybe [String], [[Number]])
type BinOp    =  Number -> Number -> Number
\end{code}
\begin{spec}
type Env v s  =  [(v,s)]
\end{spec}
\end{joincode}

A |type| declaration does not add type safety, just readability (if
used wisely).
%
The |Env| example shows that a type synonym can have type parameters.
%
\index{Env@@|Env| (environment type)}%
%
Note that |Env v s| is a type (for any types |v| and |s|), but |Env|
on its own is not a type but a \emph{\addtoindex{type constructor}}
--- a function at the type level.
%


\paragraph{|newtype| -- more protection}
%
\index{newtype@@|newtype| (keyword)}%
%
\begin{figure}
  \centering
  \includegraphics[width=0.5\linewidth]{New_cuyama.jpg}
  \caption{Humorously inappropriate type mismatch on a sign in New Cuyama, California. \href{https://commons.wikimedia.org/w/index.php?curid=2513523}{By I, MikeGogulski, CC BY 2.5, Wikipedia.} }
  \label{fig:TypeErrorSign}
\end{figure}

A simple example of the use of |newtype| in Haskell is to distinguish
values which should be kept apart.
%
A fun example of \emph{not} keeping values apart is shown in
Figure~\ref{fig:TypeErrorSign}.
%
To avoid this class of problems Haskell provides the |newtype|
construct as a stronger version of |type|.

\begin{code}
newtype Count       = Cou    Int  -- Population count
newtype DistFeet    = DisFt  Int  -- Elevation in feet above sea level
newtype Year        = Yea    Int  -- Year of establishment

-- Example values of the new types
pop  :: Count;       pop  = Cou    562;
hei  :: DistFeet;    hei  = DisFt  2150;
est  :: Year;        est  = Yea    1951;
\end{code}

This example introduces three new types, |Count|, |DistFeet|, and
|Year|, which all are internally represented by an |Int| but which are
good to keep apart.
%
The syntax also introduces \emph{\addtoindex{constructor function}s}
|Cou :: Int -> Count|, |DisFt| and |Yea| which can be used to
translate from plain integers to the new types, and for pattern
matching.
%*TODO: pehaps add example of pattern matching as well
The semantics of |Count| is the set of values of the form |Cou i|
for every value |i :: Int|.
%
It is not the same as the semantics of |Int| but the sets are
\emph{bijective}.
%
The function |Cou| is an invertible function, a
\emph{\addtoindex{bijection}}, also called a
set-\addtoindex{isomorphism}.
%
(We talk about isomorphisms between richer algebraic structures
in \cref{sec:CompSem}.)

Later in this chapter we use a newtype for the semantics of complex
numbers as a pair of numbers in the Cartesian representation but it
may also be useful to have another newtype for complex as a pair of
numbers in the polar representation.


\paragraph{The keyword |data| for syntax trees}
%
\index{data@@|data| (keyword)}%
\index{abstract syntax tree}%
\index{recursive datatype}%
%
The simplest form of a recursive datatype is the unary notation for
natural numbers:
%
\index{Nat@@|Nat| (natural numbers)}%
%
\begin{code}
data N = Z | S N
\end{code}
%
This declaration introduces
\begin{itemize}
\item a new type |N| for unary natural numbers,
\item a constructor |Z :: N| to represent zero, and
\item a constructor |S :: N -> N| to represent the successor function.
\end{itemize}
%
\index{Nat@@|Nat| (natural numbers)!S@@|S : Nat -> Nat| (successor)}%
\index{Nat@@|Nat| (natural numbers)!Z@@|Z : Nat| (zero)}%
%
The semantics of |N| is the set of natural numbers (|Nat|), with the
semantics of |Z| being |0|, |S Z| being 1, etc.
%
A way to be complete about the semantics is to state that the
semantics of |S| is ``add one''.

Examples values: |zero = Z|, |one = S Z|, |three = S (S one)|.

The |data| keyword will be used throughout the \course{} to define
(inductive) datatypes of \addtoindex{syntax tree}s for different kinds
of expressions: simple arithmetic expressions, complex number
expressions, etc.
%
\index{Bool@@|Bool| (|BB|)}%
%
But it can also be used for non-inductive datatypes, like |data Bool =
False || True|, or |data TwoDice = TD ZZ ZZ|.
%
The |Bool| type is the simplest example of a \emph{\addtoindex{sum
  type}}, where each value uses either of the two variants |False| and
|True| as the constructor.
%
The |TwoDice| type is an example of a \emph{\addtoindex{product
  type}}, where each value uses the same constructor |TD| and records
values for the values of two rolled dice.
%
(See Exercise~\ref{exc:counting} for the intuition behind the terms
``sum'' and ``product'' used here.)

\paragraph{|Maybe| and \addtoindex{parameterised type}s}
%
It is very often possible to describe a \addtoindex{family of types}
using a \addtoindex{type parameter}.
%
One simple example is the type constructor |Maybe|, used for
encapsulation of an optional value:
%
\index{Maybe@@|Maybe| type}%
%
\label{sec:Maybe}
\begin{spec}
data Maybe a = Nothing | Just a
\end{spec}
%
This declaration introduces
\begin{itemize}
\item a new type |Maybe a| for every type |a|,
\item a constructor |Nothing :: Maybe a| to represent ``no value'', and
\index{Maybe@@|Maybe| type!Nothing@@|Nothing|}%
\item a constructor |Just :: a -> Maybe a| to represent ``just a value''.
\index{Maybe@@|Maybe| type!Just@@|Just|}%
\end{itemize}
%
A maybe type can be used when an operation may, or may not, return a
value:
%
\begin{code}
inv :: QQ -> Maybe QQ
inv 0  = Nothing
inv r  = Just (1/r)
\end{code}

%*TODO: perhaps move Cartesian product earlier (to maths / set part)
\index{pair type}%
%
Two other examples of, often used, parameterised types are |(a,b)| for
the type of pairs (a \addtoindex{product type}) and |Either a b| for
either an |a| or a |b| (a \addtoindex{sum type}).
%
For reference, the either type is defined as follows in Haskell:
%
\index{Either@@|Either| type}%
%\index{Either@@|Either| type!Left@@|Left : a -> Either a b|}%
\index{Left@@|Left : a -> Either a b|}%
%\index{Either@@|Either| type!Right@@|Right : b -> Either a b|}%
\index{Right@@|Right : b -> Either a b|}%
%include Either.lhs
%TODO at first use explain GADT syntax for |data| declaration 

\section{Notation and abstract syntax for \addtoindex{sequence}s}
\label{sec:infseq}
%TODO: perhaps add as possible reading: http://www.mathcentre.ac.uk/resources/uploaded/mc-ty-convergence-2009-1.pdf
%TODO: perhaps link to https://en.wikipedia.org/wiki/Squeeze_theorem for nice examples
As preparation for the language of sequences and limits later
(\cref{par:LimitOfSequence,sec:power-series}), we spend a few lines on
the notation and abstract syntax of sequences.

In maths textbooks, the following notation is commonly in use:
%
$\left\{ a_i \right\}_{i=0}^{\infty}$ or just $\left\{ a_i \right\}$
and (not always) an indication of the type $X$ of the $a_i$.
%
Note that the |a| at the centre of this notation actually carries all
of the information: an infinite family of values $a_i$ each of type |X|.
%
If we interpret the subscript notation $a_i$ as function application
($a(i)$) we can see that |a : Nat -> X| is a useful typing of an
infinite sequence.
%
Some examples:
%
\begin{code}
type Nat    =  Natural     -- imported from |Numeric.Natural|
type QQP    =  Ratio Nat   -- imported from |Data.Ratio|
type Seq a  =  Nat -> a

idSeq :: Seq Nat
idSeq i = i                -- |{0, 1, 2, 3, ...}|

invSeq :: Seq QQP
invSeq i = 1%(1 + i)       -- |{frac 1 1, frac 1 2, frac 1 3, frac 1 4, ...}|

pow2 :: Num r =>  Seq r
pow2 = (2^{-"{}"-})        -- |{1, 2, 4, 8, ...}|

conSeq :: a -> Seq a
conSeq c i = c             -- |{c, c, c, c, ...}|
\end{code}

What operations can be performed on sequences?
%
We have seen the first one: given a value |c| we can generate a
constant sequence with |conSeq c|.
%
\label{def:pointwise}
%
\index{pointwise}%
%
We can also add sequences componentwise (also called ``pointwise''):
%
\begin{code}
addSeq :: Num a => Seq a -> Seq a -> Seq a
addSeq f g i = f i + g i
\end{code}
%
\index{binary (arity 2)}%
%
and in general we can lift any binary operation |op :: a -> b -> c| to the
corresponding, pointwise, operation of sequences:
\begin{code}
liftSeq2 :: (a->b->c) -> Seq a -> Seq b -> Seq c
liftSeq2 op f g i = op (f i) (g i)    -- |{op (f 0) (g 0), op (f 1) (g 1), ...}|
\end{code}
Similarly we can lift unary operations, and ``nullary'' operations:
\begin{code}
liftSeq1 :: (a->b) -> Seq a -> Seq b
liftSeq1 h f i = h (f i)              -- |{h (f 0), h (f 1), h (f 2), ...}|

liftSeq0 :: a -> Seq a
liftSeq0 c i = c
\end{code}

Exercise~\ref{exc:fmap}: what does function composition do to a sequence?
  For a sequence |a| what is |a . (1+)|? What is |(1+) . a|?

Another common mathematical operator on sequences is the limit (of a
sequence).
%
\index{limit (of sequence)}%
%
We will get back to limits later (in \cref{sec:LimPoint,sec:FunLimit}),
but for now we just analyse the notation and typing.
%
This definition is slightly adapted from Wikipedia (2017-11-08):
\begin{quote}
  We call \(L\) the limit of the sequence |{xn}| if the following
  condition holds: For each real number |ε>0|, there exists
  a natural number |N| such that, for every natural number
  |n >= N|, we have |absBar (xn - L) < ε|.

  If so, we say that the sequence converges to |L| and write
  \[L = \lim_{i\to\infty} x_i\]
\end{quote}
%
There are (at least) two things to note here.
%
First, with this syntax, the $\lim_{i\to\infty} x_i$ expression form
binds |i| in the expression |xi|.
%
We could just as well say that |lim| takes a function |x :: Nat -> X|
as its only argument (this is further explained in
\cref{sec:functions-and-scoping}).
%
Second, an arbitrary sequence |x|, may or may not have a limit.
%
Thus the customary use of |L =| is a bit of an abuse of notation, because
the right hand side may not be well defined.
%
\index{lim@@|lim| (limit)}%
%
One way to capture that idea is to let |lim| return the type |Maybe
X|, with |Nothing| corresponding to divergence. Then its complete type
is |(Nat -> X) -> Maybe X| and
%
\(L = \lim_{i\to\infty} x_i\) means |Just L = lim x|
%
We will return to limits and their proofs in
\cref{par:LimitOfSequence} after we have reviewed some logic.

Here we just define one more common operation: the \addtoindex{sum of
  a sequence} (like \(\sigma = \sum_{i=0}^{\infty}
1/i!\)\footnote{Here |n! = 1*2* ... *n| is the \addtoindex{factorial}
\lnOnly{(sv: fakultet)}.}).
%
Just as not all sequences have a limit, not all have a sum either.
%
But for every sequence we can define a new sequence of
\addtoindex{partial sums}:
%
\begin{code}
sums :: Num a => Seq a -> Seq a
sums a 0 = 0
sums a i = sums a (i-1) + a i
\end{code}
The function |sums| is perhaps best illustrated by examples:
\begin{spec}
  sums (conSeq c)  == {0, c, 2*c, 3*c, ...}
  sums (idSeq)     == {0, 0, 1, 3, 6, 10, ...}
\end{spec}
The general pattern is to start at zero and accumulate the sum of
initial prefixes of the input sequence.

By combining |sums| with limits we can state formally that the sum of
an infinite sequence |a| exists and is |S| iff the limit of |sums a|
exists and is |S|.
%
We can write the above as a formula: |Just S = lim (sums a)|.
%
For our example it turns out that the sum converges and that \(\sigma
= \sum_{i=0}^{\infty} 1/i! = e\) but we will not get to that until
\cref{sec:exp}.

We will also return to (another type of) limits in
\refSec{sec:typePartialDerivative} about derivatives where we explore
variants of the classical definition
%
\[f'(x) = \lim_{h\to0} \frac{f(x+h)-f(x)}{h}\]

\index{DSL!infinite sequences}%
%
To sum up this subsection, we have defined a small Domain-Specific
Language (DSL) for infinite sequences by defining a type (|Seq a|),
some operations (|conSeq|, |addSeq|, |liftSeq1|, |sums|, \ldots) and
some evaluation functions or predicates (like |lim| and |sum|).
%*TODO: the concept of "run functions" has not yet been introduced

% ----------------------------------------------------------------
%include ComplexSem.lhs


% ----------------------------------------------------------------
%include ComplexSyn.lhs

% ----------------------------------------------------------------
%include SimpleFunExp.lhs

%if False
\section{Some helper functions (can be skipped)}
\begin{code}
type QQ     =  Ratio Integer
type REAL   =  Double
\end{code}
%endif


%include E1.lhs
