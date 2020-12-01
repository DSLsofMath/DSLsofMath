%{
%format bi = "\Varid{bi}"
\chapter{Types, Functions, and Complex numbers}
\label{sec:DSLComplex}

In this chapter we exemplify our method by applying our method to the
domain of arithmetic first, and complex numbers second, which we
assume most readers will already be familiar with.
%
However, before doing so, we introduce several central concepts in the
\course{}, as well as laying out methodological assumptions.

%
We will implement certain concepts in the functional programming
language Haskell and
%
the code for this lecture is placed in a module called
|DSLsofMath.W01| that starts here\footnote{As mentioned already in the introduction, the code is available on \href{https://github.com/DSLsofMath/DSLsofMath}{GitHub}.}:
\TODO{It would also be useful to split up these imports into those needed early (|Natural|, |Ratio|) and those (|CSem|) only needed in subsection \cref{sec:complexcase} (to be introduced later).}

\begin{code}
{-# LANGUAGE InstanceSigs #-}
module DSLsofMath.W01 where
import DSLsofMath.CSem (ComplexSem(CS), (.+.), (.*.))
import Numeric.Natural (Natural)
import Data.Ratio (Ratio, (%))
\end{code}

These lines constitute the module header which usually starts a Haskell
file.
%
We will not go into details of the module header syntax here but the
purpose is to ``name'' the module itself (here |DSLsofMath.W01|) and
to |import| (bring into scope) definitions from other modules.
%
As an example, the second to last line imports types for rational
numbers and the infix operator |(%)| used to construct ratios
(|1%7| is Haskell notation for $\frac{1}{7}$, etc.).

\section{Types of |data| and functions}

Dividing up the world (or problem domain) into values of different
types is one of the guiding principles of this \course{}.
%
\begin{figure}
  \centering
  \includegraphics[width=0.5\linewidth]{New_cuyama.jpg}
  \caption{Humorously inappropriate type mismatch on a sign in New Cuyama, California. \href{https://commons.wikimedia.org/w/index.php?curid=2513523}{By I, MikeGogulski, CC BY 2.5, Wikipedia.} }
  \label{fig:TypeErrorSign}
\end{figure}
%
We will see that keeping track of types can guide the development of
theories, languages, programs and proofs.

\subsection{What is a type?}

As mentioned in the introduction, we emphasise the dividing line
between syntax (what mathematical expressions look like) and semantics
(what they mean).

As an example we start with \emph{type expressions} --- first in
mathematics and then in Haskell.
%
To a first approximation one can think of types as sets.
%
The type of truth values, |True| and |False|, is often called |Bool|
or just |BB|.
%
Thus the name (syntax) is |BB| and the semantics (meaning) is the
two-element set |{False, True}|.
%
Similarly, we have the type |Nat| whose semantics is the infinite set
of natural numbers |{0, 1, 2, ...}|.
%
Other common types are |ZZ| of integers, |QQ| of rationals, and |REAL|
of real numbers.
%

So far the syntax is trivial --- just names for certain sets --- but
we can also combine these, and for our purposes the most important construction is the
function type.
%
\subsection{Functions}
For any two type expressions |A| and |B| we can form the function type
|A -> B|.
%
Its semantics is the set of (semantic) functions from the
  semantics of |A| to the semantics of |B|.\jp{In fact we don't write this for other types. Is this really necessary?}
\footnote{The only way to be precise about semantics
  is to use a formal language, and so semantics sometimes does not
  seem to be that much different from syntax, as in this example.
  Fortunately, in the rest of this \course{}, we will be describing domains where the semantics is gives more insight than here.}
%
As an example, the semantics of |BB -> BB| is a set of four functions:
|{const False, id, not, const True}| where |not : BB -> BB| is boolean
negation.\pedantic{Additionally in Haskell one has to deal with
  diverging computations. These induce very subtle difficulties which
  we mention later. Fortunately, in the rest of this \course{}, we can
  largely ignore those subtleties.}
%
The function type construction is very powerful, and can be used to
model a wide range of concepts in mathematics (and the real world).
%
Because function types are really important, we immediately introduce a few basic
building blocks which are as useful for functions as zero and one are
for numbers.

\paragraph{Identity function}
%
For each type |A| there is an \emph{identity function} |idA : A -> A|.
%
In Haskell all of these functions are defined once and for all as follows:
\begin{code}
id :: a -> a
id x = x
\end{code}
%
When a type variable (here |a|) is used in a type signature it is
implicitly quantified (bound) as if preceded by ``for all types |a|''.
%
This use of type variables is called ``parametric polymorphism'' and
the compiler gives more help when implementing functions with such
types.
%
Another ``function building block'' is |const| which has two type
variables and two arguments:
%
\begin{code}
const :: a -> b -> a
const x _ = x
\end{code}
%

The term ``arity'' is used to describe how many arguments a function
has.
%
An |n|-argument function has arity |n|.
%
For small |n| special names are often used: binary means arity 2 (like
|(+)|), unary means arity 1 (like |negate|) and nullary means arity 0
(like |"hi!"|).
%*TODO: perhaps add something about tupling, currying and arity

We can also construct functions which manipulate functions.
They are called \emph{higher-order} functions and as a first example we present |flip|
which ``flips'' the two arguments of a binary operator.
%
\begin{code}
flip :: (a -> b -> c) -> (b -> a -> c)
flip op x y = op y x
\end{code}
%
As an example |flip (-) 5 10 == 10 - 5| and |flip const x y ==
const y x == y|.

\paragraph{Function composition}

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

The type is perhaps best illustrated by a diagram (see
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
\TODO{Explain operator section somewhere: it is used as |(%1)| in Fig.~\ref{fig:funcomp}.}

In Haskell we get the following type:

\begin{spec}
(.) :: (b->c) -> (a->b) -> (a->c)
\end{spec}
%
which may take a while to get used to.

\paragraph{Partial and total functions}

There are some differences between ``mathematical'' functions and
Haskell functions.
%
Some Haskell ``functions'' are not defined for all inputs --- they are
\emph{partial} functions.
%
Simple examples include |head :: [a] -> a| which is not defined for
the empty list and |(1/) :: REAL -> REAL| which is not defined for
zero.
%
A proper mathematical function is called \emph{total}: it is defined
for all its inputs, that is, it terminates and returns a value.

There are basically two ways of ``fixing'' a partial function: limit
the type of the inputs (the domain) to avoid the ``bad'' inputs, or
extend the type of the output (the range) to include ``default'' or ``error''
values\footnote{or better yet, meaningful values, as we shall see later}.
%
As an example, |sqrt|, the square root function, is partial if
considered as a function from |REAL| to |REAL| but total if the domain
is restricted to |RPosz|.
%
In most programming languages the range is extended instead; |sqrt ::
Double -> Double| where |sqrt (-1)| returns the ``error value'' |NaN|
(Not a Number).
%
Similarly, |(1/) :: Double -> Double| returns |Infinity :: Double|
when given zero as an input.
%
Thus |Double| is a mix of ``normal'' numbers and ``special
quantities'' like |NaN| and |Infinity|.
%
Often the type |Maybe a| with values |Nothing| and |Just a| (for all
|x::a|) is used as the target of functions.

There are also mathematical functions which cannot be implemented at
all (uncomputable functions), but we will not deal with that in this
course.

\paragraph{Partial functions with finite domain}
\pj{Look over the paragraphs and subsections - perhaps restructure}

Later on (in \cref{sec:ArithExp}), we will use partial functions for
looking up values in an environment.
%
Here we prepare this by presenting a DSL for partial functions with a
finite domain.
%
The type |Env v s| will be the \emph{syntax} for the type of partial
functions from |v| to |s|, and defined as follows:
%
\begin{code}
type Env v s = [(v,s)]
\end{code}
%
As an example value of this type we can take:
%
\begin{code}
env1 :: Env String Int
env1 = [("hey", 17), ("you", 38)]
\end{code}

The intended meaning is that |"hey"| is mapped to |17|, etc.
%
The semantic domain is the set of partial functions, and, as discussed
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
interpreted in two ways:
%
either as the type of a one-argument function taking an |Env v s| and
returning a function,
%
or as the type of a two-argument function taking an |Env v s| and a
|v| and maybe returning an |s|.

The implementation proceeds by searching for the first occurence of
|x| in the list of pairs |(v,s)| such that |x==v|, and return |Just s|
if one is found, and |Nothing| otherwise.
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

\paragraph{Pure and impure functions}

Many programming languages provide so called ``functions'' which are
actually not functions at all, but rather procedures: computations
depending on some hidden state or exhibiting some other effect.
%{
%format rand (x) = rand "(" x ")"
%format fApp (x) = f "(" x ")"
A typical example is |rand(N)| which returns a random number in the
range |1..N|.
%
Treating such an ``impure function'' as a mathematical ``pure''
function quickly leads to confusing results.
%
For example, we know that any pure function |f| will satisfy `|x == y|
implies |fApp(x) == fApp(y)|'.
%
As a special case we certainly want |fApp(x) == fApp(x)| for every |x|.
%
But with |rand| this does not hold: |rand(6)==rand(6)| will only be
true occasionally.
%
Fortunately, in mathematics and in Haskell all functions are pure.
%}

%**TODO: Perhaps more about cartesian product, etc.
%*TODO forward pointer to exercises about cardinality~\ref{exc:counting}
%*TODO explain the e : t syntax (and mention e `elem` t)

%\subsection{Functions}

\paragraph{Variable names as type hints}

In mathematical texts there are often conventions about the names used
for variables of certain types.
%
Typical examples include |f, g| for functions, |i, j, k| for natural
numbers or integers, |x, y| for real numbers and |z, w| for complex
numbers.

The absence of explicit types in mathematical texts can sometimes lead
to confusing formulations.
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
  The Laplace transformation |Lap| involves the operation of
  integration and yields the new function $\Lap{f(t)} = F(s)$ of a new
  independent variable $s$.
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
Moreover, by using this (implicit!)\ convention, it is easier to deal
with cases such as that of the Hartley transform (a close relative of
the Fourier transform), which does not change the type of the input
function, but rather the \emph{interpretation} of that type.
%
We prefer to always give explicit typings rather than relying on
syntactical conventions, and to use type synonyms for the case in
which we have different interpretations of the same type.
%
In the example of the Laplace transformation, this leads to
%
\begin{spec}
Lap : (T -> CC) -> (S -> CC)
\end{spec}
%
where |T = REAL| and |S = CC|.
\jp{But we use the variable name ('s','t') as a type name. This is perhaps also confusing?}
%
Note that the function type constructor |(->)| is used three times
here: once in |T -> CC|, once in |S -> CC| and finally at the top
level to indicate that the transform maps functions to functions.
%
This means that |Lap| is an example of a higher-order function,
and we will see many uses of this idea in this \course{}.

Now we move to introducing some of the ways types are defined in
Haskell, our language of choice for the implementation (and often also
specification) of mathematical concepts.

%TODO: perhaps use more from Expr.lhs

\subsection{Types in Haskell: |type|, |newtype|, and |data|}

There are three keywords in Haskell involved in naming types: |type|,
|newtype|, and |data|.

\paragraph{|type| -- abbreviating type expressions}

The |type| keyword is used to create a type synonym -- just another name
for a type expression.
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

The new name for the type on the right hand side (RHS) does not add
type safety, just readability (if used wisely).
%
The |Env| example shows that a type synonym can have type parameters.
%
Note that |Env v s| is a type (for any types |v| and |s|), but |Env|
itself is not a type but a \emph{type constructor}.

\paragraph{|newtype| -- more protection}

A simple example of the use of |newtype| in Haskell is to distinguish
values which should be kept apart.
%
A fun example of \emph{not} keeping values apart is shown in
Figure~\ref{fig:TypeErrorSign}.
%
To avoid this class of problems Haskell provides the |newtype|
construct as a stronger version of |type|.

\begin{code}
newtype Population       = Pop  Int  -- Population count
newtype Ftabovesealevel  = Hei  Int  -- Elevation in feet above sea level
newtype Established      = Est  Int  -- Year of establishment

-- Example values of the new types
pop :: Population;       pop  = Pop   562;
hei :: Ftabovesealevel;  hei  = Hei  2150;
est :: Established;      est  = Est  1951;
\end{code}

This example introduces three new types, |Population|,
|Ftabovesealevel|, and |Established|, which all are internally
represented by an |Int| but which are good to keep apart.
%
The syntax also introduces \emph{constructor functions} |Pop :: Int ->
Population|, |Hei| and |Est| which can be used to translate from plain
integers to the new types, and for pattern matching.
%*TODO: pehaps add example of pattern matching as well
The semantics of |Population| is the set of values of the form |Pop i|
for every value |i :: Int|.
%
It is not the same as the semantics of |Int| but it is isomorphic
(there is a one-to-one correspondence between the sets).
%
\pedantic{Usually an isomorphism cares not only about the elements of
  the sets, but some structure involving operations defined on this
  set (otherwise the term ``bijection'' is more typical). Here we have
  not talked about these operations (yet). If we were to, we'd be able
  to establish a more useful isomorphism. (And we only introduce algebraic structures in \cref{sec:CompSem}.)}

Later in this chapter we use a newtype for the semantics of complex
numbers as a pair of numbers in the Cartesian representation but it
may also be useful to have another newtype for complex as a pair of
numbers in the polar representation.

\paragraph{The keyword |data| -- for syntax trees}

The simplest form of a recursive datatype is the unary notation for
natural numbers:

\begin{code}
data N = Z | S N
\end{code}

This declaration introduces
\begin{itemize}
\item a new type |N| for unary natural numbers,
\item a constructor |Z :: N| to represent zero, and
\item a constructor |S :: N -> N| to represent the successor.
\end{itemize}
The semantics of |N| is the infinite set |{Z, S Z, S (S Z), ...}|
which is isomorphic to |Nat|.
%
Examples values: |zero = Z|, |one = S Z|, |three = S (S one)|.


The |data| keyword will be used throughout the \course{} to define
(inductive) datatypes of syntax trees for different kinds of expressions: simple
arithmetic expressions, complex number expressions, etc.
%
But it can also be used for non-inductive datatypes, like |data Bool =
False || True|, or |data TownData = Town String Population Established|.
%
The |Bool| type is the simplest example of a \emph{sum type}, where
each value uses either of the two variants |False| and |True| as the
constructor.
%
The |TownData| type is an example of a \emph{product type}, where each
value uses the same constructor |Town| and records values for the
name, population, and year of establishment of the town modelled.
%
(See Exercise~\ref{exc:counting} for the intuition behind the terms
``sum'' and ``product'' used here.)

\paragraph{|Maybe| and parameterised types}
%
It is very often possible to describe a family of types using a type parameter.
%
One simple example is the type constructor |Maybe|:
%
\begin{spec}
data Maybe a = Nothing | Just a
\end{spec}
%
This declaration introduces
\begin{itemize}
\item a new type |Maybe a| for every type |a|,
\item a constructor |Nothing :: Maybe a| to represent ``no value'', and
\item a constructor |Just :: a -> Maybe a| to represent ``just a value''.
\end{itemize}
%
A maybe type is often used when an operation may, or may not, return a
value:
%
\begin{code}
inv :: QQ -> Maybe QQ
inv 0  = Nothing
inv r  = Just (1/r)
\end{code}

%*TODO: perhaps move cartesian product earlier (to math / set part)
Two other examples of, often used, parameterised types are |(a,b)| for
the type of pairs (a product type) and |Either a b| for either an |a|
or a |b| (a sum type).\jp{Why do we give the semantics of functions but not the semantics of other types?}
%
\begin{spec}
data Either p q = Left p | Right q
\end{spec}


\section{Notation and abstract syntax for sequences}
\label{sec:infseq}
\jp{Seems like an odd place to talk about this. Why not put it together with the limits section?}
%TODO: perhaps add as possible reading: http://www.mathcentre.ac.uk/resources/uploaded/mc-ty-convergence-2009-1.pdf
%TODO: perhaps link to https://en.wikipedia.org/wiki/Squeeze_theorem for nice examples
As preparation for the language of sequences and limits
later (\cref{par:LimitOfSequence,sec:power-series}), we spend a few lines on the notation and abstract
syntax of sequences.

In math textbooks, the following notation is commonly in use: $\left\{ a_i \right\}_{i=0}^{\infty}$ or
just $\left\{ a_i \right\}$ and (not always) an indication of the type
$X$ of the $a_i$.
%
Note that the |a| at the center of this notation actually carries all
of the information: an infinite family of values $a_i$ each of type |X|.
%
If we interpret the subscript notation $a_i$ as function application ($a(i)$) we can see that
|a : Nat -> X| is a useful typing of an infinite sequence.
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
We can also add sequences componentwise (also called ``pointwise''):
%
\begin{code}
addSeq :: Num a => Seq a -> Seq a -> Seq a
addSeq f g i = f i + g i
\end{code}
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

Another common mathematical operator on sequences is the limit.
%
We will get back to limits later (\cref{sec:LimPoint,sec:FunLimit}),
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
as its only argument.\jp{This is explained in \cref{sec:functions-and-scoping}}
%
Second, an arbitrary sequence |x|, may or may not have a limit.
%
Thus the customary use of |L =| is a bit of an abuse of notation, because
the right hand side may not be well defined.
%
One way to capture that idea is to let |lim| return the type |Maybe
X|, with |Nothing| corresponding to divergence. Then its complete type
is |(Nat -> X) -> Maybe X| and
%
\(L = \lim_{i\to\infty} x_i\) means |Just L = lim x|
%
We will return to limits and their proofs in
\cref{par:LimitOfSequence} after we have reviewed some logic.
\jp{Again, awkward back-and-forth}

Here we just define one more common operation: the sum of a sequence
(like \(\sigma = \sum_{i=0}^{\infty} 1/i!\)\footnote{Here |n! =
  1*2* ... *n| is the factorial \lnOnly{(sv: fakultet)}.}).
%
Just as not all sequences have a limit, not all have a sum either.
%
But for every sequence we can define a new sequence of partial sums:
%
\begin{code}
sums :: Num a => Seq a -> Seq a
sums = scan (+) 0
\end{code}
The function |sums| is perhaps best illustrated by examples:
\begin{spec}
  sums (conSeq c)  == {0, c, 2*c, 3*c, ...}
  sums (idSeq)     == {0, 0, 1, 3, 6, 10, ...}
\end{spec}
The general pattern is to start at zero and accumulate the sum of
initial prefixes of the input sequence (we leave the defintion of
|scan| as an exercise to the reader).

%if False
%
% The definition of |sums| uses |scan| which is a generalisation which
% ``sums'' with a user-supplied operator |(⊛)| starting from an
% arbitrary value |z| (instead of zero).\jp{|scan| never comes up again. Let's not generalise.}
% %
\begin{code}
scan :: (b->a->b) -> b -> Seq a -> Seq b
scan (⊛) z a = s
  where  s 0 = z
         s i = s (i-1)  ⊛  a i
\end{code}
%endif

By combining |sums| with limits we can state formally that the sum
of an infinite sequence |a| exists and is |S| iff the limit of |sums a| exists
and is |S|.
%
We can write the above as a formula: |Just S = lim (sums a)|. For our example it
turns out that the sum converges and that
\(\sigma = \sum_{i=0}^{\infty} 1/i! = e\) but we will not get to that
until \cref{sec:exp}.

We will also return to (another type of) limits in \refSec{sec:typePartialDerivative}
about derivatives where we explore variants of the classical
definition
%
\[f'(x) = \lim_{h\to0} \frac{f(x+h)-f(x)}{h}\]

To sum up this subsection, we have defined a small Domain-Specific
Language (DSL) for infinite sequences by defining a type (|Seq a|),
some operations (|conSeq|, |addSeq|, |liftSeq1|, |sums|, \ldots) and some
evaluation functions or predicates (like |lim| and |sum|).
%*TODO: the concept of "run functions" has not yet been introduced

% ----------------------------------------------------------------



\section[A DSL of complex numbers]{A DSL of complex numbers\footnote{This part is partly based on material by
\citet{TFPIE15_DSLsofMath_IonescuJansson} which appeared at the International
Workshop on Trends in Functional Programming in Education, 2015.}
}
\label{sec:complexcase}

We now turn to our first study of mathematics as found ``in the
wild'': we will do an analytic reading of a piece of the
introduction of complex numbers by \citet{adams2010calculus}.
%
We choose a simple domain to allow the reader to concentrate on the
essential elements of our approach without the distraction of
potentially unfamiliar mathematical concepts.
%
In fact, for this section, we temporarily pretend to forget any previous
knowledge of complex numbers, and study the textbook of Adams and
Essex as we would approach a completely new domain, even if that leads
to a somewhat exaggerated attention to detail.

Adams and Essex introduce complex numbers in Appendix A of their book.
%
The section \emph{Definition of Complex Numbers} starts with:

\begin{quote}
  We begin by defining the symbol |i|, called \textbf{the imaginary
    unit}, to have the property

<      square i = -1

  Thus, we could also call |i| the square root of |-1| and denote it
  |sqrt (-1)|.
%
  Of course, |i| is not a real number; no real number has a negative
  square.
\end{quote}

%*TODO: Perhaps use this as an example of "specification by equation" pattern which is also used in the lab.
At this stage, it is not clear what the type of |i| is meant to be, we
only know that |i| is not a real number.
%
Moreover, we do not know what operations are possible on |i|, only
that |square i| is another name for |-1| (but it is not obvious that,
say |i * i| is related in any way with |square i|, since the
operations of multiplication and squaring have only been introduced so
far for numerical types such as |Nat| or |REAL|, and not for
``symbols'').

For the moment, we introduce a type for the symbol |i|, and, since we
know nothing about other symbols, we make |i| the only member of this
type.
%
We use a capital |I| in the |data| declaration because a lowercase
constructor name would cause a syntax error in Haskell.
%
For convenience we add a synonym |i == I|.
%
\begin{code}
data ImagUnits = I

i :: ImagUnits
i = I
\end{code}
%
We can give the translation from the abstract syntax to the concrete
syntax as a function |showIU|:
%
\begin{code}
showIU ::  ImagUnits       ->  String
showIU     I               =   "i"
\end{code}


Next, in the book, we find the following definition:
%
\begin{quote}
  \textbf{Definition:} A \textbf{complex number} is an expression of
  the form

<  a + bi {-"\qquad \mathrm{or} \qquad"-} a + ib,

  where |a| and |b| are real numbers, and |i| is the imaginary unit.
\end{quote}
%
This definition clearly points to the introduction of a syntax (notice
the keyword ``form'').
%
This is underlined by the presentation of \emph{two} forms, which can
suggest that the operation of juxtaposing |i| (multiplication?) is not
commutative.

A profitable way of dealing with such concrete syntax in functional
programming is to introduce an abstract representation of it in the
form of a datatype:
%
\begin{code}
data ComplexA  =  CPlus1 REAL REAL ImagUnits  -- the form |a + bi|
               |  CPlus2 REAL ImagUnits REAL  -- the form |a + ib|
\end{code}
%
We can give the translation from the (abstract) syntax to its concrete
representation as a string of characters, as the function |showCA|:
%
\begin{code}
showCA ::  ComplexA       ->  String
showCA     (CPlus1 x y i)  =  show x ++ " + " ++ show y ++ showIU i
showCA     (CPlus2 x i y)  =  show x ++ " + " ++ showIU i ++ show y
\end{code}
%
Notice that the type |REAL| is not implemented yet (and it is not even
clear how to implement it with fidelity to mathematical convention at
this stage) but we want to focus on complex numbers so we will simply
approximate |REAL| by double precision floating point numbers for now.
%
\begin{code}
type REAL = Double
\end{code}
%
\citeauthor{adams2010calculus} continue with examples:
%
\begin{quote}
  For example, $3 + 2i$, $\frac{7}{2} - \frac{2}{3}i$,
  $i\pi = 0 + i\pi$ and $-3 = -3 + 0i$ are all complex numbers.
  %
  The last of these examples shows that every real number can be
  regarded as a complex number.
\end{quote}
%
The second example is somewhat problematic: it does not seem to be of
the form |a + bi|.
%
Given that the last two examples seem to introduce shorthand for
various complex numbers, let us assume that this one does as well, and
that |a - bi| can be understood as an abbreviation of |a + (-b)i|.
%
With this provision, in our notation the examples are written as in
\cref{tab:CompleSyntaxExamplesMathHaskell}.
%
\begin{table}[tbph]
  \centering
\begin{tabular}{lll}
    \multicolumn{2}{@@{}l@@{}}{Mathematics} & Haskell
\\\hline
    $3 +2i$                        &  & |CPlus1 3 2 I|
\\ $\frac{7}{2} - \frac{2}{3} i$ &=
   $\frac{7}{2} + \frac{-2}{3} i$     & |CPlus1 (7/2) (-2/3) I|
\\ $i \pi$ &= $0 + i \pi$             & |CPlus2 0 I pi|
\\ $-3$ &= $-3 + 0 i$                 & |CPlus1 (-3) 0 I|
\end{tabular}
  \caption{Examples of notation and abstract syntax for some complex numbers.}
  \label{tab:CompleSyntaxExamplesMathHaskell}
\end{table}
%
%if False
% This is just for testing.
\begin{code}
testC1 :: [ComplexA]
testC1 =  [  CPlus1 3 2 I  ,    CPlus1 (7/2) (-2/3) I
          ,  CPlus2 0 I pi ,    CPlus1 (-3) 0 I
          ]
testS1 = map showCA testC1
\end{code}
%endif
%
We interpret the sentence ``The last of these examples \ldots'' to
mean that there is an embedding of the real numbers in |ComplexA|,
which we introduce explicitly:
%
\begin{code}
toComplex :: REAL -> ComplexA
toComplex x = CPlus1 x 0 I
\end{code}
%
Again, at this stage there are many open questions.
%
For example, we can assume that the mathematical expression $i 1$
stands for the complex number |CPlus2 0 I 1|, but what about the
expression $i$ by itself?
%
If juxtaposition is meant to denote some sort of multiplication, then
perhaps $1$ can be considered as a unit, in which case we would have
that $i$ abbreviates $i 1$ and therefore |CPlus2 0 I 1|.
%
But what about, say, $2 i$?
%
Abbreviations with |i| have only been introduced for the |ib| form,
and not for the |bi| one!

The text then continues with a parenthetical remark which helps us
dispel these doubts:
%
\begin{quote}
  (We will normally use |a + bi| unless |b| is a complicated
  expression, in which case we will write |a + ib| instead.
%
  Either form is acceptable.)
\end{quote}
%
This remark suggests strongly that the two syntactic forms are meant
to denote the same elements, since otherwise it would be strange to
say ``either form is acceptable''.
%
After all, they are acceptable according to the definition provided earlier.

Given that |a + ib| is only ``syntactic sugar'' for |a + bi|, we can
simplify our representation for the abstract syntax, eliminating one
of the constructors:
%
\begin{code}
data ComplexB = CPlusB REAL REAL ImagUnits
\end{code}
%
In fact, since it doesn't look as though the type |ImagUnits| will
receive more elements, we can dispense with it altogether:
%
\begin{code}
data ComplexC = CPlusC REAL REAL
\end{code}
%
(The renaming of the constructor to |CPlusC| serves as a guard against
the case that we have suppressed potentially semantically relevant syntax.)

We read further:
%
\begin{quote}
  It is often convenient to represent a complex number by a single
  letter;
%
  |w| and |z| are frequently used for this purpose.
%
  If |a|, |b|, |x|, and |y| are real numbers, and |w = a + bi| and |z
  = x + yi|, then we can refer to the complex numbers |w| and |z|.
%
  Note that |w = z| if and only if |a = x| and |b = y|.
\end{quote}
%
First, let us notice that we are given an important semantic
information:
%
to check equality for complex numbers, it is enough to check equality
of the components (the arguments to the constructor |CPlusC|).
%
Another way of saying this is that |CPlusC| is injective.
%
In Haskell we could define this equality as:
%
\begin{code}
instance Eq ComplexC where
    CPlusC a b == CPlusC x y = a == x && b == y
\end{code}
%
The line |instance Eq ComplexC| is there because the specify that
|ComplexC| supports the |(==)| operator.  (The cognoscenti would
prefer to obtain an equivalent definition using the shorter |deriving
Eq| clause upon defining the type.)
%*TODO: explain more about deriving - perhaps in a \footnote{}

This shows that the set of complex numbers is, in fact, isomorphic\jp{have we ever defined isomorphism?}
with the set of pairs of real numbers, a point which we can make
explicit by re-formulating the definition in terms of a |newtype|:
%
\begin{code}
newtype ComplexD = CD (REAL, REAL)   deriving Eq
\end{code}
%
As we see it, the somewhat confusing discussion of using ``letters''
to stand for complex numbers serves several purposes.
%
First, it hints at the implicit typing rule that the symbols |z| and
|w| should be complex numbers.
%
Second, it shows that, in mathematical arguments, one needs not
abstract over two real variables: one can instead abstract over a
single complex variable.
%
We already know that we have an isomorphism between pair of reals and
complex numbers.
%
But additionally, we have a notion of \emph{pattern matching}, as in
the following definition:
%
\begin{quote}
  \textbf{Definition:} If |z = x + yi| is a complex number (where |x|
  and |y| are real), we call |x| the \textbf{real part} of |z| and
  denote it |Re (z)|.
%
  We call |y| the \textbf{imaginary part} of |z| and denote it |Im
  (z)|:

< Re(z)  =  Re (x + yi)  =  x
< Im(z)  =  Im (x + yi)  =  y

\end{quote}
%
This is rather similar to Haskell's \emph{as-patterns}:
%
\begin{code}
re :: ComplexD        ->  REAL
re z @ (CD (x , y))   =   x

im :: ComplexD        ->  REAL
im z @ (CD (x , y))   =   y
\end{code}
%
a potential source of confusion being that the symbol |z| introduced
by the as-pattern is not actually used on the right-hand side of the
equations (although it could be).

The use of as-patterns such as ``|z = x + yi|'' is repeated throughout
the text, for example in the definition of the algebraic operations on
complex numbers:
%
\begin{quote}
  \textbf{The sum and difference of complex numbers}

  If |w = a + bi| and |z = x + yi|, where |a|, |b|, |x|, and |y| are real numbers,
  then

< w  +  z  =  (a + x)  +  (b + y)i
<
< w  -  z  =  (a - x)  +  (b - y)i

\end{quote}
%
With the introduction of algebraic operations, the language of complex
numbers becomes much richer.
%
We can describe these operations in a \emph{shallow embedding} in
terms of the concrete datatype |ComplexD|, for example:
%
\begin{code}
plusD :: ComplexD -> ComplexD -> ComplexD
plusD (CD (a , b)) (CD (x , y))  =  CD ((a + x) , (b + y))
\end{code}
%
\noindent
or we can build a datatype of ``syntactic'' complex numbers from the
algebraic operations to arrive at a \emph{deep embedding} as seen in
the next section.
%
Both shallow and deep embeddings will be further explained in
\cref{sec:evalD,sec:expressions-of-one-var}.\footnote{And several other places: this is a recurrent idea of the \course{}}

At this point we can sum up the ``evolution'' of the datatypes introduced so far.
%
Starting from |ComplexA|, the type has evolved by successive
refinements through |ComplexB|, |ComplexC|, ending up in |ComplexD|
(see Fig.~\ref{fig:ComplexTypeSummary}).
%
We can also make a parameterised version of |ComplexD|, by noting
that the definitions for complex number operations work fine for a
range of underlying numeric types.
%
The operations for |ComplexSem| are defined in module |CSem|,
available in Appendix~\ref{app:CSem}.
%
\begin{figure}[tbph]
\begin{spec}
data     ImagUnits     =  I
data     ComplexA      =  CPlus1  REAL   REAL ImagUnits
                       |  CPlus2  REAL   ImagUnits REAL
data     ComplexB      =  CPlusB  REAL   REAL ImagUnits
data     ComplexC      =  CPlusC  REAL   REAL
newtype  ComplexD      =  CD  (REAL, REAL)   deriving Eq
newtype  ComplexSem r  =  CS  (r , r)        deriving Eq
\end{spec}
%*TODO: explain deriving
  \caption{Complex number datatype refinement (semantics).}
  \label{fig:ComplexTypeSummary}
\end{figure}

% ----------------------------------------------------------------

\subsection{A syntax for (complex) arithmetical expressions}
\label{sec:complex-arithmetic}

By following \citet{adams2010calculus}, we have arrived at representation which captures the
\emph{semantics} of complex numbers.
%
This kind of representation is often called a ``shallow embedding''.
%
Now we turn to the study of the \emph{syntax} instead (``deep embedding'').

We want a datatype |ComplexE| for the abstract syntax tree of
expressions.
%
The syntactic expressions can later be evaluated to semantic values:
%
%
The concept of ``an evaluator'', a function from the syntax to the
semantics, is something we will return to many times in this \course{}.
%
\begin{code}
evalE :: ComplexE -> ComplexD
\end{code}
%
The datatype |ComplexE| should collect ways of building syntactic
expressions representing complex numbers and we have so far seen
%
the symbol |i|, an embedding from |REAL|, plus and times.
%
We make these four \emph{constructors} in one recursive datatype as
follows:
%
%**TODO rename |ImagUnit| to just |I| (and adapt explanation)
\begin{code}
data ComplexE  =  ImagUnit  -- syntax for |i|, not to be confused with the type |ImagUnits|
               |  ToComplex REAL
               |  Plus   ComplexE  ComplexE
               |  Times  ComplexE  ComplexE
 deriving (Eq, Show)
\end{code}
%
Note that, in |ComplexA| above, we also had a constructor for
``plus'' (|CPlus1|), but it was playing a different role.
%
They are distinguished by type: |CPlus1| took two real
numbers as arguments, while |Plus| here takes two
complex expressions as arguments.

Here are two examples of type |ComplexE| as Haskell code and as
abstract syntax trees:
\begin{code}
testE1 = Times ImagUnit ImagUnit
testE2 = Plus (ToComplex 3) (Times (ToComplex 2) ImagUnit)
\end{code}

\hspace{2em}
\begin{tikzpicture}[level 1/.style={sibling distance=3cm},baseline]
\node{|Times|}
child {node {|ImagUnit|}}
child {node {|ImagUnit|}};
\end{tikzpicture}
\hspace{2em}
\begin{tikzpicture}[level 1/.style={sibling distance=3cm},baseline]
\node{|Plus|}
child {node {|ToComplex|} child {node {|3|}}}
child {node {|Times|}
  child {node {|ToComplex|} child {node {|2|}}}
  child {node {|ImagUnit|}}};
\end{tikzpicture}

We can implement the evaluator |evalE| by pattern matching on the
constructors of the syntax tree and by recursion.
%
To write a recursive function requires a small leap of faith.
%
It can be difficult to get started implementing a function (like
|evalE|) that should handle all the cases and all the levels of a
recursive datatype (like |ComplexE|).
%
One way to overcome this difficulty is through what may seem at first glance ``wishful thinking'':
assume that all but one case have been implemented already.
%
All you need is to focus on that one remaining case, and you can
freely call the function (that you are implementing) recursively, as
long as you do it for subexpressions (subtrees of the abstract syntax
tree datatype). This pattern is called \emph{structural induction}.
%

For example, when implementing the |evalE (Plus c1 c2)| case, you can
assume that you already know the values |s1, s2 :: ComplexD|
corresponding to the subtrees |c1| and |c2| of type |ComplexE|.
%
The only thing left is to add them up componentwise and we can assume
there is a function |plusD :: ComplexD -> ComplexD -> ComplexD| taking
care of this step (in fact, we implemented it earlier in
\refSec{sec:complexcase}).
%
Continuing in this direction (by structural induction or ``wishful
thinking'') we arrive at the following implementation.
%
\begin{code}
evalE ImagUnit         = imagUnitD
evalE (ToComplex r)    = toComplexD r
evalE (Plus  c1 c2)    = plusD   (evalE c1)  (evalE c2)
evalE (Times c1 c2)    = timesD  (evalE c1)  (evalE c2)
\end{code}
%
Note the pattern here: for each constructor of the syntax datatype we
assume that there exists a corresponding semantic function.
%
The next step is to implement these functions, but let us first list
their types and compare them with the types of the syntactic constructors:
%
\begin{code}
imagUnitD :: ComplexD                        -- |ComplexE|
toComplexD :: REAL -> ComplexD               -- |REAL -> ComplexE|
timesD  :: ComplexD -> ComplexD -> ComplexD  -- |ComplexE -> ComplexE -> ComplexE|
\end{code}
%plusD   :: ComplexD -> ComplexD -> ComplexD  -- |ComplexE -> ComplexE -> ComplexE|
As we can see, each use of |ComplexE| has been replaced be a use of |ComplexD|.
%
Finally, we can start filling in the implementations:
%
\begin{code}
imagUnitD     = CD (0 ,  1)
toComplexD r  = CD (r ,  0)
\end{code}
%
The function |plusD| was defined earlier and |timesD| is left as an
exercise for the reader.
%
To sum up we have now implemented a recursive datatype for
mathematical expressions describing complex numbers, and an evaluator
that computes the underlying number.
%
Note that many different syntactic expressions will evaluate to the
same number (|evalE| is not injective).

Generalising from the example of |testE2| we also define a function to
embed a semantic complex number in the syntax:
%
\begin{code}
fromCD :: ComplexD -> ComplexE
fromCD (CD (x , y)) = Plus (ToComplex x) (Times (ToComplex y) ImagUnit)
\end{code}
%
This function is injective: different complex numbers map to different syntactic expressions.

\section{Laws, properties and testing}
There are certain laws that we would like to hold for operations on complex
numbers.
%
To specify these laws, in a way which can be easily testable in
Haskell, we use functions to |Bool| (also called \emph{predicates} or
\emph{properties}).
%
The intended meaning of such a boolean function (representing a law)
is ``forall inputs, this should return |True|''.
%
This idea is at the core of \emph{property based testing} (pioneered
by \citet{claessen_quickcheck_2000}) and conveniently available in the
library QuickCheck.
%

%
The simplest law is perhaps |square i = -1| from the start of
\refSec{sec:complexcase},
%
\begin{code}
propImagUnit :: Bool
propImagUnit =  Times ImagUnit ImagUnit === ToComplex (-1)
\end{code}
%
Note the we use a new operator here, |(===)|.
%
Indeed, we reserve the usual equality |(==)| for syntactic equality
(and here the left hand side (LHS) is clearly not syntactically equal
to the right hand side).
%
The new operator |(===)| corresponds to semantic equality, that is,
equality \emph{after evaluation}:
%

%if false
Unfortunately we have not explained classes yet.
\begin{code}
infix 0 ===
class SemEq a where
  (===) :: a -> a -> Bool
instance SemEq Int where
  (===) = (==)
instance SemEq Double where
  (===) = (==)
instance SemEq ComplexE where
\end{code}
%endif
\begin{code}
  (===) :: ComplexE -> ComplexE -> Bool
  z === w {-"\quad"-} = {-"\quad"-} evalE z == evalE w
\end{code}

Another law is that |fromCD| is an embedding: if we start from a
semantic value, embed it back into syntax, and evaluate that syntax we
get back to the value we started from.
%
\begin{code}
propFromCD :: ComplexD -> Bool
propFromCD s =  evalE (fromCD s) == s
\end{code}

Other desirable laws are that |+| and |*| should be associative and commutative and |*| should distribute over |+|:
%if false
\begin{code}
propAssocPlus :: (Num a, SemEq a) => a -> a -> a -> Bool
\end{code}
%endif
\begin{code}
propCommPlus   x y                   = {-"\quad"-}  x + y          ===  y + x
propCommTimes  x y                   = {-"\quad"-}  x * y          ===  y * x
propAssocPlus  x y z                 = {-"\quad"-}  (x + y) + z    ===  x + (y + z)
propAssocTimes x y z                 =              (x * y) * z    ===  x * (y * z)
propDistTimesPlus x y z {-"\quad"-}  =              x * (y + z)    ===  (x * y) + (x * z)
\end{code}

These laws actually fail, but not due to any mistake in the
implementation of |evalE| in itself. To see this, let us consider
associativity at different types:

\begin{code}
propAssocInt     = propAssocPlus ::  Int     -> Int     -> Int     -> Bool
propAssocDouble  = propAssocPlus ::  Double  -> Double  -> Double  -> Bool
\end{code}
%
The first property is fine, but the second fails. Why?
%
QuickCheck can be used to find small examples --- this one is perhaps the best one:
%
\begin{code}
notAssocEvidence :: (Double , Double , Double , Bool)
notAssocEvidence = (lhs , rhs , lhs-rhs , lhs==rhs)
  where  lhs = (1+1)+1/3
         rhs =  1+(1+1/3)
\end{code}
%
For completeness: these are the values:
%
\begin{spec}
  (  2.3333333333333335     -- Notice the five at the end
  ,  2.333333333333333,     -- which is not present here.
  ,  4.440892098500626e-16  -- The (very small) difference
  ,  False)
\end{spec}
%
We can now see the underlying reason why some of the laws failed for
complex numbers: the approximative nature of |Double|.
%
Therefore, to ascertain that there is no other bug hiding, we need to move away from the
implementation of |REAL| as |Double|.
We do this by abstraction: we make one more
version of the complex number type, which is parameterised on the underlying
representation type for~|REAL|.
%
At the same time\jp{why though?} we combine |ImagUnit| and |ToComplex| to
|ToComplexCart|, which corresponds to the primitive from |a + bi| discussed above:
%*TODO: perhaps explain more about the generalisation step.

%TODO: Add as an exercise the version with I | ToComplex | Plus ... | Times ...
% See data blackboard/W1/20170116_114608.jpg, eval blackboard/W1/20170116_114613.jpg
\label{sec:toComplexSyn}
\begin{code}
data ComplexSyn r  =  ToComplexCart r r
                   |  ComplexSyn r  :+:  ComplexSyn r
                   |  ComplexSyn r  :*:  ComplexSyn r

toComplexSyn :: Num a => a -> ComplexSyn a
toComplexSyn x = ToComplexCart x 0
\end{code}
%
From Appendix~\ref{app:CSem} we import |newtype ComplexSem r = CS (r ,
r) deriving Eq| and the semantic operations |(.+.)| and |(.*.)|
corresponding to |plusD| and |timesD|.
%
\begin{code}
evalCSyn :: Num r => ComplexSyn r -> ComplexSem r
evalCSyn (ToComplexCart x y)  = CS (x , y)
evalCSyn (l  :+:  r)          = evalCSyn l  .+.  evalCSyn r
evalCSyn (l  :*:  r)          = evalCSyn l  .*.  evalCSyn r
\end{code}
%
%if False
\label{sec:firstFromInteger}
\begin{code}
instance Num a => Num (ComplexSyn a) where
   (+)  = (:+:)
   (*)  = (:*:)
   fromInteger = fromIntegerCS

fromIntegerCS :: Num r =>  Integer -> ComplexSyn r
fromIntegerCS = toComplexSyn . fromInteger
\end{code}
%endif

\begin{exercise}
  Add a few more operations (hint: extend |ComplexSyn| as well) and extend |eval| appropriately.
\end{exercise}

With this parameterised type we can test the code for ``complex
rationals''\footnote{The reason why math textbooks never talk about
  this version of complex numbers is because complex numbers are used
  to handle roots of all numbers uniformly, and roots are in general
  irrational.} to avoid rounding errors.
%**TODO: add concrete example

%TODO: perhaps include
% We can also state and check properties relating the semantic and the syntactic operations:
%
% |a + b = eval (Plus (embed a) (embed b))| for all |a| and |b|.
\subsection{Generalising laws}
\label{sec:generalising-laws}
Some laws appear over and over again in different mathematical
contexts.
%
For example, binary operators are often associative or commutative, and
sometimes one operator distributes over another.
%
We will work more formally with logic in \cref{sec:logic} but
we introduce a few definitions already here:

|Associative (⊛) = Forall (a, b, c) ((a⊛b)⊛c = a⊛(b⊛c))|

|Commutative (⊛) = Forall (a, b) (a⊛b = b⊛a)|

|Distributive (⊗) (⊕) = Forall (a, b, c) ((a⊕b)⊗c = (a⊗c)⊕(b⊗c))|

The above laws are \emph{parameterised} over some operators
(|(⊛),(⊗),(⊕)|).  These laws will hold for some operators, but not for
others.  For example, division is not commutative; taking the average
of two quantities is commutative but not associative.\footnote{See
  also \crefatpage{distributivity-as-homomorphism} for further
  analysis of distributivity.}

Such generalisation can be reflected in QuickCheck properties as well.

\begin{code}
propAssoc :: SemEq a => (a -> a -> a) -> a -> a -> a -> Bool
propAssoc (⊛) x y z =  (x ⊛ y) ⊛ z === x ⊛ (y ⊛ z)
\end{code}
%
Note that |propAssocA| is a higher order function: it takes a function |(⊛)|
(written as a binary operator) as its first parameter, and tests if it is associative.
%
The property is also polymorphic: it works for many different types |a| (all
types which have an |===| operator).

Thus we can specialise it to |Plus|, |Times| and any other binary
operator, and obtain some of the earlier laws (|propAssocPlus|, |propAssocTimes|).
The same can be done with distributivity.
Doing so we learnt that the underlying set
matters: |(+)| for |REAL| has some properties, but |(+)| for |Double|
has others.
%
When formalising math as DSLs, approximation is sometimes convenient, but makes many
laws false.
%
Thus, we should attempt to do it late, and if possible, leave a
parameter to make the degree of approximation tunable (|Int|,
|Integer|, |Float|, |Double|, |QQ|, syntax trees, etc.).


%**TODO: hide or give hints / method (otherwise too hard and a bit off topic)
%Exercise: Find some operator |(#)| which satisfies |Distributive (+) (#)|
% Answer: |max|

\begin{exercise}
Find other pairs of operators satisfying a distributive law.
\end{exercise}


%if False
\section{Some helper functions (can be skipped)}
\begin{code}
type QQ     =  Ratio Integer

propAssocAdd :: (SemEq a, Num a) => a -> a -> a -> Bool
propAssocAdd = propAssoc (+)

-- timesD :: ComplexD -> ComplexD -> ComplexD
timesD (CD (ar, ai)) (CD (br, bi)) = CD (ar*br - ai*bi, ar*bi + ai*br)

instance Show ComplexD where
  show = showCD

showCD :: ComplexD -> String
showCD (CD (x, y)) = show x ++ " + " ++ show y ++ "i"
\end{code}
%endif

% end of formatting "bi" as just that (and not as \(b_i\)).
%}

%include E1.lhs
