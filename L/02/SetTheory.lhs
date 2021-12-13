\section{An aside: Pure set theory}
\label{sec:PureSet}
%TODO: assignment is included - perhaps move this
%
One way to build mathematics from the ground up is to start from pure
set theory and define all concepts by translation to sets.
%
We will only work with (a small corner of) this as a mathematical
domain to study, not as ``the right way'' of doing mathematics (there
are other ways).
%
To classify the sets we will often talk about the \emph{cardinality}
of a set which is defined as the number of elements in it.

The core of the language of pure set theory is captured by four
function symbols (|{}|, |S|, |Union|, and |Intersection|).
%\begin{code}
%data Set = EmptySet | S | Union | Intersection
%\end{code}
%
We use a nullary function symbol |{}| to denote the empty set
(sometimes written $\emptyset$) and a unary function symbol |S| for
the function that builds a singleton set from an ``element''.
%
All non-variable terms so far are |{}|, |S {}|, |S (S {})|, \ldots
%
The first set is empty but all the others denote (different)
one-element sets.

Next we add two binary function symbols for union and intersection of
sets (denoted by terms).
%
Using union we can build sets of more than one element, for example
|Union (S {}) (S (S {}))| which has two ``elements'': |{}| and |S {}|.
%

In pure set theory we don't actually have any distinguished
``elements'' to start from (other than sets), but it turns out that
quite a large part of mathematics can still be expressed.
%
Every term in pure set theory denotes a set, and the elements of each
set are again sets.
%
\lnOnly{(Yes, this can make your head spin.)}

At this point it is a good exercise to enumerate a few sets of
cardinality 0, 1, 2, and 3.
%
There is really just one set of cardinality 0: the empty set |s0 =
{}|.
%
Using |S| we can then construct |s1 = S s0| of cardinality 1.
%
Continuing in this manner we can build |s2 = S s1|, also of
cardinality 1, and so on.
%
Now we can combine different sets (like |s1| and |s2|) with |Union| to
build sets of cardinality 2: |s3 = Union s1 s2|, |s4 = Union s2 s3|, etc.
%
And we can at any point apply |S| to get back a new set of cardinality
1, like |s5 = S s3|.

\paragraph{Natural numbers}
%
To talk about things like natural numbers in pure set theory they need
to be encoded.
%
FOL does not have function definitions or recursion, but in a suitable
meta-language (like Haskell) we can write a function that creates a
set with |n| elements (for any natural number |n|) as a term in FOL.
%
Here is some pseudo-code defining the ``von Neumann'' encoding:
%
\begin{spec}
  vN 0      = {}
  vN (n+1)  = step (vN n)

  step x = Union x (S x)
\end{spec}
%
If we use conventional set notation we get |vN 0 = {}|, |vN 1 = {{}}|,
|vN 2 = {{}, {{}}}|, |vN 3 = {{}, {{}}, {{}, {{}}}}|, etc.
%format over x = "\overline{" x "}"
If we use the shorthand |over n| for |vN n| we see that |over 0 = {}|,
|over 1 = {over 0}|, |over 2 = {over 0, over 1}|, |over 3 = {over 0,
  over 1, over 2}| and, in general, that |over n| has cardinality |n|
(meaning it has |n| elements).
%
\lnOnly{The function |vN| is explored in more detail in the first assignment
of the DSLsofMath course.}


\paragraph{Pairs}
%
The constructions presented so far show that, even starting from no
elements, we can embed all natural numbers in pure set theory.
%
We can also embed unordered pairs: |{a, b} =~= Union (S a) (S b)|
and normal, ordered pairs: |(a, b) =~= {S a, {a, b}}|.
%
% |{S a, {a, b}} = Union (S (S a)) (S (Union (S a) (S b)))|
With a bit more machinery it is possible to step by step encode |Nat|,
|ZZ|, |QQ|, |REAL|, and |COMPLEX|.
%
A good read in this direction is ``The Haskell Road to Logic, Maths
and Programming'' \citep{doets-haskellroadto-2004}.

%*TODO: Perhaps add a bit about laws for pure set theory: x /= S x, Commutative(Union), etc.

\subsection{Assignment 1: DSLs, sets and von Neumann}
\label{dsls-sets-and-von-neumann}

In this assignment you will build up a domain-specific language (a
DSL) for finite sets.
%
The domain you should model is pure set theory where all members are
sets.

\index{abstract syntax tree}
%
Define a datatype |TERM v| for the abstract syntax of set expressions
with variables of type |v| and a datatype |PRED v| for predicates over
pure set expressions.

\paragraph{Part 1.} |TERM| should have constructors for

\begin{itemize}

\item the |Empty| set
\item the one-element set constructor |Singleton|
\item |Union|, and |Intersection|
  \begin{itemize}
  \item you can also try |Powerset|
  \end{itemize}
\item set-valued variables (|Var :: v -> TERM v|)
\end{itemize}

|PRED| should have contructors for
\begin{itemize}
\item the two predicates |Elem|, |Subset|
\item the logical connectives |And|, |Or|, |Implies|, |Not|
\end{itemize}

\paragraph{Part 2.} A possible semantic domain for pure sets is

\begin{code}
newtype Set = S [Set]
\end{code}

Implement the evaluation functions
\begin{code}
eval   :: Eq v => Env v Set ->  TERM v   -> Set
check  :: Eq v => Env v Set ->  PRED v  -> Bool
\end{code}

\begin{code}
type Env var dom = [(var , dom)]
\end{code}

Note that the type parameter |v| to |TERM| is for the type of
variables in the set expressions, not the type of elements of the
sets.
%
(You can think of pure set theory as ``untyped'' or ``unityped''.)

\paragraph{Part 3.} The \emph{von Neumann encoding} of natural numbers as
sets is defined recursively as

\begin{spec}
  vonNeumann 0        =  Empty
  vonNeumann (n + 1)  =  Union  (vonNeumann n)
                                (Singleton (vonNeumann n))
\end{spec}

Implement |vonNeumann| and explore, explain and implement the following
``pseudocode'' claims as functions in Haskell:
%
\begin{spec}
  claim1 n1 n2  =  {- if |(n1 <= n2)|  then  |(n1 ⊆ n2)| -}
  claim2 n      =  {- |n = {0, 1, ..., n - 1}| -}
\end{spec}

You need to insert some embeddings and types and you should use the
|eval| and |check| functions.
%
(For debugging it is useful to implement a |show| function for |Set|
which uses numerals to show the von Neumann naturals.)

