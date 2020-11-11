\section{An aside: Pure set theory}
\label{sec:PureSet}
\pj{This subsection depends on terminology from later in the chapter. Needs to be moved of rephrased.}
One way to build mathematics from the ground up is to start from pure
set theory and define all concepts by translation to sets.
%
We will only work with (a small corner of) this as a mathematical domain to study, not as
``the right way'' of doing mathematics (there are other ways).
%
The core of the language of pure set theory is captured by four
function symbols (|{}|, |S|, |Union|, and |Intersection|).
%
We use a nullary function symbol |{}| for the empty set (sometimes
written $\emptyset$) and a unary function symbol |S| for the function
that builds a singleton set from an ``element''.
%
All non-variable terms so far are |{}|, |S {}|, |S (S {})|, \ldots
%
The first set is empty but all the others are (different) one-element sets.

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
cardinality\footnote{The \emph{cardinality} of a set is the number of
  elements in it.} 0, 1, 2, and 3.
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
build sets of cardinality 2: |s3 = Union s1 s2|, |s4 = Union s2 s3|, etc..
%
And we can at any point apply |S| to get back a new set of cardinality
1, like |s5 = S s3|.

\paragraph{Natural numbers}
%
To talk about things like natural numbers in pure set theory they need
to be encoded.
%
FOL\pj{Fix these references to concepts introduced later.} does not have function definitions or recursion, but in a suitable
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
