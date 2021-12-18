
\section{Basic concepts of calculus}

Now we have built up quite a bit of machinery to express logic
formulas and proofs.
%
It is time time to apply it to some concepts in calculus.
%
We start with the concept of ``limit point'' which is used in the
formulation of different properties of limits of functions.

% TODO: Perhaps start with the ``expression'' $lim_{x\to x_0} f(x)$ and
% explain that not all |x_0| make sense, etc. [For context and
% motivation.]
%
% TODO: Or talk a bit about open and closed sets. (Open set = every
% point is internal = there is some wiggle-room around each point in the
% set. Closed set contains all its limit points.)

\paragraph{Limit point}\label{sec:LimPoint}

\emph{Definition} (adapted from \cite{rudin1964principles}, page 28):
Let |X| be a subset of |ℝ|.
%
A point |p ∈ ℝ| is a limit point of |X| iff for every |ε > 0|, there
exists |q ∈ X| such that |q ≠ p| and |absBar(q - p) < ε|.
%

To express ``Let |X| be a subset of |ℝ|'' we write |X : 𝒫 ℝ|.
%
In general, the function |𝒫| takes a set (here |REAL|) to the set of
all its subsets.
%
\begin{spec}
Limp : ℝ → 𝒫 ℝ → Prop
Limp p X = ∀ ε > 0? ∃ q ∈ X - {p}? absBar (q-p) < ε
\end{spec}
%
Notice that |q| depends on |ε|.
%
Thus by introducing a function |getq| we can move the |∃| out.

\begin{spec}
type Q = {-"ℝ_{> 0}"-} → (X - {p})
Limp p X = ∃ getq : Q? ∀ ε > 0? absBar (getq ε - p) < ε
\end{spec}

Next: introduce the ``open ball'' function |B|.
%
\begin{spec}
B : ℝ → {-"ℝ_{> 0}"-} → 𝒫 ℝ
B c r = {x | absBar (x - c) < r}
\end{spec}
%
|B c r| is often called an ``open ball'' around |c| of radius |r|.
%
On the real line this ``open ball'' is just an open interval, but with
complex |c| or in more dimensions the term feels more natural.
%
In every case |B c r| is an open set of values (points) of distance
less than |r| from |c|.
%
The open balls around |c| are special cases of \emph{neighbourhoods of
  |c|} which can have other shapes but must contain some open ball.

Using |B| we get
\begin{spec}
Limp p X = ∃ getq : Q? ∀ ε > 0? getq ε ∈ B p ε
\end{spec}

Example 1: Is |p=1| a limit point of |X={1}|?
%
No! |X - {p} = {}| (there is no |q/=p| in |X|), thus there cannot
exist a function |getq| because it would have to return elements in
the empty set!

Example 2: Is |p=1| a limit point of the open interval |X = (0,1)|?
%
First note that |p ∉ X|, but it is ``very close'' to |X|.
%
A proof needs a function |getq| which from any |ε| computes a point |q
= getq ε| which is in both |X| and |B 1 ε|.
%
We need a point |q| which is in |X| and \emph{closer} than |ε| from |1|.
%
We can try with |q = 1-ε/2| because |absBar (1-(1-ε/2)) = absBar (ε/2) = ε/2
< ε| which means |q ∈ B 1 ε|.
%
We also see that |q/=1| because |ε > 0|.
%
The only remaining thing to check is that |q ∈ X|.
%
This is true for sufficiently small |ε| but the function |getq| must
work for all positive reals.
%
We can use any value in |X| (for example |17/38|) for |ε| which are
``too big'' (|ε >= 2|).
%
Thus our function can be
%
\begin{spec}
  getq ε  | ε < 2      = 1 - ε/2
          | otherwise  = 17/38
\end{spec}
%
A slight variation which is often useful would be to use |max| to
define |getq ε = max (17/38,1-ε/2)|.
%

Similarly, we can show that any internal point (like |1/2|) is a limit
point.

Example 3: limit of an infinite discrete set |X|
%
\begin{spec}
X = {1/n | n ∈ Pos }
\end{spec}
%
Show that |0| is a limit point of |X|.
%
Note (as above) that |0 ∉ X|.

We want to prove |Limp 0 X| which is the same as |∃ getq : Q? ∀ ε > 0?
getq ε ∈ B 0 ε|.
%
Thus, we need a function |getq| which takes any |ε > 0| to an element
of |X - {0} = X| which is less than |ε| away from |0|.
%
Or, equivalently, we need a function |getn : {-"ℝ_{> 0}"-} → Pos| such
that |1/n < ε|.
%
Thus, we need to find an |n| such that |1/ε < n|.
%
If |1/ε| would be an integer we could use the next integer (|1 + 1/ε|),
so the only step remaining is to round up:
%
\begin{spec}
getq ε = 1/getn ε
getn ε = 1 + ceiling (1/ε)
\end{spec}
%
\begin{exercise}
prove that |0| is the \emph{only} limit point of |X|.
\end{exercise}
\emph{Proposition}: If |X| is finite, then it has no limit points.

\begin{spec}
∀ p ∈ ℝ? not (Limp p X)
\end{spec}
%
This is a good exercise in quantifier negation!
%
\begin{spec}
  not (Limp p X)                                  = {- Def. of |Limp| -}
  not (∃ getq : Q? ∀ ε > 0? getq ε ∈ B p ε)       = {- Negation of existential -}
  ∀ getq : Q? not (∀ ε > 0? getq ε ∈ B p ε)       = {- Negation of universal -}
  ∀ getq : Q? ∃ ε > 0? not (getq ε ∈ B p ε)       = {- Simplification -}
  ∀ getq : Q? ∃ ε > 0? absBar (getq ε - p) >= ε
\end{spec}
%
Thus, using the ``functional interpretation'' of this type we see that
a proof needs a function |noLim|
%
\begin{spec}
noLim : (getq : Q) → RPos
\end{spec}
%
such that |let ε = noLim getq in absBar (getq ε - p) >= ε|.

Note that |noLim| is a \emph{higher-order} function: it takes a
function |getq| as an argument.
%TODO: Perhaps refer to explanation in \cref{sec:functions-and-scoping}.
%
How can we analyse this function to find a suitable |ε|?
%
The key here is that the range of |getq| is |X - {p}| which is a
finite set (not containing |p|).
%
Thus we can enumerate all the possible results in a list |xs = [x1,
x2, {-"\ldots"-} xn]|, and measure their distances to |p|: |ds = map
(\x-> absBar (x - p)) xs|.
%
Now, if we let |ε = minimum ds| we can be certain that
%
|absBar (getq ε - p) >= ε| just as required (and |ε /= 0| because |p
`notElem` xs|).

\begin{exercise}
Show that |Limp p X| implies that |X| is infinite.
\end{exercise}
%
Show how to construct an infinite sequence |a : Nat -> REAL| of points
in |X - {p}| which gets arbitrarily close to |p|.
%
Note that this construction can be seen as a proof of |Limp p X =>
Infinite X|.

\subsection{The limit of a sequence}
% TODO: transcribe more of the 2016 notes
\label{par:LimitOfSequence}

Now we can move from limit points to the more familiar limit of a
sequence.
%
At the core of this \course{} is the ability to analyse definitions from
mathematical texts, and here we will use the definition of the limit
of a sequence of \citet[page 498]{adams2010calculus}:

\begin{quote}
  We say that sequence ${a_n}$ converges to the limit $L$, and we
  write $\lim_{n→∞} a_n = L$, if for every positive real number |ε|
  there exists an integer $N$ (which may depend on |ε|) such that if
  $n > N$, then |absBar (an - L) < ε|.
\end{quote}
%

The first step is to type the variables introduced.
%
A sequence |a| is a function from |Nat| to |REAL|, thus |a : Nat ->
REAL| where |an| is special syntax for normal function application of
|a| to |n : Nat|.
%
Then we have |L : REAL|, |ε : RPos|, and |N : Nat| (or |getN : RPos ->
Nat| as we will see later).
%

In the next step we analyse the new concept introduced:
%
the syntactic form $\lim_{n→∞} a_n = L$ which we could express as an
infix binary predicate |haslim| where |a haslim L| is well-typed if |a
: Nat -> REAL| and |L : REAL|.
%

The third step is to formalise the definition using logic: we define
|haslim| using a ternary helper predicate |P|:
%
\begin{spec}
  a haslim L  =  ∀ ε > 0? P a L ε   -- ``for every positive real number |ε| \ldots''

  P a L ε  = ∃ N : Nat? ∀ n ≥ N? absBar (an - L) < ε
           = ∃ N : Nat? ∀ n ≥ N? an ∈ B L ε
           = ∃ N : Nat? I a N ⊆ B L ε
\end{spec}
%
where we have introduced an ``image function'' for sequences ``from |N| onward'':
%
\begin{spec}
  I : (Nat → X) → Nat → PowSet X
  I a N = {a n | n ≥ N}
\end{spec}

The ``forall-exists''-pattern is very common and it is often useful to
transform such formulas into another form.
%
In general |Forall (x : X) (Exists (y : Y) (Q x y))| is equivalent to
|Exists (gety : X -> Y) (Forall (x : X) (Q x (gety x)))|.
%
In the new form we more clearly see the function |gety| which shows
how the choice of |y| depends on |x|.
%
For our case with |haslim| we can thus write
%
\begin{spec}
  a haslim L  =  ∃ getN : RPos -> Nat? ∀ ε > 0? I a (getN ε)  ⊆  B L ε
\end{spec}
%
where we have made the function |getN| more visible.
%
The core evidence of |a haslim L| is the existence of such a function
(with suitable properties).

% TODO: perhaps swap the argument order in the definition of |Limp| to
% make it fit better with |haslim|.

\begin{exercise}
Prove that the limit of a sequence is unique.
\end{exercise}

%TODO: add the proof from file:~/src/DSLM/DSLsofMath/2016/Lectures/BasicConcepts.lhs

\begin{exercise}
%Prove |(a1 haslim L1) & (a2 haslim L2) => (a1+a2) haslim (L1+L2)|.
Prove |(a haslim L) & (b haslim M) => (a+b) haslim (L+M)|.
\end{exercise}

% TODO: Perhaps include something about the relation between |haslim| and |Limp|

% TODO: Perhaps continue with one example: the limit of |invSeq| is |0|. See ../blackboard/W1/20170116_165408.jpg and 20170116_165412.jpg

When we are not interested in the exact limit, just that it exists, we
say that a sequence |a| is \emph{convergent} when |∃ L ? a haslim L|.

\subsection{Case study: The limit of a function}
\label{sec:LimitOfFunction}
%
As our next mathematical text book quote we take the definition of the
limit of a function of type |REAL -> REAL| from
\citet{adams2010calculus}:
%
\label{sec:FunLimit}
%
\begin{quote}
  \textbf{A formal definition of limit}

  We say that \(f(x)\) \textbf{approaches the limit} \(L\) as \(x\)
  \textbf{approaches} \(a\), and we write

  \[\lim_{x\to a} f(x) = L,\]

  if the following condition is satisfied:\\
  for every number \(\epsilon > 0\) there exists a number
  \(\delta > 0\), possibly depending on \(\epsilon\), such that if
  |0 < absBar (x - a) < delta|, then \(x\) belongs to the domain of \(f\)
  and
  \begin{spec}
    absBar (f(x) - L) < epsilon {-"."-}
  \end{spec}

\end{quote}
%
The |lim| notation has four components: a variable name |x|, a point
|a|, an expression \(f(x)\) and the limit |L|.
%
The variable name and the expression can be combined into just the
function |f|\footnote{To see why this works in the general case of any expression of $x$, read \cref{sec:functions-and-scoping}}
and this leaves us with three essential components: |a|, |f|, and |L|.
%
Thus, |lim| can be seen as a ternary (3-argument) predicate which is
satisfied if the limit of |f| exists at |a| and equals |L|.
%
If we apply our logic toolbox we can define |lim| starting something
like this:
%
\begin{spec}
lim a f L  =  Forall (epsilon > 0) (Exists (delta > 0) (P epsilon delta))
\end{spec}
%
when |P| is a predicate yet to define.
%
Indeed, it is often useful to introduce a local name (like |P| here)
to help break the definition down into more manageable parts.
%
If we now naively translate the last part we get this ``definition''
for~|P|:
%
\begin{spec}
{-"\quad"-}  where  P epsilon delta = (0 < absBar (x - a) < delta) => (x `elem` Dom f  && absBar (f x - L) < epsilon))
\end{spec}
%
Note that there is a scoping problem: we have |a|, |f|, and |L| from
the ``call'' to |lim| and we have |epsilon| and |delta| from the two
quantifiers, but where did |x| come from?
%
It turns out that the formulation ``if \ldots then \ldots'' hides a
quantifier that binds |x|.
%
Thus we get this definition:
%
\begin{spec}
lim a f L  =  Forall (epsilon > 0) (Exists (delta > 0) (Forall x (P epsilon delta x)))
  where  P epsilon delta x = (0 < absBar (x - a) < delta) => (x `elem` Dom f  && absBar (f x - L) < epsilon))
\end{spec}
%
The predicate |lim| can be shown to be a partial function of two
arguments, |a| and |f|.
%
This means that at a point |a| each function |f| can have \emph{at
  most} one limit~|L|.
%
(This is not evident from the definition and proving it is a good
exercise.)

\begin{exercise}
  What does Adams mean by ``\(\delta > 0\), possibly depending on
  \(\epsilon\)''?
  %
  How did we express ``possibly depending on'' in our formal
  defintion?
  %
  Hint: how would you express that |delta| cannot depend on |epsilon|?
\end{exercise}
%Answer: by swapping the order of quantifier
