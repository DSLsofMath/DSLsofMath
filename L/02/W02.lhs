\section{Week 2: Logic and calculational proofs}

Course learning outcomes:

\begin{itemize}
\item Knowledge and understanding
  \begin{itemize}
  \item design and implement a DSL (Domain Specific Language) for a new domain
  \item organize areas of mathematics in DSL terms
  \item explain main concepts of elementary real and complex analysis,
        algebra, and linear algebra
  \end{itemize}
\item Skills and abilities
  \begin{itemize}
  \item develop adequate notation for mathematical concepts
  \item perform calculational proofs
  \item use power series for solving differential equations
  \item use Laplace transforms for solving differential equations
  \end{itemize}
\item Judgement and approach
  \begin{itemize}
  \item discuss and compare different software implementations of
        mathematical concepts
  \end{itemize}
\end{itemize}

This week we focus on ``develop adequate notation for mathematical
concepts'' and ``perform calculational proofs'' (still in the context
of ``organize areas of mathematics in DSL terms'').

There will be a fair bit of theory: introducing propositional and
first order logic, but also ``applications'' to mathematics: prime
numbers, (ir)rationals, limit points, limits, etc.

\begin{code}
module DSLsofMath.W02 where
\end{code}

\subsection{Propositional Calculus}

Now we turn to the main topic of this week: logic and proofs.

TODO: tidy up the notes below

Swedish: Satslogik

\begin{tabular}{lll}
   |A|, |B|, |C|, \ldots  & names of propositions &
\\ |False|, |True| & Constants &
\\ |And|      & $\wedge$       & |&|
\\ |Or|       & $\vee$         & ||||
\\ |Implies|  & $\Rightarrow$  &
\\ |Not|      & $\neg$         &
\end{tabular}

Example:
%
\begin{code}
sw :: PropCalc
sw = ((a & b) -=> (b & a))
  where  a = N "A"
         b = N "B"
\end{code}
%
The example is based on the following embedding of propositional calculus terms:
\begin{code}
data PropCalc  =  N  Name
               |  C  Bool
               |  And      PropCalc  PropCalc
               |  Or       PropCalc  PropCalc
               |  Implies  PropCalc  PropCalc
               |  Not      PropCalc

(&) = And
(-=>) = Implies

type Name = String
\end{code}
With this datatype we can write an evaluator to |Bool| which computes
the truth value of a term given an enviroment:
%
\begin{code}
evalPC :: (Name -> Bool) -> PropCalc -> Bool
evalPC = error "Exercise"
\end{code}
%
The function |evalPC| translates from the syntactic to the semantic
domain.
%
Here |PropCalc| is the (abstract) \emph{syntax} of the language of
propositional calculus and |Bool| is the \emph{semantic
  domain}.
%
Alternatively, we can view |(Name -> Bool) -> Bool| as the semantic
domain.
%
A value of this type is a mapping from a truth table to |Bool|.
%
This mapping is often also tabulated as a truth table with one more
``output'' column.

As a first example, consider the proposition |t = Implies False a|.
%
The truth table semantics of |t| is usually drawn as follows: one
column for the name |a| listing all combinations of |T = Truth| and |F
= False|, and one column for the result of evaluating the expression.
%
\begin{tabular}{||l||l||}
    \hline   a & t
  \\\hline   F & T
  \\         T & T
  \\\hline
\end{tabular}

If we continue with the example |sw| from above we have two names |a|
and |b| which together can have any of four combinations of true and
false. After the name-columns are filled, we fill in the rest of the
table one operation (column) at a time. The |&| columns become |F F F
T| and finally the |=>| column becomes true everywhere.

\begin{tabular}{||lllllll||}
    \hline   |a| & |&| & |b| & |=>| & |b| & |&| & |a|
  \\\hline    F  &  F  &  F  &  T   &  F  &  F  &  F
  \\          F  &  F  &  T  &  T   &  T  &  F  &  F
  \\          T  &  F  &  F  &  T   &  F  &  F  &  T
  \\          T  &  T  &  T  &  T   &  T  &  T  &  T
  \\\hline
\end{tabular}

A proposition whose truth table is always true is called a
\emph{tautology}.
%
Truth table verification is only viable for propositions with few
names because of the exponential growth in the number of cases to
check: we get $2^n$ cases for |n| names.
%
(There are very good heuristic algorithms to look for tautologies even
for thousands of names --- but that is not part of this course.)

What we call ``names'' are often called ``(propositional) variables''
but we will soon add another kind of variables (and quantification
over them) to the calculus.



\subsection{First Order Logic (predicate logic)}

Swedish: Första ordningens logik = predikatlogik

% Adds term variables and functions, predicate symbols and quantifiers (sv: kvantorer).

We now add \emph{terms} as another datatype to the calculus.
%
A \emph{term} is either a (term) \emph{variable} (like |x|, |y|, |z|),
or the application of a \emph{function symbol} (like |f|, |g|) to a
suitable number of terms.
%
If we have the function symbols |f| of arity |2| and |g| of arity |3|
we can form terms like |f(x,x)|, |g(y,z,z)|, |g(x,y,f(x,y))|, etc.

TODO: Add simple datatype for terms. (Perhaps |Rat| from further down?)

The names from the propositional calculus are generalised to
\emph{predicate symbols} of different arity.
%
The predicate symbols can only be applied to terms, not to other
predicate symbols or formulas.
%
If we have the predicate symbols |N| of arity |0|, |P| of arity |1|
and |Q| of arity |2| we can form \emph{formulas} like |N|, |P(x)|,
|Q(f(x,x),y)|, etc.

Note that we have two separate layers: terms cannot contain formulas,
but formulas normally contain terms.

The formulas introduced so far are all \emph{atomic formulas} but we
will add two more concepts: first the logical conenctives from the
propositional calculus: |And|, |Or|, |Implies|, |Not|, and then two
quantifiers: |Forall| and |Exists|.
%TODO: fix formatting of forall and exists

An example FOL formula:
%
\[
  \forall x(P(x)\rightarrow (\exists y(Q(f(x,x),y))))
\]
%
Note that FOL can only quantify over \emph{term} variables, not over
predicates.
%
(Second order logic and higher order logic allow quantification over
predicates.)

Another example: a formula stating that |+| is commutative:
\[
  \forall x(\forall y((x+y)==(y+x)))
\]
%
Note that |==| is a binary predicate symbol while |+| is a binary
function symbol.
%
Here is the same formula without infix operators:
%
\begin{spec}
  Forall x (Forall y (Eq(plus(x,y),plus(y,x))))
\end{spec}

TODO: perhaps add simple |data FOL| for the formulas.

Forall quantification can be seen as a generalisation of |And|.
%
First we can generalise the binary operator to an |n|-ary version:
|Andn|.
%
To prove |Andn A1 A2 ... An| we need a proof of each |Ai|.
%
Thus we could define |Andn A1 A2 ... An = A1 & A2 & ... & An| where
|&| is the infix version of binary |And|.
%
The next step is to note that the formulas |Ai| can be generalised to
|A(i)| where |i| is a term variable and |A| is a unary predicate
symbol.
%
We can think of |i| ranging over an infinite collection of constant
terms |i0|, |i1|, \ldots
%
Then the final step is to introduce the notation |Forall i A(i)| for
|A(i1) & A(i2) & ... |.
%
%
% TODO (by DaHe) It might be a good idea to explain the below paragraph
% (until next subsection) in a more informal way too, since this is a
% fairly technical explanation.  Something like "If we can show P(a)
% for some unknown term a, we know P(t) for any t, since we have not
% relied on any specific property of a specific t".  A simple example
% might also be a good idea, where we end up with a function f where
% f t is a proof of P t for all terms t.
%
Now to prove |Forall x P(x)| it would be difficult to provide an
infinite collection of proofs of |P(xi)|.
%
Instead the standard procedure is to introduce a fresh constant term
|a| and prove |P(a)|.
%
Another way to view this is to say that a proof of |Forall x (P x)| is
a function |f| from terms to proofs such that |f t| is a proof of
|P t| for all terms |t|.

\subsection{An aside: Pure set theory}

One way to build mathematics from the ground up is to start from pure
set theory and define all concepts by translation to sets.
%
We will only work with this as a mathematical domain to study, not as
``the right way'' of doing mathematics.
%
The core of the language of pure set theory has the Empty set, the
one-element set constructor Singleton, set Union, and Intersection.
%
There are no ``atoms'' or ``elements'' to start from except for the
empty set but it turns out that quite a large part of mathematics can
still be expressed.

\paragraph{Natural numbers} To talk about things like natural numbers
in pure set theory they need to be encoded. Here is one such encoding
(which is explored further in the first hand-in assignment).

\begin{spec}
vonNeumann 0        =  Empty
vonNeumann (n + 1)  =  Union  (vonNeumann n)
                              (Singleton (vonNeumann n))
\end{spec}

\paragraph{Pairs}

Definition:  A pair |(a,b)| is encoded as |{{a},{a,b}}|.

% (a,b) \coloneqq \big\{\,\{a\},\{a,b\}\,\big\} ;

TODO: merge the text below and above

As an example term language we can use pure (untyped) set theory.
%
We have a nullary function symbol |{}| for the empty set (sometimes
writen $\emptyset$) and a unary function symbol |S| for the function
that builds a singleton set from an ``element''.
%
In pure set theory we don't actually have any ``elements'' to start
from: every term denotes a set.
%
All non-variable terms so far are |{}|, |S {}|, |S (S {})|, \ldots
%
The first set is empty but all the others are one-element sets.

Next we add two binary function symbols for union and intersection of
sets (denoted by terms).
%
Using union we can build sets of more than one element, for example
|Union (S {}) (S (S {}))| which has two ``elements'': |{}| and |S {}|.
%

FOL does not have function definitions or recursion, but in a suitable
meta-langauge (like Haskell) we can write a function that creates a
set with |n| elements (for any natural number |n|) as a term in FOL:
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
  over 1, over 2}| and, in general, that |over n| has cardinality |n|.
%
The function |vN| is explored in more detail in the first assignment
of the DSLsofMath course.

The constructions presented so far show that, even starting from no
elements, we can embed all natural numbers in pure set theory.
%
We can also embed unordered pairs: |{a, b} =~= Union (S a) (S b)|
and normal ordered pairs: |(a, b) =~= {S a, {a, b}}|.
%
% |{S a, {a, b}} = Union (S (S a)) (S (Union (S a) (S b)))|
With a bit more machinery it is possible to step by step encode |Nat|,
|ZZ|, |QQ|, |REAL|, |COMPLEX|.

\subsection{Back to quantifiers}

After this detour through untyped set land let us get back to the most
powerful concept of FOL: the quantifiers.
%
We have already seen how the ``forall'' quantifier can be seen as a
generalisation of |And| and in the same way we can see the ``exists''
quantifier as a generalisation of |Or|.
%

First we generalise the binary |Or| to an |n|-ary |Orn|.
%
To prove |Orn A1 A2 ... An| is enough (and necessary) to find one |i|
for which we can prove |Ai|.
%
As before we then take the step from a family of formulas |Ai| to one
unary predicate |A| expressing the formulas |A(i)| for the term
variable |i|.
%
Then the final step is to ``or'' all these formulas to obtain |Exists
i (A i)|.

At this point it is good to sum up and compare the two quantifiers and
how to prove them:

\begin{quote}
|(t, bt)| is a proof of |Exists x (P(x))| if |bt| is a proof of |P(t)|.

|f| is a proof of |Forall x (P(x))| if |f t| is a proof of |P(t)| for all |t|.
\end{quote}

If we abbreviate ``is a proof'' as |:| and use the Haskell convention
for function application we get
%
\begin{spec}
(t, bt)  :  Exists x (P x)   {-"\quad\textbf{if}\quad"-}  bt   : P t
f        :  Forall x (P x)   {-"\quad\textbf{if}\quad"-}  f t  : P t   {-"\text{~for all~}"-}  t
\end{spec}
%
This now very much looks like type rules, and that is not a coincidence.
%
The Curry-Howard correspondence says that we can think of propositions
as types and proofs as ``programs''.
%
These typing judgments are not part of FOL, but the correspondence is
used quite a bit in this course to keep track of proofs.

TODO: Add more about Curry-Howard (the binary logical connectives, etc.)

TODO: find the right place for the a note that the type of tuples is
isomorphic to the (dependent) function type |{i : 1..n} -> Ai|.

TODO: Add typed quantification for Exists.

(Roughly: |((Exists (x:T) (P x))) = ((Exists x (T x & P x)))|.)

\subsection{Proof by contradition}

Let's try to express and prove the irrationality of the square
root of 2.
%
We have two main concepts involved: the predicate "irrational" and the
function "square root of".
%
The square root function (for positive real numbers) can be specified
by $r = \sqrt{s}$ iff |r^2 == s| and |r : Nat|.
%
The formula ``x is irrational'' is just |not(R x)| where |R| is the
predicate ``is rational''.
%
\begin{spec}
  R x = Exists (a:Nat) (Exists (b:Pos) (b*x==a & GCD(a,b)==1))
\end{spec}

The classical way to prove a negation |not P| is to assume |P| and
derive something absurd (some |Q| and |not Q|, for example).
%
Lets take |P = R r| and |Q = GCD(a,b)==1|.
%
Assuming |P| we immediately get |Q| so what we need is to prove |not
Q|, that is |GCD(a,b)/=1|.
%
We can use the equations |b*r==a| and |r^2 == 2|.
%
Squaring the first equation and using the second we get |b^2*2==a^2|.
%
Thus |a^2| is even, which means that |a| is even, thus |a==2*c| for
some |c|.
%
But then |b^2*2==a^2==4*c^2| which means that |b^2==2*c^2|.
%
By the same reasoning again we have that also |b| is even.
%
But then |GCD(a,b)>=2| which implies |not Q|.

To sum up: by assuming |P| we can prove both |Q| and |not Q|.
%
Thus, by contradiction |not P| must hold.

\subsection{Proof by cases}
%
% TODO (by DaHe): Shouldn't `a` and `b` be replaced with `p` and `q` on the line below?
%
As another example, let's prove that there are two irrational numbers
|p| and |q| such that |p^q| is rational.

\begin{spec}
  S = Exists p (Exists q (not (R p) & not (R q) & R (p^q)))
\end{spec}

We know from above that |r = sqrt 2| is irrational, so as a first
attempt we could set |p=q=r|.
%
Then we have satisfied two of the three clauses (|not (R p)| and |not
(R q)|).
%
What about the third clause: is |x=p^q==r^r| rational?
%
We can reason about two possible cases, one of which has to hold: |R
x| or |not (R x)|.

Case 1: |R x| holds. Then we have a proof of |S| with |p=q=r=sqrt 2|.

Case 2: |not (R x)| holds. Then we have another irrational number |x|
to play with.
%
Let's try |p=x| and |q=r|.
%
Then |p^q == x^r == (r^r)^r == r^(r*r) == r^2 == 2| which is clearly
rational.
%
Thus, also in this case we have a proof of |S|, but now with |p=r^r|
and |q=r|.

To sum up: yes, there are irrational numbers such that their power is
rational.
%
We can prove the existence without knowing what numbers |p| and |q|
actually are!

\subsection{Functions as proofs}

To prove a formula |P => Q| we assume a proof |p : P| and derive a
proof |q : Q|.
%
Such a proof can be expressed as |(\p -> q) : (P => Q)|: a proof of an
implication is a function from proofs to proofs.

As we saw earlier, a similar rule holds for the ``forall'' quantifier:
a function |f| from terms |t| to proofs of |P t| is a proof of |Forall
x (P x)|.

A very common kind of formula is ``typed quantification'': if a type
(a set) |S| of terms can be decribed as those that satisfy the unary
predicate |T| we can introduce the short-hand notation
%
\begin{spec}
  ((Forall (x:T) (P x))) = ((Forall x (T x => P x)))
\end{spec}
%
A proof of this is a two-argument function |p| which takes a term and
a proof to a proof.

In pseudo-Haskell we can express the implication laws as follows:
%
\begin{spec}
  impIntro  : (A -> B) -> (A=>B)
  impElim   : (A=>B) -> (A -> B)
\end{spec}
%
It should come as no surprise that this ``API'' can be implemented by
|(=>) = (->)|, which means that both |impIntro| and |impElim| can be
implemented as |id|.

Similarly we can express the universal quantification laws as:
%
\begin{spec}
  AllIntro  : ((a : Term) -> P a) -> (Forall x (P x))
  AllElim   : (Forall x (P x)) -> ((a : Term) -> P a)
\end{spec}
%
To actually implement this we need a \emph{dependent} function type,
which Haskell does not provide.
%
But we can still use it as a tool for understanding and working with
logic formulas and mathematical proofs.
%

Haskell supports limited forms of dependent types and more is coming
every year but for proper dependently typed programming I recommend
the language Agda.


\subsection{Proofs for |And| and |Or|}

TODO: textify

\begin{spec}
  andIntro  :  P  ->  Q  -> P&Q
  andElimL  :  P&Q ->  P
  andElimR  :  P&Q ->  Q
\end{spec}

If we see these introduction and elimination rules as an API, what
would be a resonable implementation of the datatype |P&Q|?
%
A type of pairs!
%
Then we see that the corresponding Haskell functions would be
%
\begin{spec}
  pair  :: p -> q -> (p, q)  -- andIntro
  fst   :: (p, q) -> p       -- andElimL
  snd   :: (p, q) -> q       -- andElimR
\end{spec}

\begin{spec}
  orIntroL  :  P   ->  P|Q
  orIntroR  :  Q   ->  P|Q
  orElim    :  (P=>R)->(Q=>R) -> ((P|Q) => R)
\end{spec}

Here the implementation type can be a labelled sum type, also called
disjoint union and in Haskell: |Either|.
%TODO: put in separate file, make sure it type checks, include back.
\begin{spec}
  data Either p q = Left p | Right q
  -- |Left| is |orIntroL|, |Right| is |orIntroR|
  either :: (p->r) -> (q->r) -> Either p q -> r
  either l r (Left x)   =  l x
  either l r (Right y)  =  r y
\end{spec}

TODO: include |Either| in the Haskell intro at the end of the previous chapter

\subsection{Case study: there is always another prime}

As an example of combining forall, exists and implication let us turn
to one statement of the fact that there are infiniely many primes.
%
If we assume we have a unary predicate expressing that number is prime
and a binary (infix) predicate ordering the natural numbers we can
define a formula |IP| for ``Infinite many Primes'' as follows:
%
\begin{spec}
 IP = Forall n (Prime n => Exists m (Prime m & m > n))
\end{spec}
%*TODO: perhaps introduce Prime and < in L01
%
Combined with the fact that there is at least one prime (like |2|) we
can repeatadly refer to this statement to produce a never-ending
stream of primes.

To prove this formula we first translate from logic to programs as
described above.
%
We can translate step by step, starting from the top level.
%
The forall-quantifier translates to a (dependent) function type |(n :
Term) -> | and the implication to a normal function type |Prime n ->|.
%
The exists-quantifier translates to a (dependent) pair type |((m :
Term), ...)| and finally the |&| translates into a pair type.
%
Putting all this together we get a type signature for any |proof| of
the theorem:
%
\begin{spec}
proof : (n : Term) -> Prime n -> ((m : Term), (Prime m, m>n))
\end{spec}

Now we can start filling in the definition of |proof| as a
two-argument function returning a nested pair:
%
\begin{spec}
proof n np = (m, (pm, gt))
  where  m' = 1 + factorial n
         m  = {- some non-trivial prime factor of |m'| -}
         pm = {- a proof that |m| is prime -}
         gt = {- a proof that |m>n| -}
\end{spec}
%
The proof |pm| is the core of the theorem.
%
First, we note that for any |2<=p<=n| we have
%
\begin{spec}
 mod m' p ==
 mod (1 + n!) p ==
 mod 1 p + mod (n!) p ==
 1 + 0 ==
 1
\end{spec}
% TODO: explain that mod x y is shown as x % y
%
Thus |m'| is not divisible by any number from |2| to |n|.
%
But is it a prime?
%
If |m'| is prime then |m=m'| and the proof is done (because |1+n! >= 1
+ n > n|).
%
Otherwise, let |m| be a prime factor of |m'| (thus |m'=m*q|, |q>1|).
%
Then |1 == mod m' p == (mod m p)*(mod q p)| which means that neither
|m| nor |q| are divisible by |p| (otherwise the product would be
zero).
%
Thus they must both be |>n|.
%
QED.

Note that the proof can be used to define a somewhat useful function
which takes any prime number to some larger prime number.
%
We can compute a few example values:

\begin{tabular}{r@@{ $\mapsto$ }rl}
     2 &  3 &( 1+2! )
  \\ 3 &  7 &( 1+3! )
  \\ 5 & 11 &( 1+5! = 121 = 11*11 )
  \\ 7 & 71 &\ldots
\end{tabular}

\subsection{Existantial quantification as a pair type}

We mentioned before that existential quantification can be seen as as
a ``big |Or|'' of a family of formulas |P a| for all terms |a|.
%
This means that to prove the quantification, we only need exhibit one
witness and one proof for that member of the family.
%
\begin{spec}
  ExistsIntro  :  (a : Term) -> P a -> (Exists x (P x))
\end{spec}
%
For binary |Or| the ``family'' only had two members, one labelled |L|
for |Left| and one |R| for |Right|, and we used one introduction
rule for each.
%
Here, for the generalisation of |Or|, we have unified the two rules
into one with an added parameter |a| corresponding to the label which
indicates the family member.

In the other direction, if we look at the binary elimination rule, we
we see the need for two arguments to be sure of how to prove the
implication for any family member of the binary |Or|.
\begin{spec}
orElim    :  (P=>R)->(Q=>R) -> ((P|Q) => R)
\end{spec}
%
The generalisation unifies these two to one family of arguments.
%
If we can prove |R| for each member of the family, we can be sure to
prove |R| when we encounter some family member:
%
\begin{spec}
  ExistsElim   :  ((a:Term)-> P a => R) -> (Exists x (P x)) => R
\end{spec}
%
The datatype corresponding to |Exists x (P x)| is a pair of a witness
|a| and a proof of |P a|.
%
We sometimes write this type |(a:Term, P a)|.

\subsection{Basic concepts of calculus}

Now we have built up quite a bit of machinery to express logic
formulas and proofs.
%
It is time time to apply it to some concepts in calculus.
%
We start we the concept of ``limit point'' which is used in the
formulation of different properties of limits of functions.

TODO: Perhaps start with the ``expression'' $lim_{x\to x_0} f(x)$ and
explain that not all |x_0| make sense, etc. [For context and
motivation.]

TODO: Or talk a bit about open and closed sets. (Open set = every
point is internal = there is some wiggle-room around each point in the
set. Closed set contains all its limit points.)

\paragraph{Limit point}

\emph{Definition} (adapted from \cite{rudin1964principles}, page 28):
Let |X| be a subset of |ℝ|.
%
A point |p ∈ ℝ| is a limit point of |X| iff for every |ε > 0|, there
exists |q ∈ X| such that |q ≠ p| and |abs(q - p) < ε|.
%
% TODO: Maybe explain the notation 𝒫 ℝ, i.e. Any subset of REAL has this type.
%
\begin{spec}
Limp : ℝ → 𝒫 ℝ → Prop
Limp p X = ∀ ε > 0? ∃ q ∈ X - {p}? abs (q-p) < ε
\end{spec}
%
Notice that |q| depends on |ε|.
%
Thus by introducing a function |getq| we can move the |∃| out.

\begin{spec}
type Q = {-"ℝ_{> 0}"-} → (X - {p})
Limp p X = ∃ getq : Q? ∀ ε > 0? |getq ε - p| < ε
\end{spec}

Next: introduce the ``disk function'' |Di|.

TODO: perhaps rename |Di| to $N$ for ``neighbourhood'' (or something based on ``open ball'').
%
\begin{spec}
Di : ℝ → {-"ℝ_{> 0}"-} → 𝒫 ℝ
Di c r = {x | abs (x - c) < r}
\end{spec}
Then we get
\begin{spec}
Limp p X = ∃ getq : Q? ∀ ε > 0? getq ε ∈ Di p ε
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
= getq ε| which is in both |X| and |Di 1 ε|.
%
We need a point |q| which is in |X| and \emph{closer} than |ε| from |1|
%
We can try with |q = 1-ε/2| because |abs (1-(1-ε/2)) = abs (ε/2) = ε/2
< ε| which means |q ∈ Di 1 ε|.
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

\begin{spec}
X = {1/n | n ∈ Pos }
\end{spec}

Show that |0| is a limit point of |X|.
%
Note (as above) that |0 ∉ X|.

We want to prove |Limp 0 X| which is the same as |∃ getq : Q? ∀ ε > 0?
getq ε ∈ Di 0 ε|.
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
Exercise: prove that |0| is the \emph{only} limit point of |X|.

\emph{Proposition}: If |X| is finite, then it has no limit points.

\begin{spec}
∀ p ∈ ℝ? not (Limp p X)
\end{spec}
%
This is a good excercise in quantifier negation!
%
\begin{spec}
  not (Limp p X)
= {- Def. of |Limp| -}
  not (∃ getq : Q? ∀ ε > 0? getq ε ∈ Di p ε)
= {- Negation of existential -}
  ∀ getq : Q? not (∀ ε > 0? getq ε ∈ Di p ε)
= {- Negation of universal -}
  ∀ getq : Q? ∃ ε > 0? not (getq ε ∈ Di p ε)
= {- Simplification -}
  ∀ getq : Q? ∃ ε > 0? abs (getq ε - p) >= ε
\end{spec}
%
Thus, using the ``functional interpretation'' of this type we see that
a proof needs a function |noLim|
%
\begin{spec}
noLim : (getq : Q) → RPos
\end{spec}
%
such that |let ε = noLim getq in abs (getq ε - p) >= ε|.

Note that |noLim| is a \emph{higher-order} function: it takes a
function |getq| as an argument.
%
How can we analyse this function to find a suitable |ε|?
%
The key here is that the range of |getq| is |X - {p}| which is a
finite set (not containing |p|).
%
Thus we can enumerate all the possible results in a list |xs = [x1,
x2, {-"\ldots"-} xn]|, and measure their distances to |p|: |ds = map
(\x-> abs (x - p)) xs|.
%
Now, if we let |ε = minimum ds| we can be certain that
%
|abs (getq ε - p) >= ε| just as required (and |ε /= 0| because |p
`notElem` xs|).

Exercise: If |Limp p X| we now know that |X| is infinite.
%
Show how to construct an infinite sequence |a : Nat -> REAL| of points
in |X - {p}| which gets arbitrarily close to |p|.
%
Note that this construction can be seen as a proof of |Limp p X =>
Infinite X|.

\paragraph{The limit of a sequence} TODO: transcribe the 2016 notes
\label{par:LimitOfSequence}

Now we can move from limit points to the more familiar limit of a
sequence.
%
A sequence |a| is a function from |Nat| to |REAL| and we define a
binary infix predicate |haslim| using a helper predicate |P|:
%
\begin{spec}
  a haslim L  =  ∀ ε > 0? P a L ε

  P a ε L = ∃ N : ℤ? ∀ n ≥ N? abs (an - L) < ε
\end{spec}


TODO: perhaps swap the argument order in the definition of |Limp| to
make it fit better with |haslim|.

Exercise: prove that |(a1 haslim L1) & (a2 haslim L2)| implies
|(a1+a2) haslim (L1+L2)|.

% TODO: Perhaps include something about the relation between |haslim| and |Limp|

% TODO: Perhaps continue with one example: the limit of |invSeq| is |0|. See ../blackboard/W1/20170116_165408.jpg and 20170116_165412.jpg


\subsection{Questions and answers from the exercise sessions week 2}

\paragraph{Variables, |Env| and |lookup|}

This was a frequently source of confusion already the first week so
there is already a question + answers earlier in this text.
%
But here is an additional example to help clarify the matter.
\begin{code}
data Rat v = RV v | FromI Integer | RPlus (Rat v) (Rat v) | RDiv (Rat v) (Rat v)
  deriving (Eq, Show)

newtype RatSem = RSem (Integer, Integer)
\end{code}
We have a type |Rat v| for the syntax trees of rational number
expressions and a type |RatSem| for the semantics of those rational
number expressions as pairs of integers.
%
The constructor |RV :: v -> Rat v| is used to embed variables with
names of type |v| in |Rat v|.
%
We could use |String| instead of |v| but with a type parameter |v| we
get more flexibility at the same time as we get better feedback from
the type checker.
%
To evaluate some |e :: Rat v| we need to know how to evaluate the
variables we encounter.
%
What does ``evaluate'' mean for a variable?
%
Well, it just means that we must be able to translate a variable name
(of type |v|) to a semantic value (a rational number in this case).
%
To ``translate a name to a value'' we can use a function (of type |v
-> RatSem|) so we can give the following implementation of the
evaluator:
%
\begin{code}
evalRat1 ::  (v -> RatSem) -> (Rat v -> RatSem)
evalRat1 ev (RV v)       = ev v
evalRat1 ev (FromI i)    = fromISem i
evalRat1 ev (RPlus l r)  = plusSem  (evalRat1 ev l) (evalRat1 ev r)
evalRat1 ev (RDiv  l r)  = divSem   (evalRat1 ev l) (evalRat1 ev r)
\end{code}
Notice that we simply added a parameter |ev| for ``evaluate variable''
to the evaluator.
%
The rest of the definition follows a common pattern: recursively
translate each subexpression and apply the corresponding semantic
operation to combine the results: |RPlus| is replaced by |plusSem|,
etc.
%
\begin{code}
fromISem :: Integer -> RatSem
fromISem i = RSem (i, 1)

plusSem :: RatSem -> RatSem -> RatSem
plusSem = undefined -- TODO: exercise

-- Division of rational numbers
divSem :: RatSem -> RatSem -> RatSem
divSem (RSem (a, b)) (RSem (c, d)) = RSem (a*d, b*c)
\end{code}

Often the first argument |ev| to the eval function is constructed from
a list of pairs:
\begin{code}
type Env v s = [(v, s)]
envToFun :: (Show v, Eq v) => Env v s -> (v -> s)
envToFun [] v = error ("envToFun: variable "++ show v ++" not found")
envToFun ((w,s):env) v
  | w == v     = s
  | otherwise  = envToFun env v
\end{code}
Thus, |Env v s| can be seen as an implementation of a ``lookup
table''.
%
It could also be implemented using hash tables or binary search trees,
but efficiency is not the point here.
%
Finally, with |envToFun| in our hands we can implement a second
version of the evaluator:
\begin{code}
evalRat2 :: (Show v, Eq v) => (Env v RatSem) -> (Rat v -> RatSem)
evalRat2 env e = evalRat1 (envToFun env) e
\end{code}

\paragraph{The law of the excluded middle}

Many had problems with implementing the ``law of the excluded middle''
in the exercises and it is indeed a tricky property to prove.
%
They key to implementing it lies in double negation and as that is
encoded with higher order functions it gets a bit hairy.

TODO[Daniel]: more explanation

\paragraph{SET and PRED}

Several groups have had trouble grasping the difference between |SET|
and |PRED|.
%
This is understandable, beacuse we have so far in the lectures mostly
talked about term syntax + semantics, and not so much about predicate
syntax and semantics.
%
The one example of terms + predicates covered in the lectures is
Predicate Logic and I never actually showed how eval (for the
expressions) and check (for the predicates) is implemented.

As an example we can we take our terms to be the rational number
expressions defined above and define a type of predicates over those
terms:
\begin{code}
type Term v = Rat v

data RPred v  =  Equal     (Term v) (Term v)
              |  LessThan  (Term v) (Term v)
              |  Positive  (Term v)

              |  AND  (RPred v) (RPred v)
              |  NOT  (RPred v)
  deriving (Eq, Show)
\end{code}
%
Note that the first three constructors, |Eq|, |LessThan|, and
|Positive|, describe predicates or relations between terms (which can contain term
variables)
%
while the two last constructors, |AND| and |NOT|, just combine such
relations together.
%
(Terminology: I often mix the words ``predicate'' and ``relation''.)

We have already defined the evaluator for the |Term v| type but we
need to add a corresponding ``evaluator'' (called |check|) for the
|RPred v| type.
%
Given values for all term variables the predicate checker should just
determine if the predicate is true or false.
\begin{code}
checkRP :: (Eq v, Show v) => Env v RatSem -> RPred v -> Bool
checkRP env (Equal     t1 t2)  = eqSem        (evalRat2 env t1) (evalRat2 env t2)
checkRP env (LessThan  t1 t2)  = lessThanSem  (evalRat2 env t1) (evalRat2 env t2)
checkRP env (Positive  t1)     = positiveSem  (evalRat2 env t1)

checkRP env (AND p q)  = (checkRP env p) && (checkRP env q)
checkRP env (NOT p)    = not (checkRP env p)
\end{code}
Given this recursive definition of |checkRP|, the semantic functions
|eqSem|, |lessThanSem|, and |positiveSem| can be defined by just
working with the rational number representation:
\begin{code}
eqSem        :: RatSem -> RatSem -> Bool
lessThanSem  :: RatSem -> RatSem -> Bool
positiveSem  :: RatSem -> Bool
eqSem        = error "TODO"
lessThanSem  = error "TODO"
positiveSem  = error "TODO"
\end{code}

\subsection{More general code for first order languages}

``överkurs''

It is possible to make one generic implementation which can be
specialised to any first order language.
%

TODO: add explanatory text

\begin{itemize}
\item |Term| = Syntactic terms
\item |n| = names (of atomic terms)
\item |f| = function names
\item |v| = variable names
\item |WFF| = Well Formed Formulas
\item |p| = predicate names
\end{itemize}


\begin{spec}
data Term n f v  =  N n | F f [Term n f v] | V v
  deriving Show

data WFF n f v p =
     P p    [Term n f v]
  |  Equal  (Term n f v)   (Term n f v)

  |  And    (WFF n f v p)  (WFF n f v p)
  |  Or     (WFF n f v p)  (WFF n f v p)
  |  Equiv  (WFF n f v p)  (WFF n f v p)
  |  Impl   (WFF n f v p)  (WFF n f v p)
  |  Not    (WFF n f v p)

  |  FORALL  v (WFF n f v p)
  |  EXISTS  v (WFF n f v p)
  deriving Show
\end{spec}

TODO: Perhaps introduce GADT datatype notation in exercises

\subsection{Exercises: abstract FOL}

%include E2.lhs
