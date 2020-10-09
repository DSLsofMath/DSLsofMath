\subsection{First Order Logic}

%
Our second DSL is that of \emph{First Order Logic\lnOnly{\footnote{Swedish: Första ordningens logik = predikatlogik}}},
or FOL for short, and also known as Predicate Logic.

%TODO: include top-level explanation: Adds term variables and functions, predicate symbols and quantifiers (sv: kvantorer).

Compared to propositional logic, the main addition is
\emph{quantification over individuals}. Additionally, one adds a
language of terms --- its semantic domain being the individuals which
we quantify over.

Let us study terms first.
%
A \emph{term} is either a (term) \emph{variable} (like |x|, |y|, |z|),
or the application of a \emph{function symbol} (like |f|, |g|) to a
suitable number of terms.
%
If we have the function symbols |f| of arity |2| and |g| of arity |3|
we can form terms like |f(x,x)|, |g(y,z,z)|, |g(x,y,f(x,y))|, etc.
%
The individuals are often limited to a single domain. For example here
we will take individuals to be rationals.\jp{why?} Consequently, the
actual function symbols are also domain-specific --- for rationals we
will have addition, division, etc.
%
In this case we can model the terms as a datatype:

\begin{code}
type VarT = String
data RatT = RV VarT | FromI Integer | RPlus RatT RatT | RDiv RatT RatT
  deriving Show
\end{code}
%
The above introduces variables (with the constructor |RV|)
and three function symbols:
%
|FromI| of arity |1|, |RPlus|, |RDiv| of arity |2|.
\begin{exercise}
  Following the usual pattern, write the evaluator for |RatT|:
\begin{code}
evalRat :: RatT -> (VarT -> RatSem) -> RatSem
type RatSem = Rational
\end{code}

%if False
\begin{code}
evalRat = error "evalRat: todo"
\end{code}
%endif

\end{exercise}

As mentioned above, the propositions (often referred to as
\emph{formulas} in the context of FOL) are extended so that they can
refer to terms. That is, the names from the propositional calculus are
generalised to \emph{predicate symbols} of different arity.
%
The predicate symbols can only be applied to terms, not to other
predicate symbols or formulas.
%
If we have the predicate symbols |New| of arity |0|, |Pos| of arity |1|
and |Less| of arity |2| we can form \emph{formulas} like |New|, |Pos(x)|,
|Less(f(x,x),y)|, etc.
%
Note that we have two separate layers, with terms at the bottom:
%
formulas normally refer to terms, but terms cannot refer to formulas.

The formulas introduced so far are all \emph{atomic formulas}:
generalisations of the \emph{names} from |PropCalc|.
%
Now we will add two more concepts:
%
first the logical connectives from the propositional calculus:
%
|And|, |Or|, |Implies|, |Not|, and then two quantifiers:
%
``forall'' (|ForallAlone|) and ``exists'' (|ExistsAlone|).

Thus the following is an example FOL formula:
%
\begin{spec}
  Forall x (Pos(x) => (Exists y (Less(f(x,x),y))))
\end{spec}
%
The fact that quantification is over individuals is a defining
characteristic of FOL.  If one were to, say, quantify over predicates,
we'd have a higher-order logic, with completely different properties.
%

As another example, we can write a formula stating that the function symbol |plus| is
commutative:
%
\begin{spec}
  Forall x (Forall y (Eq(plus(x,y),plus(y,x))))
\end{spec}
%
Here is the same formula with infix operators:
%
\begin{spec}
  Forall x (Forall y ((x+y)==(y+x)))
\end{spec}
%
Note that |==| is a binary predicate symbol (written |Eq| above),
while |+| is a binary function symbol (written |plus| above).


As before we can model the expression syntax (for FOL, in this case)
as a datatype.
%
We keep the logical connectives |And|, |Or|, |Implies|, |Not| from the
type |PropCalc|, add predicates over terms, and quantification.
%
The constructor |Equal| could be eliminated in favour of |PName "Eq"| but
it is often included as a separate constructor.

\begin{code}
type PSym = String
data FOL  =  PName PSym [RatT]
          |  Equal  RatT  RatT

          |  And      FOL   FOL
          |  Or       FOL   FOL
          |  Implies  FOL   FOL
          |  Not      FOL

          |  FORALL  VarT  FOL
          |  EXISTS  VarT  FOL
  deriving Show
\end{code}

\subsubsection{Evaluator for Formulas and \extraMaterial Undecidability}

Setting us up for failure, let us attempt to write an |eval| function
for FOL, as we did for propositional logic.

In propositional logic, we allowed the interpretation of propositional variables
to change depending on the environment. Here, we will let the interpretation of
term variables be dependent on an environment, which will therefore map
(term) variables
to individuals (|VarT -> RatSem|).
If we so wished, we could have an environment for the interpretation of predicate
names, with an environment of type |PSym -> [RatSem] -> Bool|. Rather, with little
loss of generality, we will fix this interpretation, via a constant function |eval0|, which
may look like this:

\begin{code}
eval0 :: PSym -> [RatSem] -> Bool
eval0 "Equal" [t1,t2] = t1 == t2
eval0 "LessThan" [t1,t2] = t1 < t2
eval0 "Positive" [t1] = t1 > 0
\end{code}
etc.

the environment which was mapping
names to truth values is replaced by an environment mapping each
predicate/argument combinations to a truth value: (|PSym -> [RatSem]
-> Bool|). Additionally we need en evironment mapping   So we would use the following
type:

\begin{code}
eval :: FOL -> (VarT -> RatSem) -> Bool
\end{code}
And go our merry way for most cases:
\begin{code}
eval formula ratEnv = case formula of
  (PName n args) -> eval0 n (map (flip evalRat ratEnv) args)
  Equal a b -> evalRat a ratEnv == evalRat b ratEnv
  And p q -> eval p ratEnv && eval p ratEnv
  Or  p q -> eval p ratEnv || eval p ratEnv
\end{code}
etc.

However, as soon as we encounter quantifiers, we have a problem. To
evaluate |EXISTS x p| (at least in certain contexts) we may need to
evaluate |p| for each possible value of |x|. But, unfortunately, there
are infinitely many such possible values, and so we can never know if
the formula is a tautology.\footnote{FOL experts will scoff at this
  view, because they routinely use much more sophisticated methods of
  evaluation, which handle quantifiers in completely different ways.
  Their methods are even able to identify tautologies as
  such. However, even such methods are not guaranteed to terminate on
  formulas which are not tautologies. Therefore, as long as an
  even-very-advanced FOL tautology-checker is running, there is no way
  to know how close it is to confirming if the formula at hand is a
  tautology or not. This is not a technical limitation, but rather a
  fundamental one, which boils down to the presence of quantifier over
  an infinite domain.} So, if we were to try to run the evaluator, it
would not terminate. Hence, the best that we can ever do is, given a
hand-written proof of the formula, check if the proof is
valid. Fortunately, we have already studied the notion of proof in the
section on propositional logic, and it only needs to be extended to
support quantifiers.

\subsubsection{Universal quantification}
%
Universal quantification (Forall or ∀) can be seen as a generalisation of |And|.
%
To see this, we can begin by generalising the binary operator |And| to an |n|-ary version:
|Andn|.
%
To prove |Andn(A1,A2, ..., An)| we need a proof of each |Ai|.
%
Thus we could define |Andn(A1,A2, ..., An) = A1 & A2 & ... & An| where
|&| is the infix version of binary |And|.
%
The next step is to require the formulas |Ai| to be of the same form, ie. the result of applying a constant function |A| to
the individual |i|.
%
And, we can think of the variable |i| ranging over the full set of individuals |i1|, |i2|, \ldots
%
Then the final step is to introduce the notation |Forall i A(i)| for
|A(i1) & A(i2) & ... |.
%

Now, a proof of |Forall x A(x)| should in some way contain a proof of
|A(x)| for every possible |x|.
%
For the binary |And| we simply provide the two proofs, but in the
infinite case, we need an infinite collection of proofs.
%
To do so, a possible procedure is to introduce a fresh (meaning that we know nothing about this new term) constant term |a| and
prove |A(a)|.
%
Intuitively, if we can show |A(a)| without knowing anything about |a|,
we have proved |Forall x A(x)|.
%
Another way to view this is to say that a proof of |Forall x (P x)| is
a function |f| from individuals to proofs such that |f t| is a proof of |P
t| for each term |t|.
%

So we can now extend our type for proofs: the introduction rule for
universal quantification is \(\frac{A(x) \quad \text{\(x\) fresh
  }}{∀x. A(x)}\).  The corresponding constructor can be
|UniversalIntro :: (RatSem -> Proof) -> Proof|.

% TODO: A simple example might also be a good idea, where we end up
% with a function f where f t is a proof of P t for all terms t.
%

Note that the scoping rule for |Forall x b| is similar to the rule
for a function definition, |f x = b|, and for anonymous functions, |\x
-> b|.
%
Just as in those cases we say that the variable |x| is \emph{bound} in
|b| and that the \emph{scope} of the variable binding extends until
the end of |b| (but not further).
%

One common source of confusion in mathematical (and other semi-formal)
texts is that variable binding sometimes is implicit.
%
A typical example is the notation for equations: for instance |x^2 + 2*x + 1 == 0| usually means
roughly |Exists x (x^2 + 2*x + 1 == 0)|.
%
We write ``roughly'' here because the scope of |x| very often extends
to some text after the equation where something more is said about the
solution |x|.

Let us now consider the elimination rule for universal
quantification. The idea here is that if |A(x)| holds for every
abstract individual |x|, then it also holds for any concrete
individual |a|: \(\frac{∀x. A(x)}{A(a)}\). As for |And| we had to
provide the other argument to recover |p `And` q|, here we have to be
able reconstruct the general form |A(x)| --- indeed, it is not simply
a matter of substituting |x| for |a|, because there can be several
occurences of |a| in the formula to prove. So, in fact, the proof
constructor must contain the general form |A(x)|, for example as a
function from individuals: |UniversalElim :: (RatSem -> Prop) -> Proof ->
Proof|.

Let us sketch the proof-checker cases corresponding to universal
quantification:
\begin{spec}
proofChecker (UniversalIntro f a) (Forall x p) = proofChecker (f a) (subst x a p)
  where a = freshFor [a, Forall x p]
proofChecker (UniversalElim f t) p = checkUnify (f x) p && proofChecker t (Forall x (f x))
  where x = freshFor [f x, p]
\end{spec}
The introduction rule uses a new concept: |subst x a p|, which
replaces the variable |x| by |a| in |p|, but otherwise follows closely
our informal explanation. The eliminator uses |checkUnify| which
verifies that |f x| is indeed a generalisation of the formula to
prove, |p|. Finally we need a way to introduce fresh variables
|freshFor|, which conjures up a variable occuring nowhere (in its
argument).

\subsubsection{Existential quantification}

%
We have already seen how the universal quantifier can be seen as a
generalisation of |And| and in the same way we can see the existential (∃)
quantifier as a generalisation of |Or|.
%

First we generalise the binary |Or| to an |n|-ary |Orn|.
%
To prove |Orn A1 A2 ... An| it is enough (and necessary) to find one
|i| for which we can prove |Ai|.
%
As before we then take the step from a family of formulas |Ai| to a single
unary predicate |A| expressing the formulas |A(i)| for the (term)
variable |i|.
%
Then the final step is to take the disjunction of this infinite set of formulas
to obtain |Exists i (A i)|.

The elimination and introduction rules for existential quantification are:

\[
  \frac{P(a)}{∃i. P(i)}\hspace{10em}\frac{\text{for every \(a\),~}{P(a) → R}  \quad \quad ∃i. A(i)}{R}
\]
The introduction rule says that to prove the existential
quantification, we only need exhibit one witness ($a$) and one proof
for that member of set of individuals. For binary |Or| the ``family''
only had two members, one labelled |Left| and |Right|, and we
effectively had one introduction rule for each.
%
Here, for the generalisation of |Or|, we have unified the two rules
into one with an added parameter |a| corresponding to the label which
indicates the family member.

In the other direction, if we look at the binary elimination rule,
we see the need for two arguments to be sure of how to prove the
implication for any family member of the binary |Or|.
%
\begin{spec}
orElim      :: Or p q -> (p `Implies` r) -> (q `Implies` r) -> r
\end{spec}
%
The generalisation unifies these two to one family of arguments.
%
If we can prove |R| for each member of the family, we can be sure to
prove |R| when we encounter some family member:

The constructors for proofs can be |ExistsIntro
:: RatSem -> Proof -> Proof| and |ExistsElim :: (RatSem -> Prop) -> Proof ->
Proof|. In this case we'd have |i| as the
first argument of |ExistsProof| and a proof of |A(i)| as its second
argument.

\begin{exercise}
  Sketch the |proofChecker| cases for universal quantification.
\end{exercise}

\subsubsection{Typed quantification}
\label{sec:TypedQuant}

So far, we have considered quantification always as over the full set of
individuals, but it is often convenient to
quantify over a subset with a certain property (like all even numbers,
or all non-empty sets).

Even though it is not usually considered as strictly part of FOL, it
does not fundamentally change its character if we extended it with
several types (or sorts) of individuals (one speaks of
``multi-sorted'' FOL).

In such a variant, the quantifiers look like |∀(x:S). P(x)| and
|∃(x:S). P(x)|.

Indeed, if a type (a set) |S| of terms can be described as those that
satisfy the unary predicate |T| we can understand |∀(x:T). P(x)| as a
shorthand for |∀x. T(x) `Implies` P(x)|. Likewise we can understand
|∃(x:T). P(x)| as a shorthand for |∃x. T(x) `And` P(x)|.

As hinted at in the previous chapters, we find that writing types
explicitly can greatly help understanding, and we won't refrain from
writing down types in quantifiers in FOL formulas.

\begin{exercise}
  Prove that the de Morgan dual of typed universal quantification is
  the typed existential quantification, using the above translation to
  untyped quantification.

%TODO: include somewhere as a solution.
%   not (Exists (x:T) (P x))          = {- Def. of typed quantification -}
%   not (Exists x (T x & P x))        = {- de-Morgan for existential    -}
%   Forall x (not (T x & P x))        = {- de-Morgan for and            -}
%   Forall x (not (T x) | not (P x))  = {- |(A => B)  ==  (not A | B)|  -}
%   Forall x (T x => not (P x))       = {- Def. of typed quantification -}
%   Forall (x:T) (not (P x))

\end{exercise}

\subsubsection{Curry-Howard for quantification over individuals}

We can try and draw parrellels with an hypothetical programming
lanaguage corresponding to FOL.

In such a programming language, we expect to be able to encode proof rules
as follows:
\begin{spec}
  allIntro   :: ((a : Individual) -> P a) -> (Forall x (P x))
  allElim    :: (Forall x (P x)) -> ((a : Individual) -> P a)
  existIntro :: (a : Individual) -> P a -> Exists x (P x)
  ExistsElim :: ((a:Individual) -> P a `Implies` R) -> (Exists x (P x)) `Implies` R
\end{spec}

(We must write the above using a \emph{dependent} function type |(a:A) → B|, see below.).

Taking the intuitionistic version of FOL (with the same treatment of
negation as for propositional logic), we additionally expect to be
able to represent proofs of quantifiers, directly. That is:
\begin{quote}
|(t, bt)| is a program of type |Exists x (P(x))| if |bt| is has type |P(t)|.

|f| is a program of type |Forall x (P(x))| if |f t| is has type |P(t)| for all |t|.
\end{quote}

Unfortunately, in its 2010 standard, Haskell does not provide the
equivalent of quantification over individuals. Therefore, one would
have to use a different tool than Haskell as a proof assistant for
(intuitionistic) FOL.  the quantification that Haskell provides
|forall a. ...| is \emph{over types} rather than individuals.
\footnote{There is ongoing progress in this direction, but we find the
  current state too clunky to be worthy of basing our development on
  it.}  What we'd need is: 1. a type corresponding to universal
quantification, the dependent function type |(a:A) → B|, and 2. a type
corresponding to |Exists (x:A) (P x)|, the dependent pair |(x:A,
P(x))|.


We can recommend the language Agda (which provides even more forms of
quantification), however, in order to avoid a multiplicity of tools
and potentially an excessive emphasis on proof formalism, we will
refrain to formalise any proof as Agda programs in the
Remainder. Rather, in the rest of the chapter, we will illustrate the
logical principles seen so far by examples.


\subsubsection{\extraMaterial More general code for first order languages}
\jp{we actually NEVER used the specialisation to rationals. Why not simply use this one directly in the main text?}

It is possible to make one generic implementation of |FOL| which can
be specialised to any first order language.


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
