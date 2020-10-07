\subsection{First Order Logic}

When formally proving properties in FOL we should use the introduction
and elimination rules.
%
The propositional fragment of FOL is given by the rules for ∧, →, ⟷, ¬,
∨.

%
%TODO: include top-level explanation: Adds term variables and functions, predicate symbols and quantifiers (sv: kvantorer).
Our second DSL is that of \emph{First Order Logic\lnOnly{\footnote{Swedish: Första ordningens logik = predikatlogik}}},
or FOL for short, and also known as Predicate Logic.
%
This language has two datatypes: \emph{propositions}, and \emph{terms} (which are new).
%
A \emph{term} is either a (term) \emph{variable} (like |x|, |y|, |z|),
or the application of a \emph{function symbol} (like |f|, |g|) to a
suitable number of terms.
%
If we have the function symbols |f| of arity |2| and |g| of arity |3|
we can form terms like |f(x,x)|, |g(y,z,z)|, |g(x,y,f(x,y))|, etc.
%
The actual function symbols are usually domain-specific --- we can use
rational number expressions as an example.
%
In this case we can model the terms as a datatype:
%
%{
%let rat = True
%include FOLRat.lhs
%let rat = False
%}
%
The above introduces variables %(with the constructor |RV|)
and three function symbols:
%
|FromI| of arity |1|, |RPlus|, |RDiv| of arity |2|.

The propositions from |PropCalc| are extended so that they can refer
to terms.
%
We will normally refer to a |FOL| proposition as a \emph{formula}.
%
The names from the propositional calculus are generalised to
\emph{predicate symbols} of different arity.
%
The predicate symbols can only be applied to terms, not to other
predicate symbols or formulas.
%
If we have the predicate symbols |New| of arity |0|, |Pos| of arity |1|
and |Less| of arity |2| we can form \emph{formulas} like |New|, |Pos(x)|,
|Less(f(x,x),y)|, etc.
%
Note that we have two separate layers:
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

An example FOL formula:
%
\begin{spec}
  Forall x (Pos(x) => (Exists y (Less(f(x,x),y))))
\end{spec}
%
Note that FOL can only quantify over \emph{term} variables, not over
predicates.
%
(Second order logic and higher order logic allow quantification over
predicates.)

Another example: a formula stating that the function symbol |plus| is
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
%
%{
%let fol = True
%include FOLRat.lhs
%let fol = False
%}
\paragraph{Undecidability}

Setting us up for failure, let us attempt to write |evalPC| for FOL, as we did for propositional logic.

Truth tables are replaced by truth tables for each predicate/argument
combinations (|PSym -> [RatT] -> Bool|). (TODO: what is |VarT -> RatT|?)

We
would use the following type:

\begin{spec}
evalFOL :: FOL -> (VarT -> RatT) -> (PSym -> [RatT] -> Bool) -> Bool
\end{spec}
And go our merry way for most cases:
\begin{spec}
evalFOL formula ratEnv predEnv = case formula of
  (PName n args) -> predEnv n args
  Equal a b -> a == b
  And p q = evalFOL p ratEnv predEnv && evalFOL p ratEnv predEnv
  Or  p q = evalFOL p ratEnv predEnv || evalFOL p ratEnv predEnv
\end{spec}
etc.

however, as soon as we encounter quantifiers, we have a problem. To
evaluate |EXISTS x p| (in certain contexts at least) we may need to evaluate |p| for each possible value
of |x|. But, unfortunately, there are infinitely many such possible
values, and so we can never know if the formula is a
tautology.\footnote{
FOL experts will scoff at this view, because they routinely use much more sophisticated methods of evaluation, which handle quantifiers in completely different ways.
Their methods are also able to identify tautologies as such. However, even such methods are not guaranteed to terminate on formulas which are not tautologies. Therefore, as long as
an even-very-advanced FOL checker is running, there is no way to know how close it is to confirming if the formula at hand is a tautology or not. This is not a technical limitation,
but rather a fundamental one, which boils down to the presence of quantifiers in FOL.
} So, if we were to try to run the
evaluator, it would not terminate. Hence, the best that we can ever
do is, given a hand-written proof of the formula, check if the proof is valid.

Hence, |evalFOL| would have a type such as the following:

\begin{spec}
evalFOL :: FOL -> FOLProof -> (VarT -> RatT) -> (PSym -> [RatT] -> Bool) -> Bool
\end{spec}
We then turn our attention to the structure of the |FOLProof| type.


\paragraph{Quantifiers: meaning, proof and syntax.}
%
``Forall''-quantification can be seen as a generalisation of |And|.
%
To see this, we can begin by generalising the binary operator |And| to an |n|-ary version:
|Andn|.
%
To prove |Andn(A1,A2, ..., An)| we need a proof of each |Ai|.
%
Thus we could define |Andn(A1,A2, ..., An) = A1 & A2 & ... & An| where
|&| is the infix version of binary |And|.
%
The next step is to note that the formulas |Ai| can be generalised to
|A(i)|\jp{lhs2tex typesets this strangely. Perhaps simple math mode works better?} where |i| is a term variable and |A| is a unary predicate
symbol.
%
We can think of |i| ranging over an infinite collection of constant
terms |i1|, |i2|, \ldots
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
To do so, a possible procedure is to introduce a fresh constant term |a| and
prove |A(a)|.
%
Intuitively, if we can show |A(a)| without knowing anything about |a|,
we have proved |Forall x A(x)|.
%
Another way to view this is to say that a proof of |Forall x (P x)| is
a function |f| from terms to proofs such that |f t| is a proof of |P
t| for each term |t|.\jp{This is actually very different, because the term structure is available to the proof. I am wondering if we're not misleading here. Perhaps not talk about FOL at all? Or much less? And focus on intuitionistic logics as early as possible.}
%
Continuing to build our syntax for proofs, we could write: 

|ForallProof :: (RatT -> FOLProof) -> FOLProof|

% TODO: A simple example might also be a good idea, where we end up
% with a function f where f t is a proof of P t for all terms t.
%

Note that the syntactic rule for |Forall x b| is similar to the rule
for a function definition, |f x = b|, and for anonymous functions, |\x
-> b|.
%
Just as in those cases we say that the variable |x| is \emph{bound} in
|b| and that the \emph{scope} of the variable binding extends until
the end of |b| (but not further).
%
The scoping of |x| in |Exists x b| is the same as in |Forall x b|.

One common source of confusion in mathematical (and other semi-formal)
texts is that variable binding sometimes is implicit.
%
A typical example is the notation for equations: for instance |x^2 + 2*x + 1 == 0| usually means
roughly |Exists x (x^2 + 2*x + 1 == 0)|.
%
We write ``roughly'' here because the scope of |x| very often extends
to some text after the equation where something more is said about the
solution |x|.

TODO: Echoes:
 
As we saw earlier, a similar rule holds for the ``forall'' quantifier:
%
a function |f| from terms |t| to proofs of |P t| is a proof of |Forall
x (P x)|.

As we saw in \refSec{sec:TypedQuant}, a very common kind of
formula is ``typed quantification'':
%
if a type (a set) |S| of terms can be described as those that satisfy
the unary predicate |T| we can introduce the short-hand notation
%
\begin{spec}
  (Forall (x:T) (P x)) = (Forall x (T x => P x))
\end{spec}
%
A proof of this is a two-argument function |p| which takes a term |t|
and a proof of |T t| to a proof of |P t|.


\subsubsection{Existential quantification}

%
We have already seen how the ``forall'' quantifier can be seen as a
generalisation of |And| and in the same way we can see the ``exists''
quantifier as a generalisation of |Or|.
%

First we generalise the binary |Or| to an |n|-ary |Orn|.
%
To prove |Orn A1 A2 ... An| it is enough (and necessary) to find one
|i| for which we can prove |Ai|.
%
As before we then take the step from a family of formulas |Ai| to one
unary predicate |A| expressing the formulas |A(i)| for the term
variable |i|.
%
Then the final step is to ``or'' all these formulas to obtain |Exists
i (A i)|.
%
Continuing to build our syntax for proofs, we may write: |ExistsProof
:: RatT -> FOLProof -> FOLProof|. In this case we'd have |i| as the
first argument of |ExistsProof| and a proof of |A(i)| as its second
argument.

At this point it is good to sum up and compare the two quantifiers and
how to prove them:

\begin{quote}
|(t, bt)| is a proof of |Exists x (P(x))| if |bt| is a proof of |P(t)|.

|f| is a proof of |Forall x (P(x))| if |f t| is a proof of |P(t)| for all |t|.
\end{quote}


%TODO At this stage we're ready to complete our defintion of the proof language and the proof checker.


\paragraph{Typed quantification}
%
\jp{What is the purpose of this paragraph?}
In each instance of FOL\jp{Have we talked about several instances of FOL?}, quantification is always over the full set of
terms (the ``universe of discourse''), but it is often convenient to
quantify over a subset with a certain property (like all even numbers,
or all non-empty sets).
%
We will use a notation we can call ``typed quantification'' as a
short-hand notation for the full quantification in combination with a
restriction to the subset.
%
For existential and universal quantification these are the definitions:

% (Exists (x:T) (P x)) =~= (Exists x (And (T x) (P x)))
\begin{spec}
(Exists (x:T) (P x)) =~= (Exists x (T x & P x))
(Forall (x:T) (P x)) =~= (Forall x (T x => P x))
\end{spec}
% (Forall (x:T) (P x)) =~= (Forall x (Implies (T x) (P x)))
\label{sec:TypedQuant}%
%
Note that we silently convert between |T| seen as a type (in |x : T|
on the left) and |T| seen as a unary predicate on terms (in |T x| on
the right)\jp{But have we ever defined what a FOL type is? We also have Haskell-level types and so there is a big potential for confusion here. There is also the extremely tempting prospect of representing formulas as Haskell types and proofs as Haskell terms, directly.}.

%**TODO make it an actual exercise
A good exercise is to work out the rules for ``pushing negation
through'' typed quantification, from the corresponding rules for full
quantification.

%TODO: include somewhere as a solution.
%   not (Exists (x:T) (P x))          = {- Def. of typed quantification -}
%   not (Exists x (T x & P x))        = {- de-Morgan for existential    -}
%   Forall x (not (T x & P x))        = {- de-Morgan for and            -}
%   Forall x (not (T x) | not (P x))  = {- |(A => B)  ==  (not A | B)|  -}
%   Forall x (T x => not (P x))       = {- Def. of typed quantification -}
%   Forall (x:T) (not (P x))

\subsubsection{Quantifiers as function types}

We can express the universal quantification laws as:
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
every year but for proper dependently typed programming we recommend
the language Agda.\jp{make this a footnote?}

\subsubsection{Existential quantification as a pair type}

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

In the other direction, if we look at the binary elimination rule,
we see the need for two arguments to be sure of how to prove the
implication for any family member of the binary |Or|.
%
\begin{spec}
orElim    :  (P=>R) -> (Q=>R) -> ((P|Q) => R)
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

\subsubsection{\extraMaterial More general code for first order languages}

It is possible to make one generic implementation of |FOL| which can
be specialised to any first order language.
%

% TODO: perhaps add some explanatory text

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
