\section{Logic and calculational proofs}
\label{sec:logic}

In this chapter, we continue to exercise our skill of organize areas
of mathematics in DSL terms. We do so on languages of propositions and
proofs. Thus, at the same time we will develop adequate notations for
mathematical foundations and perform calculational proofs.
%
There will be a fair bit of theory: introducing propositional and
first order logic, but also applications to mathematics: prime
numbers, (ir)rationals, limit points, limits, etc. and some
Haskell concepts.

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module DSLsofMath.W02 where
\end{code}
%if False
\begin{code}
import qualified DSLsofMath.AbstractFOL as FOL
\end{code}
%endif
\subsection{Propositional Calculus}
%
The main topic of this chapter is logic, and proofs.
%
Our first DSL for this chapter is thus the language of \emph{propositional
  calculus} (or propositional logic), modelling simple propositions with the usual
combinators for and, or, implies, etc.
%
\lnOnly{(The Swedish translation is ``satslogik'' and some more Swe-Eng
translations are collected on the GitHub page of these lecture notes\footnote{\url{https://github.com/DSLsofMath/DSLsofMath/wiki/Translations-for-mathematical-terms}}.)}
%
Some concrete syntactic constructs are collected in
Table~\ref{tab:PropCalc} where each row lists common synonyms and their arity (number of arguments).
%
\begin{table}[htbp]
  \centering
\begin{tabular}{lccl}
   |False|    & $\bot$         & F      & nullary
\\ |True|     & $\top$         & T      & nullary
\\ |Not|      & $\neg$         & |~|    & unary
\\ |And|      & $\wedge$       & |&|    & binary
\\ |Or|       & $\vee$         & |bar|  & binary
\\ |Implies|  & $\supset$      & |=>|   & binary
\end{tabular}
\caption{Syntax for propositions. In addition, |a|, |b|, |c|, \ldots are used as names of propositions}
\label{tab:PropCalc}
\end{table}

Some example propositions: \(|p1| = a \wedge (\neg a)\),
\(|p2| = a \Rightarrow b\), \(|p3| = a \vee (\neg a)\),
\(|p4| = (a \wedge b) \Rightarrow (b \wedge a)\).
%
The names |a|, |b|, |c|, \ldots are ``propositional variables'':
variables: they can be substituted for any proposition.  However we
will soon add another kind of variables (and quantification over them)
to the calculus --- so we keep calling them ``names'' to avoid mixing
them up.

Since names stand for propositions, if we assign all combinations of
truth values for the names, we can compute a truth value of the whole
proposition.
%
In our examples, |p1| is always false, |p2| is mixed and |p3| and |p4|
are always true.

%
Just as we did with simple arithmetic, and with complex number
expressions in \cref{sec:DSLComplex}, we can model the abstract
syntax of propositions as a datatype:
\begin{code}
data PropCalc  =  Con      Bool
               |  Not      PropCalc
               |  And      PropCalc  PropCalc
               |  Or       PropCalc  PropCalc
               |  Implies  PropCalc  PropCalc
               |  Name     Name
type Name = String
\end{code}
%
The example expressions can then be expressed as
%
\begin{code}
p1 = And (Name "a") (Not (Name "a"))
p2 = Implies (Name "a") (Name "b")
p3 = Or (Name "a") (Not (Name "a"))
p4 = Implies (And a b) (And b a)
  where a = Name "a"; b = Name "b"
\end{code}
%
We can write an evaluator which, given an environment (an assignment of names to truth values), takes
a |PropCalc| term to its truth value:
%
\begin{code}
evalPC :: (Name -> Bool) -> (PropCalc -> Bool)
evalPC env = error "Exercise" -- see \ref{par:SETandPRED} for a similar function
\end{code}
%
The function |evalPC| translates from the syntactic to the semantic
domain.
%
(The evaluation function for a DSL describing a logic is often called
|check| instead of |eval| but for consistency we stick to |eval|.)
%
Here |PropCalc| is the (abstract) \emph{syntax} of the language of
propositional calculus and |Bool| is the \emph{semantic
  domain}.
%
\footnote{Alternatively, we can view |(Name -> Bool) -> Bool| as the semantic
domain of |PropCalc|.
%
A value of this type is a mapping from an environment to |Bool|.}

%if False
%*TODO: perhaps show this code for those interested
\begin{code}
type S = (Name -> Bool) -> Bool
impS, andS, orS  :: S -> S -> S
notS   :: S -> S
nameS  :: Name -> S
nameS n env = env n
impS f g env = f env `implies` g env
andS f g env = f env && g env
orS  f g env = f env || g env
notS f = not . f
implies :: Bool -> Bool -> Bool
implies False  _  =  True
implies _      p  =  p

evalPC' :: PropCalc -> S
evalPC' (Implies p q)  = impS (evalPC' p) (evalPC' q)
evalPC' (And p q)  = andS (evalPC' p) (evalPC' q)
evalPC' (Or  p q)  = orS  (evalPC' p) (evalPC' q)
evalPC' (Not p)    = notS (evalPC' p)
evalPC' (Name n)   = nameS n
\end{code}
%endif

%
\begin{wrapfigure}{R}{0.17\textwidth}
  \centering
\begin{tabular}{||l||l||}
    \hline   a & t
  \\\hline   F & T
  \\         T & T
  \\\hline
\end{tabular}
\caption{|F => a|}
\label{fig:F2a}
\end{wrapfigure}
%
As a first example of a truth table, consider the proposition |F => a|
which we call |t| here.
%
% We will use the shorter notation with just |T| for true and |F| for
% false.
%
The truth table semantics of |t| is usually drawn as in~\refFig{fig:F2a}:
%
one column for the name |a| listing all combinations of |T| and |F|,
and one column for the result of evaluating the expression.
%
This table shows that no matter what value assignment we try for the
only variable |a|, the semantic value is |T = True|.
%
Thus the whole expression could be simplified to just |T| without
changing its semantics.

\begin{wrapfigure}{R}{0.35\textwidth}
  \centering
\begin{tabular}{||lllllll||}
    \hline   |a| & |&| & |b| & |=>| & |b| & |&| & |a|
  \\\hline    F  &  F  &  F  &  T   &  F  &  F  &  F
  \\          F  &  F  &  T  &  T   &  T  &  F  &  F
  \\          T  &  F  &  F  &  T   &  F  &  F  &  T
  \\          T  &  T  &  T  &  T   &  T  &  T  &  T
  \\\hline
\end{tabular}
\caption{\(|p4| = (a \wedge b) \Rightarrow (b \wedge a)\).
}
\label{fig:abswap}
\end{wrapfigure}
%
If we continue with the example |p4| from above we have two names |a|
and |b| which together can have any of four combinations of true and
false.
%
After the name-columns are filled, we fill in the rest of the table
one operation (column) at a time (see \refFig{fig:abswap}).
%
The |&| columns become |F F F T| and finally the |=>| column (the
output) becomes true everywhere.

A proposition whose truth table output is constantly true is called a
\emph{tautology}.
%
Thus both |t| and |p4| are tautologies. So, we can write a tautology-tester as follows:
\begin{spec}
isTautology :: PropCalc -> Bool
isTautology p = and [evalPC truthTable p | t <- truthTables (freeNames p)]

truthTables (n:ns) n' = [if n == n' then b else t | b <- [True,False], t <- truthTables ns] 
\end{spec}
%
Truth table verification is only viable for propositions with few
names because of the exponential growth in the number of cases to
check: we get $2^n$ different |truthTables| for |n| names.
%

% *TODO: formulate more clearly as an exercise
\begin{exercise}
At this point it is good to implement a few utility functions on
|PropCalc|: list the names used in a term (|freeNames|), simplify to disjunctive
normal form, simplify to conjunctive normal form, etc.
(Conjunctive normal form: allow only |And|, |Or|, |Not|, |Name| in that
order in the term.)
\end{exercise}

Even if there are (much) better algorithms to evaluate truth values,
the best algorithms remain exponential in the number of variables. We
will not go this route. Rather, we can introduce the notion of \emph{proof}

\subsubsection{Proofs for Propositional Logic}

Given the a |PropCalc| proposition |p| and a proof |t| (represented as
an element of another type |PropCalcProof|), we can write a function that checks
that |t| is a valid proof of |p|.

\begin{code}
checkProof :: Proof -> PropCalc -> Bool
\end{code}

We now have to figure out what consitutes proofs.

To prove |And P Q|, one needs simultaneously of |P| and a proof of
|Q|. (In logic texts, one will often find
\[ \frac{P \quad Q}{P \and Q} \] to represent this rule. This is called the \emph{introduction rule for (∧)})
(For the proof to be complete, one still needs to provide a full proof of |P| and another for |Q|.)

Therefore, in Haskell, can represent this rule with the a proof-term constructor:
\begin{spec}
AndIntro :: Proof -> Proof -> Proof
\end{spec}

and, a case of |checkProof| will look like this:

\begin{spec}
checkProof (AndIntro t u) (And p q) = checkProof t p && checkProof u q
\end{spec}

To prove |Or P Q|, we need either a proof of |P| or proof of |Q| --- but
we need to know which side (|Left| for |p| or |Right| for |q|) we
refer to.  Therefore, we need a proof-term constructor:

\begin{spec}
OrIntro :: Either Proof Proof -> Proof
\end{spec}

To deal with negation, one approach is to push it down using de Morgan
laws until we reach names. Another approach is to use the following rule:
\[ \frac{P → Q \quad P → ¬Q}{¬P} \], which we can represent by the
|NegIntro :: PropCalc -> Proof -> Proof -> Proof| constructor. Here, we have an additional |PropCalc| argument,
which gives the |Q| formula. 


In addition to introduction rules (where the connective appears as
conclusion), we also have elimination rules (where the connective
appears as premiss). For conjuction, we have both $\frac{P ∧ Q}{P}$ and
$\frac{P ∧ Q}{Q}$. Because we have inductive proofs (described from the bottom up),
we have the additional difficulty that these rules conjure-up a new
proposition, $Q$. So we can represent them respectively by |AndElim1 :: Proof -> PropCalc -> Proof| (and |AndElim2| symmetrically).
\jp{rename |PropCalc| to prop?}

Our eliminator of disjunction is $\frac {P ∨ Q \quad P → R \quad Q → R} R$.\jp{finish}
Our eliminator for negation is $\frac {¬ ¬ P} P$

We can then write our proof checker as follows:

\begin{code}
checkProof TruthIntro (Con True) = True
checkProof (AndIntro t u) (And p q) = checkProof t p && checkProof u q
checkProof (OrIntro (Left t)) (Or p q) = checkProof t p
checkProof (OrIntro (Right t)) (Or p q) = checkProof t q
checkProof (NotIntro t u q) (Not p) = checkProof t (p `Implies` q) && checkProof u (p `Implies` Not q)
checkProof (AndElimL t q) p = checkProof t (p `And` q)
checkProof (AndElimR t p) q = checkProof t (p `And` q)
checkProof (OrElim t u v p q) r = checkProof t (p `Implies` r) && checkProof u (q `Implies` r) && checkProof v (Or p q)
checkProof (NotElim t) p = checkProof t (Not (Not p))
checkProof (FalseElim t) p = checkProof t (Con False)
\end{code}

Anything else is an incorrect proof.

\paragraph{Implication, hypothetical derivations, contexts}

For |Implies|, we can use the so-called material implication
definition which we invoked earlier in truth tables. It means to define |Implies a b =
(Not a) `Or` b|. However this choice does not bring any new insight.

Another possibility is a rule which can be written like so:\[\frac{\begin{array}{c}P \\ \vdots \\ Q \end{array}}{P → Q}\].
Such a notation can however be terribly confusing. We were used to the fact that proofs above the line had to be continued. So what can the dots possibly mean?
The intent is that, to prove $P → Q$, it suffices to prove $Q$, but one is allowed to use $P$ as an assumption in the proof of $Q$.

We can use our DSL to make this formal, by adding a constructor for implication introduction: |ImplyIntro :: (Proof -> Proof) -> Proof|.
The fact that the premiss can depend on the assumption |Q| is represented by a function whose parameter is the proof of |Q| in question.

The eliminator for implication, known as \textit{modus ponens} is \(\frac{P → Q \quad P} Q\). We formalise it as |ImplyElim :: Proof -> Proof -> PropCalc -> Proof|
(The proposition |P| is a not given by the conclusion). We complete our proof checker as follows:

\begin{code}
checkProof Assumption p = True
checkProof (ImplyIntro f) (p `Implies` q) = checkProof (f Assumption) q
checkProof (ImplyElim t u p) q = checkProof t (q `Implies` p) && checkProof u q
checkProof _ _ = False -- incorrect proof
\end{code}
with the DSL for proofs being:

\begin{code}
data Proof  =  Assumption
            |  TruthIntro
            |  FalseElim Proof
            |  AndIntro Proof Proof
            |  AndElimL Proof PropCalc
            |  AndElimR Proof PropCalc
            |  OrIntro (Either Proof Proof)
            |  OrElim Proof Proof Proof PropCalc PropCalc
            |  NotIntro Proof Proof PropCalc
            |  NotElim Proof
            |  ImplyIntro (Proof -> Proof)
            |  ImplyElim  Proof Proof PropCalc
\end{code}


Aside.
The |Assumption| constructor may make the reader somewhat uneasy: how come that we can simply assume anything? The intent is that this
constructor is private to the |checkProof| function (or module). No user-defined proof can use it. The most worried readers
can also define the following version of |checkProof|, which uses an extra context to check that assumption have been rightfully introduced earlier.
\footnote{This kind of presentation of the checker matches well the sequent calculus presentation of the proof system.}
 
\begin{spec}
checkProof :: Context -> Proof -> PropCalc -> Bool
checkProof ctx (ImplyIntro t) (p `Implies` q) = checkProof' (p:ctx) t q
checkProof ctx Assumption p = p `elem` ctx
\end{spec}

\paragraph{Example proof}

\begin{code}
conjunctionCommutative = (a `And` b ) `Implies` (b `And` a)
  where a = Name "a"; b = Name "b"

conjunctionCommutativeProof =
  ImplyIntro (\aAndb ->
  AndIntro  (AndElimR aAndb (Name "a"))
            (AndElimL aAndb (Name "b")) )

\end{code}

checkProof conjunctionCommutativeProof conjunctionCommutative == True

\paragraph{Haskell as a proof assistant}

What if we could do this:

\begin{code}
type ConjunctionCommutative = forall p q. (p `And` q) `Implies` (q `And` p)
conjunctionCommutativeProof' :: ConjunctionCommutative
conjunctionCommutativeProof' =
   implyIntro (\aAndb ->
   andIntro  (andElimR aAndb)
             (andElimL aAndb))
\end{code}

Instead of writing propositions, we write types.

We would not have to run (or for that matter write) a proof checker: the haskell type-checker does the work for us.

Well we can do it!

First, we can use the type-checker to encode the proof rules as programs:
\begin{code}
implyIntro :: (p -> q) -> (p `Implies` q)
implyElim :: p -> (p `Implies` q) -> q

andIntro :: p -> q -> And p q
andElimL :: a `And` b -> a
andElimR :: a `And` b -> b

orIntro :: Either a b -> Or a b
orElim :: Or p q -> (p `Implies` r) -> (q `Implies` r) -> r

falseElim :: False -> p -- ex falso quod libet
\end{code}


But also, because the meaning of a proof of conjuction is exactly a pair of proofs, etc.

Conjunction is represented as pairs; that is, if |p : P| and |q : Q| then |(p,q) : And P Q|.


Similarly, disjuction becomes |Either|.
%
If |p : P| then |Left p : Or P Q| and if |q : Q| then |Right q : Or P
Q|.
%
In this way we can build up what is called ``proof terms'' for a large
fragment of logic.
%
It turns out that each such proof term is basically a program in a
functional programming language, and that the formula a certain term
proves is the type for the program.

\begin{code}
type Implies p q = p -> q
implyElim a f = f a
implyIntro f x = f x

type And p q = (p,q)
andIntro t u = (t,u)
andElimL = fst
andElimR = snd

type Or a b = Either a b
orIntro x = x
orElim (Left   t) u _ = u t
orElim (Right  t) _ v = v t

type Truth = ()
truthIntro = ()

data False
falseElim x = case x of {}
\end{code}


(Attn. diverging proofs!)

\subsection{Intuitionistic Propositional Logic}

\begin{code}
type Not a = a `Implies` False

notElim        ::  Not (Not p) -> p
notElim = error "not possible as such in Haskell"

notIntro       ::  (p -> And q (Not q)) -> Not p
notIntro = error "TODO"
\end{code}
  
\subsection{First Order Logic}
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

\subsection{An aside: Pure set theory}
\jp{If this is an aside then it's an alternative to FOL? What is the purpose of this aside? Move it?}

One way to build mathematics from the ground up is to start from pure
set theory and define all concepts by translation to sets.
%
We will only work with this as a mathematical domain to study, not as
``the right way'' of doing mathematics (there are other ways\jp{In fact the DSL approach that we advocate for is not quite fitting that model.}).
%
In this section we keep the predicate part of the version of |FOL|
from the previous section, but we replace the term language |RatT|
with pure (untyped) set theory.
%

The core of the language of pure set theory is captured by four
function symbols.
%
We have a nullary function symbol |{}| for the empty set (sometimes
written $\emptyset$) and a unary function symbol |S| for the function
that builds a singleton set from an ``element''.
%
All non-variable terms so far are |{}|, |S {}|, |S (S {})|, \ldots
%
The first set is empty but all the others are (different) one-element sets.
%

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

At this point it can be a good exercise to enumerate a few sets of
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
The function |vN| is explored in more detail in the first assignment
of the DSLsofMath course.


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

\subsection{Back to quantifiers}

After this detour through untyped set land, let us get back to the
most powerful concept of FOL: the quantifiers.
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

\paragraph{Curry--Howard}
%
\jp{Intuitionistic vs. classical}
What if we abbreviate ``is a proof'' as ``|:|'' and use the Haskell convention
for function application we get
%
\begin{spec}
(t, bt)  :  (Exists x (P x))   {-"\quad\textbf{if}\quad"-}  bt   : P t
f        :  (Forall x (P x))   {-"\quad\textbf{if}\quad"-}  f t  : P t   {-"\text{~for all~}"-}  t
\end{spec}
%
This now very much looks like type rules\jp{Is this something that the reader should know?}, and that is not a coincidence.
%
The \emph{Curry--Howard correspondence} says that we can think of
propositions as types and proofs as programs.\footnote{Such programs must terminate to prevent circular reasoning.}
%
These typing judgements are not often presented as of FOL, but the correspondence is
used quite a bit in this course to keep track of proofs.


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

\subsection{Proof by contradiction}

%if false
\begin{code}
data ZZ where
  GCD,(:*:) :: ZZ -> ZZ -> ZZ
  One :: ZZ
type Pos = ZZ
data (:=:) (a::ZZ) (b::ZZ) where
data GCD  (a::ZZ) (b::ZZ)  where
type R x = forall (a::ZZ). forall (b::Pos). Not ((b ':*: x) :=: a `And` ('GCD a b :=: 'One))
\end{code}
%endif

Let us express and prove the irrationality of the square
root of 2.
%
We have two main concepts involved: the predicate ``irrational'' and the
function ``square root of''.
%
The square root function (for positive real numbers) can be specified
by $r = \sqrt{s}$ iff |r^2 == s| and |r : REAL|.
%
The formula ``x is irrational'' is just |not(R x)| where |R| is the
predicate ``is rational''.
%
\begin{spec}
  R x = Exists (a:ZZ) (Exists (b:Pos) (b*x==a & GCD(a,b)==1))
\end{spec}
\jp{Why the GCD bit? This is strong version of rational which is easier to negate, but it should be justified at least a bit.}
The classical way to prove a negation |not P| is to assume |P| and
derive something absurd (some |Q| and |not Q|, for example).
%
Lets take |P = R r| and |Q = GCD(a,b)==1|.\jp{Do we have r=2 at this point? From which point?}
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
\jp{With the build of proof terms, I was fully expecting to see the above formalised as such. We probably need quite a bit of stuff about GCD.}

\subsection{Proof by cases}
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

Case 1: |R x| holds.
%
Then we have a proof of |S| with |p=q=r=sqrt 2|.

Case 2: |not (R x)| holds.
%
Then we have another irrational number |x| to play with.
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
%
(The careful reader may have noted that this example also depends on
the axiom of the Excluded Middle.)

\subsection{Functions as proofs}
\jp{It seems that we're resetting (again) and we are going to use Haskell as a proof assisstant. At the very least signposting is necessary. }

To prove a formula |P => Q| we assume a proof |p : P| and derive a
proof |q : Q|.
%
Such a proof can be expressed as |(\p -> q) : (P => Q)|:
%
a proof of an implication is a function from proofs to proofs.

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

In pseudo-Haskell we can express the implication laws as follows:
\jp{But this only makes sense \emph{after} one has accepted Haskell as a proof assistant. And then one may wonder if anything is gained by this exercise.}
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
every year but for proper dependently typed programming we recommend
the language Agda.\jp{make this a footnote?}


%TODO: find the right place for the a note that the type of tuples is isomorphic to the (dependent) function type |{i : 1..n} -> Ai|.

\subsection{Proofs for |And| and |Or|}
\label{sec:PropFrag}
When formally proving properties in FOL we should use the introduction
and elimination rules.
%
The propositional fragment of FOL is given by the rules for ∧, →, ⟷, ¬,
∨.
%
We can use the Haskell type-checker to check proofs in this fragment,
using the functional models for introduction and elimination rules.
%
\begin{figure*}[tbp]
%{
%let abstractfol = True
%include AbstractFOL.lhs
%let abstractfol = False
%}
  \caption{The Haskell module |AbstractFOL|.}
  \label{fig:AbstractFOL}
\end{figure*}
%
Examine Fig.~\ref{fig:AbstractFOL} (also available in the file
\url{AbstractFOL.lhs}), which introduces an empty\jp{an abstract type would be much better} datatype for every
connective (except ⟷), and corresponding types for the introduction
and elimination rules.
%
The introduction and elimination rules are explicitly left
undefined\jp{also, abstract would be better.}, but we can still combine them and type check the
results.
%
For example\footnote{The Haskell notation ``|FOL.Add|'' means the
  |FOL| module version of |Add|.
  %
  It is used here to avoid confusion with the constructor |Add|
  defined earlier in the same chapter.}:
\begin{code}
example0 :: And p q -> And q p
example0 evApq   =  andIntro (andElimR evApq) (andElimL evApq)
\end{code}
%
The variable name |evApq| is a mnemonic for ``\textbf{ev}idence of |And p q|''.

Notice that Haskell will not accept
%
\begin{spec}
example0 evApq   =  andIntro (andElimL evApq) (andElimR evApq)
\end{spec}
%
unless we change the type.

Another example, which is very useful, is ``ex falso quodlibet'',
latin for ``from falsehood, anything (follows)''
\jp{Why this stranglely complicated version instead of forall r. BOT -> r}
%
\begin{code}
exFalso :: And q (Not q) -> p
exFalso evAqnq   =  notElim (notIntro (\ hyp -> evAqnq))
\end{code}
%**TODO explain in more detail

To sum up the |And| case we have one introduction and two elimination rules:
%
\begin{spec}
  andIntro  ::  p -> q -> And p q
  andElimL  ::  And p q ->  p
  andElimR  ::  And p q ->  q
\end{spec}
%
%TODO (by DaHeu):
% The technique of using typed holes is also mentioned at the start of the
% exercises. I think it would be a good idea to use one of the examples to
% demonstrate a step-by-step solution using this technique.
%
If we see these introduction and elimination rules as an API, what
would be a reasonable implementation of the datatype |And p q|?
%
A type of pairs!
%
Then we see that the corresponding Haskell functions would be
%
\begin{spec}
  pair  :: p -> q -> (p, q)  -- |andIntro|
  fst   :: (p, q) -> p       -- |andElimL|
  snd   :: (p, q) -> q       -- |andElimR|
\end{spec}

%{
%let tupling = True
%include AbstractFOL.lhs
%let tupling = False
%}

\paragraph{|Or| is the dual of |And|.}
%
Most of the properties of |And| have corresponding properties for |Or|.
%
Often it is enough to simply swap the direction of the ``arrows''
(implications) and swap the role between introduction and elimination.

\begin{spec}
  orIntroL  :  P   ->  (P|Q)
  orIntroR  :  Q   ->  (P|Q)
  orElim    :  (P=>R)->(Q=>R) -> ((P|Q) => R)
\end{spec}

Here the implementation type can be a labelled sum type, also called
disjoint union and in Haskell:
%
%include Either.lhs
%
%*TODO: Perhaps add an example with (q->p) -> (Not p -> Not q)
%*TODO: Perhaps add an example with (p->p')->(q->q')->(And p q -> And p q)
%*TODO: Perhaps add an example with (p->p')->(q->q')->(Or  p q -> Or  p q)
%*TODO: Explain that the values of type |And p q| can be seen as "proofs" (abstract or concrete).
%
\subsection{Case study: there is always another prime}

As an example of combining quantification (forall, exists) and implication let us turn
to one statement of the fact that there are infinitely many primes.
%
If we assume that we have a unary predicate expressing that a number is prime
and a binary (infix) predicate ordering the natural numbers we can
define a formula |IP| for ``Infinitely many Primes'' as follows:
%
\begin{spec}
 IP = Forall n (Prime n => Exists m (Prime m & m > n))
\end{spec}
%TODO: perhaps introduce Prime and < in L01
%
Combined with the fact that there is at least one prime (like |2|) we
can repeatedly refer to this statement to produce a never-ending
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
%
Now we can start filling in the definition of |proof| as a
2-argument function returning a triple: % nested pair
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
 mod m' p                        ==  {- Def. of |m'| -}
 mod (1 + n!) p                  ==  {- modulo distributes over |+| -}
 mod (mod 1 p  +  mod (n!) p) p  ==  {- modulo comp.: |n!| has |p| as a factor -}
 mod (1        +  0) p           ==
 1
\end{spec}
where |mod x y| is the remainder after integer division of |x| by |y|.
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

\subsection{Existential quantification as a pair type}

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

\subsection{Basic concepts of calculus}
\jp{This is marked as a subjection but after reading the text it feels more like a new chapter is starting.}

Now we have built up quite a bit of machinery to express logic
formulas and proofs.\jp{But are we using it here? Except writing down formulas?}
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
In general, the operator |𝒫| takes a set (here |REAL|) to the set of
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

\begin{spec}
X = {1/n | n ∈ Pos }
\end{spec}

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
  not (Limp p X)                                   = {- Def. of |Limp| -}
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
If |Limp p X| we now know that |X| is infinite.
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
At the core of DSLsofMath\jp{should this be a macro?} is the ability to analyse definitions from
mathematical texts, and here we will use the definition of the limit
of a sequence from \citet[page 498]{adams2010calculus}:

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
Then we have |L : REAL|, |ε : RPos|, and |N : Nat| (or |N : RPos ->
Nat|).
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
  I : (Nat → X) → Nat → PS X
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
prove that |(a1 haslim L1) & (a2 haslim L2)| implies
|(a1+a2) haslim (L1+L2)|.
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

  We say that \(f(x)\) \textbf{approaches the limit} \(L\) as \(x\) \textbf{approaches} \(a\), and we write

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
function |f| and this leaves us with three essential components: |f|,
|a|, and |L|.
%
Thus, |lim| can be seen as a ternary (3-argument) predicate which is
satisfied if the limit of |f| exists at |a| and equals |L|.
%
If we apply our logic toolbox we can define |lim| starting something like this:
%
\begin{spec}
lim f a L  =  Forall (epsilon > 0) (Exists (delta > 0) (P epsilon delta))
\end{spec}
%
when |P| is a predicate yet to define.
Indeed, it is often useful to introduce a local name (like |P| here) to help
break the definition down into more manageable parts.
%
If we now naively translate the last part we get this ``definition''
for |P|:
%
\begin{spec}
{-"\quad"-}  where  P epsilon delta = (0 < absBar (x - a) < delta) => (x `elem` Dom f  && absBar (f x - L) < epsilon))
\end{spec}
%
Note that there is a scoping problem: we have |f|, |a|, and |L| from
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
arguments, |f| and |a|.
%
This means that each function |f| can have \emph{at most} one limit
|L| at a point |a|.
%
(This is not evident from the definition and proving it is a good
exercise.)
\jp{Exercise: what does Adams means by ``delta possibly depends on epsilon''? How did we express that in our formal defintion? Hint: how whould you express that delta cannot depend on epsilon?}

\subsection{Recap of syntax trees with variables, |Env| and |lookup|}

% TODO reformulate in book form (or move earlier, or remove)

This was frequently a source of confusion already the first week so
there is already a question + answers earlier in this text.
%
But here is an additional example to help clarify the matter.
%
\begin{code}
data Rat v = RV v | FromI Integer | RPlus (Rat v) (Rat v) | RDiv (Rat v) (Rat v)
  deriving (Eq, Show)

newtype RatSem = RSem (Integer, Integer)
\end{code}
%
We have a type |Rat v| for the syntax trees of rational number
expressions (with variables of type |v|) and a type |RatSem| for the
semantics of those rational number expressions as pairs of integers.
%
The constructor |RV :: v -> Rat v| is used to embed variables with
names of type |v| in |Rat v|.
%
We could use |String| instead of |v| but with a type parameter |v| we
get more flexibility at the same time as we get better feedback from
the type checker.
%
Note that this type parameter serves a different purpose from the type
parameter in~\ref{sec:toComplexSyn}.

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
%
\begin{code}
type Env v s = [(v, s)]

envToFun :: (Show v, Eq v) => Env v s -> (v -> s)
envToFun [] v = error ("envToFun: variable "++ show v ++" not found")
envToFun ((w,s):env) v
  | w == v     = s
  | otherwise  = envToFun env v
\end{code}
%
Thus, |Env v s| can be seen as an implementation of a ``lookup
table''.
%
It could also be implemented using hash tables or binary search trees,
but efficiency is not the point here.
%
Finally, with |envToFun| in our hands we can implement a second
version of the evaluator:
%
\begin{code}
evalRat2 :: (Show v, Eq v) => (Env v RatSem) -> (Rat v -> RatSem)
evalRat2 env e = evalRat1 (envToFun env) e
\end{code}

% \paragraph{The law of the excluded middle}
%
% Many had problems with implementing the ``law of the excluded middle''
% in the exercises and it is indeed a tricky property to prove.
% %
% They key to implementing it lies in double negation and as that is
% encoded with higher order functions it gets a bit hairy.
%
% TODO[Daniel]: more explanation

\paragraph{SET and PRED}
%
\label{par:SETandPRED}

Several groups have had trouble grasping the difference between |SET|
and |PRED|.
%
This is understandable, because we have so far in the lectures mostly
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

This subsection contains some extra material which is not a required
part of the course.

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

%include E2.lhs
