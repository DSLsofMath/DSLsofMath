%if false
\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyCase #-}
module DSLsofMath.PropositionalLogic where

deriving instance Eq Prop
\end{code}
%endif

\section{Propositional Calculus}
\label{sec:PropFrag} % yep, the propositional calculus is a propositional fragment of other logics, such as FOL.

Our first DSL for this chapter is the language of \emph{propositional
  calculus} (or propositional logic), modelling simple propositions
with the usual combinators for and, or, implies, etc.
%
\lnOnly{(The Swedish translation is ``satslogik'' and some more
  Swe-Eng translations are collected on the GitHub page of these
  lecture notes\footnote{\url{https://github.com/DSLsofMath/DSLsofMath/wiki/Translations-for-mathematical-terms}}.)}%
%
When reading a logic book, one will encounter several concrete
syntactic constructs related to propositional logic, which are
collected in Table~\ref{tab:PropCalc}.
%
Each row lists common synonyms and their arity.
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
\caption{Syntactic constructors for propositions.}
% In addition, |a|, |b|, |c|, \ldots are used as names of propositions
\label{tab:PropCalc}
\end{table}

Some example propositions are |p1 = a && (not a)|, |p2 = a |||| (not
a)|, |p3 = a => b|, |p4 = (a && b) => (b && a)|.
%
The names |a|, |b|, |c|, \ldots are ``propositional variables'': they
can be substituted for any proposition.
%
We could call them ``variables'', but in upcoming sections we will add
another kind of variables (and quantification over them) to the
calculus --- so we keep calling them ``names'' to avoid confusing them.


% simple arithmetic in \sref{sec:ch1ex}
Just as we did with with complex number expressions in
\cref{sec:complex-arithmetic}, %\cref{sec:DSLComplex}
we can model the abstract syntax of propositions as a datatype:
%
\begin{code}
data Prop  =  Con      Bool
           |  Not      Prop
           |  And      Prop  Prop
           |  Or       Prop  Prop
           |  Implies  Prop  Prop
           |  Name     Name
type Name = String
\end{code}
%
The example expressions can then be expressed as
%
\begin{code}
p1, p2, p3, p4 :: Prop
p1 = And  (Name "a")  (Not (Name "a"))
p2 = Or   (Name "a")  (Not (Name "a"))
p3 = Implies  (Name "a")  (Name "b")
p4 = Implies  (And a b)   (And b a)
  where a = Name "a"; b = Name "b"
\end{code}
%
Because ``names'' stand for propositions, if we assign
truth values for the names, we can compute a truth value of the whole
proposition for the assignment in question.
%

\subsection{An Evaluator for |Prop|}
Let us formalise this idea in general, by writing an evaluator which takes
a |Prop| to its truth value.
%
(The evaluation function for a DSL describing a logic is often called
|check| instead of |eval| but for consistency we stick to |eval|.)
%
\begin{code}
type Env = Name -> Bool
eval :: Prop -> Env -> Bool
eval (Implies p q)  env = eval p env  ==>  eval q env
eval (And p q)      env = eval p env  &&   eval q env
eval (Or  p q)      env = eval p env  ||   eval q env
eval (Not p)        env = not (eval p env)
eval (Name n)       env = env n
eval (Con t)        env = t

(==>) :: Bool -> Bool -> Bool
False  ==> _ {-"\quad"-}  = True
True   ==> p              = p
\end{code}
%
The function |eval| translates from the syntactic domain to the
semantic domain, given an environment (an assignment of names to truth
values), which we represent as a function from each |Name| to |Bool|.
%
Here |Prop| is the (abstract) \emph{syntax} of the language of
propositional calculus and |Bool| is the \emph{semantic domain}, and
|env : Env| is a necessary extra parameter to write the function.
%
Alternatively, and perhaps more elegantly, we can view |Env -> Bool|
as the semantic domain.
%
\subsection{Truth tables and tautologies}

\definecolor{lightergray}{gray}{0.90}
\newcolumntype{a}{>{\columncolor{lightgray}}c}
\newcolumntype{b}{>{\columncolor{lightergray}}c}

\begin{figure}[tbp]\centering
  \begin{subfigure}[b]{0.2\textwidth}\centering
    \begin{tabular}{|| a c a||}
        \hline  |F| & |=>| & |a|
      \\\hline  |F| & |T| & |F|
      \\        |F| & |T| & |T|
      \\\hline
    \end{tabular}
    \caption{|t = F => a|}
    \label{fig:F2a}
  \end{subfigure}
  \begin{subfigure}[b]{0.2\textwidth}\centering
    \begin{tabular}{|| aca||}
        \hline   |a| & |=>|& |b|
      \\\hline   |F| & |T| & |F|
      \\         |F| & |T| & |T|
      \\         |T| & |F| & |F|
      \\         |T| & |T| & |T|
      \\\hline
    \end{tabular}
    \caption{|p3 = a => b|.}
    \label{fig:TruthTableImplies}
  \end{subfigure}
  \begin{subfigure}[b]{0.45\textwidth}\centering
    \begin{tabular}{|| abacaba ||}
        \hline   |a| & |&&| & |b| & |=>| & |b| & |&&| & |a|
      \\\hline   |F| & |F| & |F| & |T|  & |F| & |F| & |F|
      \\         |F| & |F| & |T| & |T|  & |T| & |F| & |F|
      \\         |T| & |F| & |F| & |T|  & |F| & |F| & |T|
      \\         |T| & |T| & |T| & |T|  & |T| & |T| & |T|
      \\\hline
    \end{tabular}
    \caption{\(|p4| = (a \wedge b) \Rightarrow (b \wedge a)\).}
    \label{fig:abswap}
  \end{subfigure}
  \caption{Truth table examples. Darker shades are filled in first,
    white column is the final result.}
  \label{fig:TruthTables}
\end{figure}

%TODO Perhaps cite the full \refFig{fig:TruthTables}
%TODO Perhaps cite the middle subfigure \refFig{fig:TruthTableImplies}

\index{assignment function||textbf}
%
Values of type |Name -> a| are called ``assignment functions'' because
they assign values (of type |a|) to the variable names.
%
When we have |a = Bool|, and not too many variable names, we can
enumerate all the combinations in a truth table.

As a first example of a truth table, consider the proposition |F => a|
which we call |t| here.
%
The truth table semantics of |t| is usually drawn as
in~\refFig{fig:F2a}:
%
one column for each symbol, filled with the truth value of the
expression ``rooted'' at that symbol.
%
Thus, here we have one column for the name |a| listing all
combinations of |T| and |F|, one (boring) column for |F|, and one
column in the middle for the result of evaluating the expression.
%
This table shows that no matter what value assignment we try for the
only variable |a|, the semantic value is |T = True|.
%
Thus the whole expression could be simplified to just |T| without
changing its semantics.

If we continue with the example |p4| from above we have two names |a|
and |b| which together can have any of four combinations of true and
false.
%
After the name-columns are filled, we fill in the rest of the table
one operation (column) at a time (see \refFig{fig:abswap}).
%
The |&| columns become |F F F T| and finally the |=>| column (the
output) becomes true everywhere.
%
For our other examples, |p1| is always false, |p2| is always true, and
|p3| is mixed.

A proposition whose truth table output is constantly true is called a
\emph{tautology}.
%
Thus |t|, |p2| and |p4| are tautologies.
%
We can formalise this idea as the following tautology-tester --- a
predicate which specifies the subset of |Prop|erties which are always
true:
%
\begin{code}
isTautology :: Prop -> Bool
isTautology p = and (map (eval p) (envs (freeNames p)))
\end{code}
%
It uses the helper functions |envs| to generate all possible
environments (functions of type |Env = Name -> Bool|) for a given list
of names and |freeNames| to find all names in a proposition.
%
As an example, for |p4| above, |freeNames| would return the list
|["a", "b"]| and |envs| would return a four-element |[Env]|, one for
each row in the truth table.
%
The |map| would then apply |eval p4| to each element in the list to
evaluate top-level truth value of the expression for each row.
%
Finally |and| combines the results with |(&&)| to ensure that they are
all |True|.
%
%TODO: Haskell notation for list comprehensions not introduced. Possible alternative in TeX comment
% \begin{code}
% envs (n:ns)  =  map (\(e,b) -> \n' -> if n == n' then b else e n') ((envs ns) × [False,True])
% [] × ys = []
% (x:xs) × ys = map (x,) ys ++ (xs × ys)
% \end{code}

\begin{code}
envs :: [Name] -> [Env]
envs []      =  [error "envs: never used"]
envs (n:ns)  =  [  \n' -> if n == n' then b else e n'
                |  b  <-  [False, True]
                ,  e  <-  envs ns
                ]

freeNames :: Prop -> [Name]
freeNames = error "exercise"
\end{code}
%
Truth table verification is only viable for propositions with few
names because of the exponential growth in the number of cases to
check: for |n| names we get $2^n$ different rows in a truth table.

\begin{exercise}
Define the function |freeNames|.
\end{exercise}

There are much better algorithms to evaluate truth values than the
naive one we just showed, but we will not go this route.
%
Rather, we can introduce the notion of \emph{proof}.
%
(And in fact, the complexity of the best (known) algorithms for
checking that a proposition is a tautology remain exponential in the
number of variables.)


\subsection{Proofs for Propositional Logic}

Given a |Prop|osition |p| and a proof |t| (represented as
an element of another type |Proof|), we can write a function that checks
that |t| is a valid proof of |p|:

\begin{code}
checkProof :: Proof -> Prop -> Bool
\end{code}
%
But we still have to figure out what consitutes proofs.
%
We will build up the ``proof DSL'' one step at a time by looking at
what we need to prove the different propositions.

To prove |And P Q|, one needs both a proof of |P| and a proof of |Q|.
%
In logic texts, one will often find
\[ \frac{P \quad Q}{P ∧ Q} \] to represent this fact, which is called the \emph{introduction rule for (∧)}.
%
For the proof to be complete, one still needs to provide a full proof of |P| and another for |Q| --- it is not enough to just invoke this rule.

Therefore, in Haskell, can represent this rule by a proof-term
constructor |AndIntro| with two |Proof| arguments:
\begin{spec}
AndIntro :: Proof -> Proof -> Proof
\end{spec}
%
and, the corresponding case of the |checkProof| function will look like this:

\begin{spec}
checkProof (AndIntro t u) (And p q) = checkProof t p && checkProof u q
\end{spec}

To prove |Or P Q|, we need either a proof of |P| or a proof of |Q| ---
but we need to know which side (|Left| for |p| or |Right| for |q|) we
refer to.
%
Therefore, we introduce two proof-term constructors:
%
% \pj{A symmetric treatment would use |(,)| for |AndIntro| and |Either| for |OrIntro|, or it would use two arguments (as now) for |AndIntro| and |OrIntroL| and |OrIntroR| instead of |OrIntro|.}
%
\begin{spec}
OrIntroL  :: Proof -> Proof
OrIntroR  :: Proof -> Proof
\end{spec}
%
There are a couple of possible approaches to deal with negation.
%
One approach is to use de Morgan dualisation:
%
\begin{spec}
Not (a  `Or`   b)  =  Not a  `And`  Not b
Not (a  `And`  b)  =  Not a  `Or`   Not b
...
\end{spec}
%
Negation can then be pushed all the way down to names, which can
recieve a special treatment in proof-checking.

However, we will instead apply the same treatment to negation as to
other constructions, and define a suitable introduction rule:
\[ \frac{P → Q \quad P → ¬Q}{¬P} \]
%
(Intuitively, this rule says that to prove \(¬P\), one needs to derive
a contradiction from \(P\).)
%
We can represent it by the constructor |NotIntro :: Prop -> Proof ->
Proof -> Proof|.
%
Because we have inductive proofs (described from the bottom up), we
have the additional difficulty that this rule conjures-up a new
proposition, |Q|.
%
This is why we need an additional |Prop| argument, which gives the |Q|
formula.

There is no rule to introduce falsity (|⊥|) --- otherwise we'd have an
inconsistent logic!
%
Thus the last introduction rule deals with Truth, with no
premiss: \[\frac{\phantom{hello}}{⊤}\]
%
The proof has no information either: |TruthIntro :: Proof|.

To complete the system, in addition to introduction rules (where the
connective appears as conclusion), we also need elimination rules
(where the connective appears as premiss).
%
For conjuction (|And|), we have two eliminations rules:
%
\[\frac{P ∧ Q}{P} \qquad \text{and} \qquad \frac{P ∧ Q}{Q}\]
%
So we represent them by |AndElimL :: Prop -> Proof -> Proof| (and
|AndElimR| symmetrically), where the extra |Prop| argument corresponds
to~|Q|.

For disjunction (|Or|) the idea is that if we know that \(P ∨ Q\)
holds, then we have two cases: either |P| holds or |Q| holds.
%
If only we can find a proposition |R| which is a consequence of both
|P| and |Q|, then, regardless of which case we are facing, we know
that |R| will hold.
%
So, we get the following elimination rule for \(P ∨ Q\):
%
\[\frac {P ∨ Q \quad P → R \quad Q → R} R\]


Our elimination for negation is $\frac {¬ ¬ P} P$.
%
It simply says that two negations cancel.

Finally we can eliminate falsity as follows:
\[\frac {⊥} P\]
%
This rule goes sometimes by its descriptive latin name \textit{ex
  falso quodlibet} --- from falsehood, anything (follows).

We can then write our proof checker as follows:
\savecolumns
\begin{code}
checkProof TruthIntro        (Con True)   =   True
checkProof (AndIntro t u)    (And p q)    =   checkProof t p
                                          &&  checkProof u q
checkProof (OrIntroL t)      (Or p q)     =   checkProof t p
checkProof (OrIntroR u)      (Or p q)     =   checkProof u q
checkProof (NotIntro q t u)  (Not p)      =   checkProof t (p `Implies` q)
                                          &&  checkProof u (p `Implies` Not q)
checkProof (AndElimL q t)       p  =   checkProof  t  (p `And` q)
checkProof (AndElimR p t)       q  =   checkProof  t  (p `And` q)
checkProof (OrElim p q t u v)   r  =   checkProof  t  (p `Implies` r)
                                   &&  checkProof  u  (q `Implies` r)
                                   &&  checkProof  v  (Or p q)
checkProof (NotElim t)          p  =   checkProof  t  (Not (Not p))
checkProof (FalseElim t)        p  =   checkProof  t  (Con False)
\end{code}

Any other combination of proof/prop is an incorrect combination: the proof is not valid for the proposition.

\begin{spec}
checkProof _ _ = False -- incorrect proof
\end{spec}

Once more, it can be interesting to view |checkProof| as an evaluator.
%
This can be made plain by flipping its arguments: |flip checkProof ::
Prop -> (Proof -> Bool)|.
%
This way, one can understand |Proof -> Bool|, a subset of proofs, as
the semantic domain of |Prop|.
%
In other words, a proposition can be interpreted as the subset of
proofs which prove it.

\subsection{Implication, hypothetical derivations, contexts}

We have so far omitted to deal with |Implies|.
%
One reason is that we can use the so-called material implication
definition which we invoked earlier in truth tables.
%
It means to define |Implies a b = (Not a) `Or` b| --- and this
equality means that there is no need to deal specially with
|Implies|.
%
However this approach does not bring any new insight.
%
In particular, this view is hard to transport to more complicated
logics (such as second-order logic).

Thus we take our usual approach and give rules for it.
%
The introduction rule is sometimes written in this way in logic
texts: \[\frac{\begin{array}{c}P \\ \vdots \\ Q \end{array}}{P → Q}\]
Such a notation can, however, be terribly confusing.
%
We were already used to the fact that proofs above the line had to be
continued, so what can the dots possibly mean?
%
The intended meaning of this notation is that, to prove $P → Q$,
it suffices to prove $Q$, but one is also allowed to use $P$ as an
assumption in this (local) proof of $Q$.

We can use our DSL to formalise this rule as Haskell data, by adding a
constructor corresponding to implication introduction: |ImplyIntro ::
(Proof -> Proof) -> Proof|.
%
The fact that the premiss can depend on the assumption |Q| is
represented by a function whose parameter is the proof of |Q| in
question.
%
In other words, to prove the formula |P -> Q| we assume a proof |t| of
|P| and derive a proof |u| of~|Q|.
%
So, a proof of an implication is a function from proofs to proofs.

The eliminator for implication (also known as \textit{modus ponens})
is
%
\[\frac{P → Q \quad P} Q\]
%
We formalise it as |ImplyElim :: Prop -> Proof -> Proof ->
Proof|\footnote{The proposition |P| is not given by the conclusion and
  thus is provided as part of the proof.}.
%
And we can finally complete our proof checker as follows:

\restorecolumns
\begin{code}
checkProof (Assume p')          p                =   p == p'
checkProof (ImplyIntro f)       (p `Implies` q)  =   checkProof (f (Assume p)) q
checkProof (ImplyElim p t u)    q                =   checkProof t (p `Implies` q)
                                                 &&  checkProof u p
checkProof _                    _                =   False -- incorrect proof
\end{code}
%
And, for reference, the complete DSL for proofs is given by the
following datatype:
\begin{code}
data Proof  =  TruthIntro                   |  FalseElim Proof
            |  AndIntro  Proof  Proof
            |  AndElimL  Prop   Proof       |  AndElimR  Prop  Proof
            |  OrIntroL  Proof              |  OrIntroR  Proof
            |  OrElim Prop Prop Proof Proof Proof
            |  NotIntro Prop Proof Proof    |  NotElim Proof
            |  Assume Prop
            |  ImplyIntro (Proof -> Proof)  |  ImplyElim  Prop  Proof Proof
\end{code}

\paragraph{Aside}
The |Assume| constructor may make the reader somewhat uneasy: how
come that we can simply assume anything?
%
The intent is that this constructor is \emph{private} to the
|checkProof| function (or module).
%
No user-defined proof can use it.
%
The most worried readers can also define the following version of
|checkProof|, which uses an extra context to check that assumption
have been rightfully introduced earlier.\footnote{For the \textit{cognoscenti}, this kind of
  presentation of the checker matches well the sequent calculus
  presentation of the proof system} 

\begin{spec}
checkProof' :: Context -> Proof -> Prop -> Bool
checkProof' ctx  (ImplyIntro t)  (p `Implies` q)  = checkProof' (p:ctx) t q
checkProof' ctx  (Assume _)      p                = p `elem` ctx
\end{spec}

\paragraph{Example proof}
We can put our proof-checker to the test by writing some proofs and
verifying them.

\begin{code}
conjunctionComm :: Prop
conjunctionComm = p4

conjunctionCommProof :: Proof
conjunctionCommProof = ImplyIntro step
  where  step :: Proof -> Proof
         step evAB =  AndIntro  (AndElimR  (Name "a")  evAB  )
                                (AndElimL  (Name "b")  evAB  )
\end{code}
where |evAB| stands for ``evidence for |A| and |B|''.
%
We can then run the checker and verify:
|checkProof conjunctionCommProof conjunctionComm == True|

%if False
\begin{code}
-- >>> checkProof conjunctionCommProof conjunctionComm
-- True
\end{code}
%endif

\begin{exercise}
  Try to swap |AndElimL| and |AndElimR| in the above proof.
  %
  What will happen and why?
%if False
\begin{code}
conjunctionCommProof2 :: Proof
conjunctionCommProof2 =
  ImplyIntro (\evAB ->
  AndIntro  (AndElimL (Name "b") evAB)
            (AndElimR (Name "a") evAB) )

-- >>> checkProof conjunctionCommProof2 conjunctionComm
-- False
\end{code}

%endif
\end{exercise}

\paragraph{|Or| is the dual of |And|}
Before moving on to our next topic, we make a final remark on |And| and |Or|.
%
Most of the properties of |And| have corresponding properties for
|Or|.
%
This can be explained one way by observing that they are de Morgan
duals.
%
Another explanation is that one can swap the direction of the arrows
in the types of the the role between introduction and
elimination.
%
(Using our presentation, doing so requires applying isomorphisms.)


\subsection{The Haskell type-checker as a proof checker}
\label{sec:haskell-as-proof checker}
Perhaps surprisingly, the proof-checker that we just wrote is already
built-in in the Haskell compiler.
%
Let us clarify what we mean, using the same example, but adapt it to
let the type-checker do the work:

\begin{code}
conjunctionCommProof' :: Implies (And a b) (And b a)
conjunctionCommProof' = implyIntro step
  where  step :: And a b -> And b a
         step evAB =  andIntro  (andElimR  evAB)
                                (andElimL  evAB)
\end{code}

That is, instead of writing propositions, we write types (|And|, |Or|,
|Implies| --- which we leave abstract for now).
Instead of using |Proof| constructors, we use functions
whose types capture rules:
%
\begin{code}
truthIntro  :: Truth
falseElim   :: False -> p
andIntro    :: p -> q -> And p q
andElimL    :: And p q -> p
andElimR    :: And p q -> q
orIntroL    :: p -> Or p q
orIntroR    :: q -> Or p q
orElim      :: Or p q -> (p `Implies` r) -> (q `Implies` r) -> r
notIntro    :: (p `Implies` q) `And`  (p `Implies` Not q) -> Not p
notElim     :: Not (Not p) -> p
implyIntro  :: (p -> q) -> (p `Implies` q)
implyElim   :: (p `Implies` q) -> (p -> q)
\end{code}
Instead of running |checkProof|, we type-check the above program.
%
Because the proof is correct, we get no type-error.
\begin{exercise}
  What would happen if you swap |andElimR| and |andElimL|? Why?
\end{exercise}
%

This style of propositional logic proof is very economical, because
not only the checker comes for free, but we additionally get all
the engineering tools of the Haskell tool-chain.

One should be careful however that Haskell is not designed with
theorem-proving in mind.  For this reason it is easily possible make
the compiler accept invalid proofs. The main two sources of invalid
proofs are 1. non-terminating programs and 2. exception-raising
programs. In sum, the issue is that Haskell allows the programmer to
define partial functions (instead of total ones, see
\cref{sec:partial-and-total-functions}).


\subsection{Intuitionistic Propositional Logic}
\label{sec:intuitionistic-logic}
% ToC:
% + intuitionistic logic
% + Not p = p -> False
% + notIntro
% + implement Implies as (->)
% + implement And as (,)
% + implement Or as Either
% + introduce "proof terms" and STLC
% + "excluded middle" impossible
% + implement not (not exclMid)
% + revisiting the tupling transform
% + "Logic as impoverished typing rules"
We can make the link beween Haskell and logic more tight if we
restrict ourselves to \emph{intuitionistic} logic.

One way to characterize intuitionistic logic is that it lacks native
support for negation.
%
Instead, |Not p| is represented as |p `Implies` False|:
\begin{code}
type Not p = p `Implies` False
\end{code}
The intuition behind this definition is the principle of proof by
contradiction: if assuming |p| leads to a contradition (|False|), then
|p| must be false; so |Not p| should hold.

When doing this kind of definition, one gives up on |notElim|: there
is no way to eliminate (double) negation.
\begin{code}
notElim = error "not possible as such in intuitionistic logic"
\end{code}
On the other hand the introduction rule for negation becomes a theorem of the logic.
The formulation of the theorem is:
\begin{spec}
notIntro :: (p `Implies` q) `And` (p `Implies` Not q) -> Not p
\end{spec}
where |Not p = p `Implies` False|, and its proof is:
\begin{code}
notIntro (evPimpQ, evPimpNotQ) =
    implyIntro $ \evP ->
    let  evQ     = evPimpQ     `implyElim` evP
         evNotQ  = evPimpNotQ  `implyElim` evP
    in evNotQ `implyElim` evQ
\end{code}
% $ -- resync emacs mode parser
%    (evPimpNotQ `implyElim` evP ) `implyElim` (evPimpQ `implyElim` evP)
\label{sec:curry-howard}%
By focusing on intuitionistic logic, we can give a \emph{typed}
representation for each of the formula constructors.
%
Let us consider implication first.
%
The proof rules |impIntro| and |impElim| seem to be conversion from
and to functions, and so it should be clear that the representation of
the implication formula is a function:
%
\begin{code}
type Implies p q = p -> q
implyElim   f = f
implyIntro  f = f
\end{code}
%
Conjunction is represented as pairs; that is, if |p : P| and |q : Q|
then the proof of |And P Q| should be a pair of |p| and |q|.
%
The elimination rules are projections.
%
In code:
\begin{code}
type And p q = (p,q)
andIntro t u = (t,u)
andElimL  = fst
andElimR  = snd
\end{code}
Similarly, disjuction is represented as |Either|: if |p : P| then
|Left p : Or P Q| and if |q : Q| then |Right q : Or P Q|.
\begin{code}
type Or a b = Either a b
orIntroL  = Left
orIntroR  = Right
orElim pOrq f g = case pOrq of
  Left   p -> f  p
  Right  q -> g  q
\end{code}
We already had characterized or-elimination as case analysis, and,
indeed, this is how we implement it.

Truth is represented as the unit type:
\begin{code}
type Truth = ()
truthIntro = ()
\end{code}

And falsehood is represented as the \emph{empty} type (with no
constructor):
\begin{code}
data False
falseElim x = case x of {}
\end{code}
Note that the case-analysis has nothing to take care of here.

In this way we can build proofs (``proof terms'') for all of
intuitionistic propositional logic (IPL).
%
As we have seen, each such proof term is a program in Haskell.
%
Conversely, every program written in this fragment of Haskell
(functions, pairs, |Either|, no recursion and full coverage of cases)
can be turned into a proof in IPL.
%
This fragment is called the simply-typed lambda calculus (STLC) with
sum and products.

\subsection{Type-Driven Development of Proofs as Programs}
\index{type-driven development}
%
With the logic connectives implemented as type constructors we explore
a few more examples of laws and their proofs.

\paragraph{The law of the excluded middle}
\label{sec:excluded-middle}
As an example of how intuitionism affects logic, consider the law
of the excluded middle, which states that, for any proposition |P|,
either |P| or |Not P| holds.
%
For example, either it rains or it does not rain.
%
There is no ``middle ground''.
%
If we attempt to prove |Or P (Not P)| in intuitionitic logic, we
quickly find ourselves in a dead end.
%
Clearly, we cannot prove |P| for any |P|.
%
Likewise |Not P|, or equivalently |P -> False| cannot be deduced.

What we have to do is to account for the fact that we cannot use
negation elimination, and so we have to make-do with proving |Not (Not
Q)| instead of~|Q|.
%
This is exactly what we have to do to (almost) prove the law of
excluded middle.
%
Doing so we can then provide this Haskell-encoded proof:

\begin{code}
excludedMiddle :: Not (Not (p `Or` Not p)) -- to prove this, we can ...
excludedMiddle k = -- ... assume |Not (Or p (Not p))| and prove falsity.
   k -- So, we can prove falsity if we can prove |Or p (Not p)|.
   (Right  -- We can prove in particular the right case, |Not p|
     (\evP ->  -- ... by assuming that |p| holds, and prove falsity.
        k --  Again, we can prove falsity if we can prove |Or p (Not p)|.
        (Left -- This time, we can prove in particular the left case, |p| ...
          evP))) -- because we assumed it earlier!
\end{code}
which can be shortened to a one-liner if we cut out the comments:
\begin{code}
excludedMiddle' :: Not (Not (p `Or` Not p))
excludedMiddle' k = k (Right (\evP -> k (Left evP))) 
\end{code}
% excludedMiddle k = k (Right (\evP -> k (Left evP)))
% excludedMiddle k = k (Right (k . Left))
% excludedMiddle k = (k . Right) (k . Left)

\paragraph{Revisiting the tupling transform}
%
In Exercise~\ref{exc:tuplingE1}, the ``tupling transform'' was
introduced, relating a pair of functions to a function returning a
pair. (Revisit that exercise if you skipped it before.)
%
There is a logic formula corresponding to the type of the tupling
transform:
%
\begin{spec}
  (a `Implies` (b `And` c)) `Iff` (a `Implies` b) `And` (a `Implies` c)
\end{spec}
(|Iff| refers to implication in both directions).
The proof of this formula closely follows the implementation of the transform.
%
Therefore we start with the two directions of the transform as functions:

\begin{code}
test1' :: (a -> (b, c)) -> (a->b, a->c)
test1' a2bc =  ( \a -> fst  (a2bc a)
               , \a -> snd  (a2bc a) )

test2' :: (a->b, a->c) -> (a -> (b, c))
test2' fg = \a -> (fst fg a, snd fg a)
\end{code}
%
Then we move on to the corresponding logic statements with proofs.
%
Note how the functions are ``hidden inside'' the proof.
%
\begin{code}
test1  ::  Implies (Implies a (And b c)) (And (Implies a b) (Implies a c))
test1  = implyIntro (\a2bc ->
             andIntro  (implyIntro (\a -> andElimL  (implyElim a2bc a)))
                       (implyIntro (\a -> andElimR  (implyElim a2bc a))))

test2  ::  Implies (And (Implies a b) (Implies a c)) (Implies a (And b c))
test2  =   implyIntro (\fg ->
             implyIntro (\a ->
               andIntro  (implyElim (andElimL  fg) a)
                         (implyElim (andElimR  fg) a)))
\end{code}


\paragraph{Logic as impoverished typing rules}

Another view of the same isomorphism is that the logical rules for IPL
can be obtained by erasing programs from the typing rules for STLC.
We will show here only the application rule, leaving the rest as
an exercise. This typing rule for function application can be written as follows:

\[\frac{f : A → B \quad x : A}{f(x) : B}\]

After erasing the colon (:) sign and what comes before it, we obtain
\textit{modus ponens} --- implication elimination.

The \emph{Curry--Howard correspondence} is a general principle that
says that we can think of propositions as types, and proofs as
programs. This principle goes beyond propositional logic (and even first
order logic): it applies to all sorts of logics and programming
languages, with various levels of expressivity and features.

%
%
%*TODO: Perhaps add an example with (q->p) -> (Not p -> Not q)
%*TODO: Perhaps add an example with (p->p')->(q->q')->(And p q -> And p q)
%*TODO: Perhaps add an example with (p->p')->(q->q')->(Or  p q -> Or  p q)
%*TODO: Explain that the values of type |And p q| can be seen as "proofs" (abstract or concrete).
%
