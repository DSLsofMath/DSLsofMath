%if false
\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyCase #-}
module PropositionalLogic where

deriving instance Eq Prop
\end{code}
%endif

\section{Propositional Calculus}
\label{sec:PropFrag} % yep, the propositional calculus is a propositional fragment of other logics, such as FOL.

Our first DSL for this chapter is the language of \emph{propositional
  calculus} (or propositional logic), modelling simple propositions with the usual
combinators for and, or, implies, etc.
%
\lnOnly{(The Swedish translation is ``satslogik'' and some more Swe-Eng
translations are collected on the GitHub page of these lecture notes\footnote{\url{https://github.com/DSLsofMath/DSLsofMath/wiki/Translations-for-mathematical-terms}}.)}
%
When reading a logic book, one will encounter several concrete syntactic constructs related to propositional logic, which are collected in
Table~\ref{tab:PropCalc}. Each row lists common synonyms and their arity (number of arguments).
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

Some example propositions will include \(|p1| = a \wedge (\neg a)\),
\(|p2| = a \Rightarrow b\), \(|p3| = a \vee (\neg a)\),
\(|p4| = (a \wedge b) \Rightarrow (b \wedge a)\).
%
The names |a|, |b|, |c|, \ldots are ``propositional variables'': they
can be substituted for any proposition. We could call them ``variables'', but in upcoming sections we will add
another kind of variables (and quantification over them) to the
calculus --- so we keep calling them ``names'' to avoid mixing them
up.


%
Just as we did with simple arithmetic, and with complex number
expressions in \cref{sec:DSLComplex}, we can model the abstract
syntax of propositions as a datatype:
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
p1 = And (Name "a") (Not (Name "a"))
p2 = Implies (Name "a") (Name "b")
p3 = Or (Name "a") (Not (Name "a"))
p4 = Implies (And a b) (And b a)
  where a = Name "a"; b = Name "b"
\end{code}
%
Because ``names'' stand for propositions, if we assign 
truth values for the names, we can compute a truth value of the whole
proposition for the assignment in question.
%

\subsection{An Evaluator for |Prop|}
Let us formalise this in general,
by writing an evaluator which takes
a |Prop| term to its truth value.

(The evaluation function for a DSL describing a logic is often called
|check| instead of |eval| but for consistency we stick to |eval|.)
%
\begin{code}
eval :: Prop -> (Name -> Bool) -> Bool
eval (Implies p q)  env = eval p env `implies` eval q env
eval (And p q)      env = eval p env && eval q env
eval (Or  p q)      env = eval p env || eval q env
eval (Not p)        env = not (eval p env)
eval (Name n)       env = env n
eval (Con t)        env = t

implies :: Bool -> Bool -> Bool
implies False  _  =  True
implies _      p  =  p
\end{code}
%
The function |eval| translates from the syntactic domain to the semantic
domain, given an environment (an assignment of names to truth values), which we represent as a function from each |Name| to |Bool|.
.
Here |Prop| is the (abstract) \emph{syntax} of the language of
propositional calculus and |Bool| is the \emph{semantic
  domain}, and |Name -> Bool| is a necessary extra parameter to write the function.
%
Alternatively, and perhaps more elegantly, we can view |(Name -> Bool) -> Bool| as the semantic domain.
%
\subsection{Truth tables and tautologies}
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
%
For our other examples, |p1| is always false, |p2| is mixed and |p3| is always true.

A proposition whose truth table output is constantly true is called a
\emph{tautology}.
%
Thus |t|, |p3| and |p4| are tautologies. We can formalise this idea as the following tautology-tester:
\begin{code}
isTautology :: Prop -> Bool
isTautology p = and [eval p e | e <- envs (freeNames p)]
\end{code}
which uses the following intermediate function to generate all possible environments for a given list of names
\begin{code}
envs :: [Name] -> [Name -> Bool]
envs (n:ns) = [\n' -> if n == n' then b else e n' | b <- [True,False], e <- envs ns]
\end{code}
and a function to find all names in a proposition:
\begin{code}
freeNames :: Prop -> [Name]
freeNames = error "exercise"
\end{code}
%
Truth table verification is only viable for propositions with few
names because of the exponential growth in the number of cases to
check: we get $2^n$ different |truthTables| for |n| names.
%
\begin{exercise}
Define the function |freeNames|.
\end{exercise}

There are much better algorithms to evaluate truth values than the naive one we just showed,
but we will not go this route.  Rather, we can introduce the notion of \emph{proof}.
(And in fact, the best (known) algorithms remain exponential in the number of variables.)


\subsection{Proofs for Propositional Logic}

Given a |Prop|osition |p| and a proof |t| (represented as
an element of another type |Proof|), we can write a function that checks
that |t| is a valid proof of |p|:

\begin{code}
checkProof :: Proof -> Prop -> Bool
\end{code}

But we still have to figure out what consitutes proofs.

To prove |And P Q|, one needs simultaneously of |P| and a proof of
|Q|. (In logic texts, one will often find
\[ \frac{P \quad Q}{P ∧ Q} \] to represent this fact, which is called the \emph{introduction rule for (∧)})
(For the proof to be complete, one still needs to provide a full proof of |P| and another for |Q| --- it is not enough to just invoke this rule.)

Therefore, in Haskell, can represent this rule by a proof-term constructor |AndIntro| with two |Proof| arguments:
\begin{spec}
AndIntro :: Proof -> Proof -> Proof
\end{spec}
%
and, the corresponding case of the |checkProof| function will look like this:

\begin{spec}
checkProof (AndIntro t u) (And p q) = checkProof t p && checkProof u q
\end{spec}

To prove |Or P Q|, we need either a proof of |P| or proof of |Q| --- but
we need to know which side (|Left| for |p| or |Right| for |q|) we
refer to.  Therefore, we need a proof-term constructor:

\begin{spec}
OrIntro :: Either Proof Proof -> Proof
\end{spec}
For reference, the either type is defined as follows in the Haskell prelude:
%include Either.lhs

There are a couple of possible approaches to deal with negation.
One approach is to define it as de Morgan dualisation:
\begin{spec}
Not (a `Or` b) = Not a `And` Not b
Not (a `And` b) = Not a `Or` Not b
...
\end{spec}
Negation can then only apply to names, which can recieve a special treatment in proof-checking.

However, we will instead apply the same treatment to negation as to other constructions, and define
a suitable introduction rule:
\[ \frac{P → Q \quad P → ¬Q}{¬P} \]. We can represent it by the
|NegIntro :: Prop -> Proof -> Proof -> Proof| constructor.

Because we have inductive proofs (described from the bottom up),
we have the additional difficulty that this rule conjures-up a new
proposition, $Q$. This is why we need an additional |Prop| argument,
which gives the |Q| formula.

There is no way to introduce Falsity (⊥), otherwise we'd have an inconsistent logic!
Finally we can introduce "Truth", with no premiss:\(\frac{}{⊤}\). The proof has no information either |TruthIntro :: Proof|.

To complete the system, in addition to introduction rules (where the connective appears as
conclusion), we also need elimination rules (where the connective
appears as premiss). For conjuction, we have two eliminations rules: $\frac{P ∧ Q}{P}$ and
$\frac{P ∧ Q}{Q}$.  So we represent them by |AndElim1 :: Proof -> Prop -> Proof| (and |AndElim2| symmetrically),
where the extra |Prop| argument corresponds to |Q|.

Our elimination rule for disjunction is $\frac {P ∨ Q \quad P → R \quad Q →
R} R$. The idea here is that if we know that \(P ∨ Q\) holds, then we
have two cases: either |P| holds and |Q| holds. If only we can find a
proposition |R| which is a consequence of both |P| and |Q|, then,
regardless of which case we are facing, we know that |R| will hold.

Our elimination for negation is $\frac {¬ ¬ P} P$. It simply says that two negations cancel out.

Finally we can eliminate Falsity as follows: $\frac {⊥} P$. This rule
goes some times by its descriptive latin name \textit{ex falso
quodlibet} --- from falsehood, anything (follows).

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

Any other combination of proof/prop else is an incorrect combination: the proof is not valid for the proposition.

\begin{spec}
checkProof _ _ = False -- incorrect proof
\end{spec}

At this point it can be interesting to, see |checkProof| as an
evaluator again, by, and flipping its arguments: |flip checkProof ::
Prop -> (Proof -> Bool)|. This way, one can understand |Proof ->
Bool|, a subset of propositions, as the semantic domain of |Prop|.  In
other words, a proposition can be interpreted at the subset of
propositions which prove it.


\subsection{Implication, hypothetical derivations, contexts}

We have so far omitted to deal with |Implies|. One reason is that we
can use the so-called material implication definition which we invoked
earlier in truth tables. It means to define |Implies a b = (Not a)
`Or` b| --- and this equality means that there is no need to deal
specially with |Implies|. However this approach does not bring any new
insight. In particular, this view is hard to transpose to more
complicated logics (such as second-order logic).

Thus we take our usual approach and give rules for it. The introduction rule is sometimes written like so in logic texts: \[\frac{\begin{array}{c}P \\ \vdots \\ Q \end{array}}{P → Q}\]
Such a notation can however be terribly confusing. We were used to the fact that proofs above the line had to be continued, so what can the dots possibly mean?
The intended meaning of this notation is that, to prove that $P → Q$, it suffices to prove $Q$, but one is also allowed to use $P$ as an assumption in this (local) proof of $Q$.

We can use our DSL to formalise this rule as Haskell data, by adding a
constructor corresponding to implication introduction: |ImplyIntro ::
(Proof -> Proof) -> Proof|.  The fact that the premiss can depend on
the assumption |Q| is represented by a function whose parameter is the
proof of |Q| in question.
%
In other words, to prove the formula |P -> Q| we assume a proof |t| of
|P| and derive a proof |u| of |Q|. So, a proof of an implication is a
function from proofs to proofs.

The eliminator for implication (also known as the hard-to-translate
phrase \textit{modus ponens}) is \(\frac{P → Q \quad P} Q\). We
formalise it as |ImplyElim :: Proof -> Proof -> Prop -> Proof| (The
proposition |P| is a not given by the conclusion and thus has to be
provided as part of the proof.). 
And we can finally complete our proof checker as follows:

\begin{code}
checkProof (Assumption p') p = p == p'
checkProof (ImplyIntro f) (p `Implies` q) = checkProof (f (Assumption p)) q
checkProof (ImplyElim t u p) q = checkProof t (q `Implies` p) && checkProof u q
checkProof _ _ = False -- incorrect proof
\end{code}

And, for reference, the complete DSL for proofs is given by the
following datatype:
\begin{code}
data Proof  =  Assumption Prop
            |  TruthIntro
            |  FalseElim Proof
            |  AndIntro Proof Proof
            |  AndElimL Proof Prop
            |  AndElimR Proof Prop
            |  OrIntro (Either Proof Proof)
            |  OrElim Proof Proof Proof Prop Prop
            |  NotIntro Proof Proof Prop
            |  NotElim Proof
            |  ImplyIntro (Proof -> Proof)
            |  ImplyElim  Proof Proof Prop
\end{code}


\paragraph{Aside}
The |Assumption| constructor may make the reader somewhat uneasy: how
come that we can simply assume anything? The intent is that this
constructor is \emph{private} to the |checkProof| function (or
module). No user-defined proof can use it. The most worried readers
can also define the following version of |checkProof|, which uses an
extra context to check that assumption have been rightfully introduced
earlier.  \footnote{This kind of presentation of the checker matches
  well the sequent calculus presentation of the proof system.}
 
\begin{spec}
checkProof :: Context -> Proof -> Prop -> Bool
checkProof ctx (ImplyIntro t) (p `Implies` q) = checkProof' (p:ctx) t q
checkProof ctx Assumption p = p `elem` ctx
\end{spec}

\paragraph{Example proof}
We can put our proof-checker to the test, by writing a number of
proofs and verifying them.

\begin{code}
conjunctionCommutative :: Prop
conjunctionCommutative = (p `And` q) `Implies` (q `And` p)
  where p = Name "p"; q = Name "q"

conjunctionCommutativeProof :: Proof
conjunctionCommutativeProof =
  ImplyIntro (\evPQ ->
  AndIntro  (AndElimR evPQ (Name "p"))
            (AndElimL evPQ (Name "q")) )

\end{code}
(where |evPQ| stands for "evidence for |P| and |Q|.")

We can then run the checker and verify:
|checkProof conjunctionCommutativeProof conjunctionCommutative == True|

%if False
\begin{code}
-- >>> checkProof conjunctionCommutativeProof conjunctionCommutative
-- True
\end{code}
%endif

\begin{exercise}
  Try to swap |AndElimL| and |AndElimR| in the above proof. What will
  happen and why?
%if False
\begin{code}
conjunctionCommutativeProof2 :: Proof
conjunctionCommutativeProof2 =
  ImplyIntro (\evPQ ->
  AndIntro  (AndElimL evPQ (Name "q"))
            (AndElimR evPQ (Name "p")) )

-- >>> checkProof conjunctionCommutativeProof2 conjunctionCommutative
-- False
\end{code}

%endif
\end{exercise}


\subsection{The Haskell type-checker as a proof checker}
\label{sec:haskell-as-proof checker}
Perhaps surprisingly, the proof-checker that we just wrote is already
built-in in the Haskell compiler. Let us clarify what we mean, using
the same example, but adapt it to let the type-checker do the work:

\begin{code}
conjunctionCommutativeProof' :: (p `And` q) `Implies` (q `And` p)
conjunctionCommutativeProof' =
   implyIntro (\evPQ ->
   andIntro  (andElimR evPQ)
             (andElimL evPQ))
\end{code}

That is, instead of writing propositions, we write types (|And|, |Or|,
|Implies|).  Instead of using |Proof| constructors, we use functions
whose types capture rules:
%
\begin{code}
truthIntro  :: Truth
falseElim   :: False -> p
andIntro    :: p -> q -> And p q
andElimL    :: a `And` b -> a
andElimR    :: a `And` b -> b
orIntro     :: Either a b -> Or a b
orElim      :: Or p q -> (p `Implies` r) -> (q `Implies` r) -> r
notIntro    :: (p `Implies` q) `And`  (p `Implies` Not q) -> Not p
notElim     :: Not (Not p) -> p
implyIntro  :: (p -> q) -> (p `Implies` q)
implyElim   :: (p `Implies` q) -> p -> q
\end{code}
Instead of running |checkProof|, we type-check the above program.
Because the proof is correct, we get no type-error.
\begin{exercise}
  What would happen if you swap |andElimR| and |andElimL|? Why?
\end{exercise}
%

This style of propositional logic proof is very advantageous, because
we not only the checker comes for free, but we additionally get all
the engineering tools of the Haskell tool-chain.

One should be careful however that Haskell is not design with
theorem-proving in mind.  For this reason it is easily possible make
the compiler accept invalid proofs. The main two sources of invalid
proofs are 1. non-terminating programs and 2. exception-raising
programs.


\subsection{Intuitionistic Propositional Logic and Simply Typed
  Lambda-Calculus, Curry-Howard isomorphism.}
\label{sec:intuitionistic-logic}
We can make the link beween Haskell and logic more tight if we
restrict ourselves to \emph{intuitionistic} logic.

One way to characterize intuitionistic logic is that it lacks native
support for negation. Instead, |Not p| is represented as |p `Implies`
False|:
\begin{code}
type Not p = p `Implies` False
\end{code}
The intuition behind this definition is the principle of proof by contradiction: if assuming |p| leads to a contradition (|False|),
then |p| must be false; so |Not p| should hold.

When doing this kind of definition, one gives up on |notElim|: there is no way to eliminate
(double) negation.
\begin{code}
notElim = error "not possible as such in intuitionistic logic"
\end{code}
On the other hand the introduction for negation becomes a theorem of the logic.
The formulation of the theorem is:
\begin{spec}
notIntro :: (p `Implies` q) `And` (p `Implies` (q `Implies` False)) -> p `Implies` False
\end{spec}
And its proof is:
\begin{code}
notIntro (evPimpliesQ,evPimpliesNotQ) evP =
    (evPimpliesNotQ `implyElim` evP ) `implyElim` (evPimpliesQ `implyElim` evP)
\end{code}

\label{sec:curry-howard}
By focusing on intuitionistic logic, we can give a \emph{typed}
representation for each of the formula constructors. Let us consider implication first.
|impIntro| and |impElim| seem to be conversion from and to functions, and so it
should be obvious that the representation of the implication formula is a function:

\begin{code}
type Implies p q = p -> q
implyElim f x = f x
implyIntro f x = f x
\end{code}

Conjunction is represented as pairs; that is, if |p : P| and |q : Q|
then the proof of |And P Q| should be a pair of |p| and |q|. The
elimination rules are projections. In code:
\begin{code}
type And p q = (p,q)
andIntro t u = (t,u)
andElimL = fst
andElimR = snd
\end{code}
Similarly, disjuction is represented as |Either|:
if |p : P| then |Left p : Or P Q| and if |q : Q| then |Right q : Or P
Q|.
\begin{code}
type Or a b = Either a b
orIntro x = x
orElim (Left   t) u _ = u t
orElim (Right  t) _ v = v t
\end{code}
We already had characterize or-elimination as case analysis, and,
indeed, this is how we implement it.

Truth is represented as the unit type:
\begin{code}
type Truth = ()
truthIntro = ()
\end{code}

And falsehood is represented as the \emph{empty} type:
\begin{code}
data False
falseElim x = case x of {}
\end{code}
Note that the case-analysis has nothing to take care of here.

In this way we can build proofs (``proof terms'') for all of
intuitionistic propositional logic (IPL). As we have seen, each such
proof term is a program in Haskell. Conversely, every program written
in this fragment of Haskell (functions, pairs, |Either|, no recursion
and full coverage of cases) can be turned into a proof in IPL. This
fragment is called the simply-typed lambda calculus (STLC) with sum and products.

\paragraph{The law of the excluded middle}
\label{sec:excluded-middle}
As an example of how intuitionism twists usual law, consider the law of the excluded middle,
which states that, for any proposition |P|, either |P| or |Not P| holds. For example, either
it rains or it does not rain. There is no ``middle compromise''.
If we attempt to prove |P `Or` (Not P)| in intuitionitic logic, we quickly find ourselves in a dead end.
Clearly, we cannot prove |P| for any |P|. Likewise |Not P|, or equivalently |P -> False| cannot be deduced.

What we have to do account for the fact that we cannot use negation elimination, and so
we have to make-do with proving |Not (Not Q)| instead of |Q|. This is exactly what
we have to do to prove  the law of excluded
middle.
We can then provide this Haskell-encoded proof:

\begin{code}
excludedMiddle :: Not (Not (p `Or` Not p))
-- to prove this, we can ...
excludedMiddle k = -- ... assume |Not (p `Or` (Not p))| and prove falsity.
   k -- So, we can prove falsity if we can prove |p `Or` (Not p)|.
   (Right  -- We can prove in particular the right case, |Not p|
     (\evP ->  -- ... by assuming that |p| holds, and prove falsity.
        k --  But again, we can prove falsity if we can prove |p `Or` (Not p)|.
        (Left -- This time, we can prove in particular the left case, |p| ...
          evP))) -- because we assumed it earlier!
\end{code}


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
  (a `Implies` (b `And` c)) `Iff` (a` Implies` b) `And` (a `Implies` c)
\end{spec}
(|Iff| refers to implication in both directions).
The proof of this formula closely follows the implementation of the transform.
%
Therefore we start with the two directions of the transform as functions:

\begin{code}
test1' :: (a -> (b, c)) -> (a->b, a->c)
test1' = \a2bc ->  ( \a -> fst (a2bc a)
                   , \a -> snd (a2bc a) )

test2' :: (a->b, a->c) -> (a -> (b, c))
test2' = \fg -> \a -> (fst fg a, snd fg a)
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
exercise. This typing rule for function application can be written as follows:

\[\frac{f : A → B \quad x : A}{f(x) : B}\]

After erasing the colon (:) sign and what comes before it, we obtain
\textit{modus ponens} --- implication elimination.

The \emph{Curry--Howard correspondence} is a general principle that
says that we can think of propositions as types, and proofs as
programs. This principle goes beyond propositional logic (and first
order logic, etc.): it applies to all sorts of logics and programming
languages, with various levels of expressivity and features.

\subsection{|Or| is the dual of |And|.}
Before moving on to our next topic, we make a final remark on |And| and |Or|.
%
Most of the properties of |And| have corresponding properties for
|Or|.  This can be explained one way by observing that they are de
Morgan duals. Another explanatino is that one can swap the direction
of the arrows in the types of the the role between introduction and
elimination. (Using our presentation, doing so requires applying
isomorphisms.)

%
%
%*TODO: Perhaps add an example with (q->p) -> (Not p -> Not q)
%*TODO: Perhaps add an example with (p->p')->(q->q')->(And p q -> And p q)
%*TODO: Perhaps add an example with (p->p')->(q->q')->(Or  p q -> Or  p q)
%*TODO: Explain that the values of type |And p q| can be seen as "proofs" (abstract or concrete).
%

