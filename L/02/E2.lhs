\newpage
\subsection{Exercises}

% TODO: Perhaps introduce GADT datatype notation use in some exercise solutions

\begin{exercise}
  Define a function for de Morgan dualisation.
\end{exercise}

\begin{exercise}
  Define a function to rewrite propositions into conjunctive normal form.
\end{exercise}


\begin{exercise}
  Define a function to rewrite propositions into disjunctive normal form.
\end{exercise}


\subsubsection{Exercises: abstract FOL}
\label{exercises-for-dslsofmath-week-2-2017}
\begin{code}
{-# LANGUAGE EmptyCase #-}
import PropositionalLogic
\end{code}



\paragraph{Short technical note}\label{short-technical-note}

For these first exercises on the propositional fragment of FOL
(introduced in \refSec{sec:PropFrag}), you might find it useful
to take a look at typed holes, a feature which is enabled by default
in GHC and available (the same way as the language extension
\texttt{EmptyCase} above) from version 7.8.1 onwards:
\url{https://wiki.haskell.org/GHC/Typed_holes}.

If you are familiar with Agda, these will be familiar to use.
%
In summary, when trying to code up the definition of some expression
(which you have already typed) you can get GHC's type checker to help
you out a little in seeing how far you might be from forming the
expression you want.
%
That is, how far you are from constructing something of the
appropriate type.

Take |example0| below, and say you are writing:

\begin{spec}
example0 e = andIntro (_ e)  _
\end{spec}

When loading the module, GHC will tell you which types your holes
(marked by ``\_'') should have for the expression to be type correct.

On to the exercises.
%
% TODO (by DaHe): My impression when playing around with these execises now is
% that they aren't particularly difficult (at least the first ones), but
% students just don't have any idea how to approach them at this point. I think
% the solution to this isn't necessarily to alter the exercises, but rather to
% add more examples (step-by-step solutions) of these types of proofs, to give
% students a feel for the overall workflow.
%
% One thing that could be altered though would be to maybe split some of the
% more involved exercises into sub-questions, so the students could be "guided"
% through their solution. This would also serve to give students a feel for the
% workflow when solving this type of exercises. In particular, the law of
% excluded middle might be a good candidate to be split in this manner (or even
% turned into an example), since I think that's the one pretty much everyone got
% stuck at last year.
%
\begin{exercise}
Prove these theorems (for arbitrary |p|, |q| and |r|):

\begin{spec}
  Impl (And p q) q
  Or p q -> Or q p
  (p->q) -> (Not q -> Not p)      -- call it |notMap|
  Or p (Not p)                    -- Hard. Use |notElim|, |notMap|, etc.
\end{spec}
For the hardest example it can be good to use ``theory exploration'':
try to combine the earlier theorems and rules to build up suitable
term for which |notMap| or |notElim| could be used.

\end{exercise}
\begin{exercise}
Translate to Haskell and prove the De Morgan laws:

\begin{spec}
  ¬ (p ∨ q) ⟷  ¬p ∧ ¬q
  ¬ (p ∧ q) ⟷  ¬p ∨ ¬q
\end{spec}

(translate equivalence to conjunction of two implications).

\end{exercise}
\begin{exercise}
%**TODO: perhaps remove to avoid confusion
  So far, the implementation of the datatypes has played no role.
  %
  To make this clearer: define the types for connectives in
  |AbstractFol| in any way you wish, e.g.:

\begin{spec}
  newtype And p q  =  A ()
  newtype Not p    =  B p
\end{spec}

etc.\ as long as you still export only the data types, and not the
constructors.
%
Convince yourself that the proofs given above still work and that the
type checker can indeed be used as a poor man's proof checker.

\end{exercise}
\begin{exercise}
 The introduction and elimination rules suggest that some
  implementations of the datatypes for connectives might be more
  reasonable than others. We have seen that the type of evidence for
  |p → q| is very similar to the type of functions |p -> q|, so it
  would make sense to define

\begin{spec}
  type Impl p q  =  (p -> q)
\end{spec}

Similarly, |∧-ElimL| and |∧-ElimR| behave like the functions |fst| and
|snd| on pairs, so we can take

\begin{spec}
  type And p q  =  (p, q)
\end{spec}

while the notion of proof by cases is very similar to that of writing
functions by pattern-matching on the various clauses, making |p ∨ q|
similar to |Either|:

\begin{spec}
  type Or p q   =  Either p q
\end{spec}

\begin{enumerate}
\item Define and implement the corresponding introduction and
  implementation rules as functions.
\item Compare proving the distributivity laws
\end{enumerate}

\begin{spec}
  (p ∧ q) ∨ r ⟷  (p ∨ r) ∧ (q ∨ r)
  (p ∨ q) ∧ r ⟷  (p ∧ r) ∨ (q ∧ r)
\end{spec}

using the ``undefined'' introduction and elimination rules, with
writing the corresponding functions with the given implementations of
the datatypes.
%
The first law, for example, requires a pair of functions:

\begin{spec}
  (  Either (p, q) r -> (Either p r, Either q r)
  ,  (Either p r, Either q r) -> Either (p, q) r
  )
\end{spec}

\textbf{Moral:} The natural question is: is it true that every time we
find an implementation using the ``pairs, |->|, |Either|'' translation
of sentences, we can also find one using the ``undefined''
introduction and elimination rules?
%
The answer, perhaps surprisingly, is \emph{yes}, as long as the
functions we write are total.
%
This result is known as \emph{the Curry--Howard isomorphism}.

\end{exercise}
\begin{exercise}
 Can we extend the Curry--Howard isomorphism to formulas with |¬|?
%
  In other words, is there a type that we could use to define |Not p|,
  which would work together with pairs, |->|, and |Either| to give a
  full translation of sentential logic?

  Unfortunately, we cannot.
  %
  The best that can be done is to define an empty type

\begin{code}
data Empty
\end{code}

and define |Not| as

\begin{spec}
type Not p   =  p  ->  Empty
\end{spec}

The reason for this definition is: when |p| is |Empty|, the type |Not
p| is not empty: it contains the identity

\begin{spec}
idEmpty :: Empty -> Empty
isEmpty evE = evE
\end{spec}

When |p| is not |Empty| (and therefore is true), there is no (total,
defined) function of type |p -> Empty|, and therefore |Not p| is
false.

Moreover, mathematically, an empty set acts as a contradiction:
%
there is exactly one function from the empty set to any other set,
namely the empty function.
%
Thus, if we had an element of the empty set, we could obtain an
element of any other set.

Now to the exercise:

Implement |notIntro| using the definition of |Not| above, i.e., find a
function

\begin{spec}
notIntro :: (p -> (q, q -> Empty)) -> (p -> Empty)
\end{spec}

Using

\begin{code}
contraHey :: Empty -> p
contraHey evE   =  case evE of {}
\end{code}

prove

\begin{spec}
(q ∧ ¬ q) → p
\end{spec}

You will, however, not be able to prove |p ∨ ¬ p| (try it!).

Prove

\begin{spec}
¬ p ∨ ¬ q  → ¬ (p ∧ q)
\end{spec}

but you will not be able to prove the converse.

\end{exercise}
\begin{exercise}
  The implementation |Not p = p -> Empty| is not adequate for
  representing all the closed formulas in FOL, but it is adequate for
  \emph{constructive logic} (also known as \emph{intuitionistic}).
%
  In constructive logic, the |¬ p| is \emph{defined} as |p -> ⊥|, and
  the following elimination rule is given for |⊥|: |⊥-Elim : ⊥ -> a|,
  corresponding to the principle that everything follows from a
  contradiction (``if you believe ⊥, you believe everything'').

  Every sentence provable in constructive logic is provable in
  classical logic, but the converse, as we have seen in the previous
  exercise, does not hold.
%
  On the other hand, there is no sentence in classical logic which
  would be contradicted in constructive logic.
%
  In particular, while we cannot prove |p ∨ ¬ p|, we \emph{can} prove
  (constructively!) that there is no |p| for which |¬ (p ∨ ¬ p)|,
  i.e., that the sentence |¬ ¬ (p ∨ ¬p)| is always true.

  Show this by implementing the following function:

  \begin{spec}
    noContra :: (Either p (p -> Empty) -> Empty) -> Empty
  \end{spec}

  Hint: The key is to use the function argument to |noContra| twice.

\end{exercise}

%\newpage
\begin{exercise}
  \textit{From exam 2016-08-23}

  Consider the classical definition of continuity:

  \begin{quote}
  \emph{Definition:} Let $X ⊆ ℝ$, and $c ∈ X$. A function
  $f : X \to ℝ$ is \emph{continuous at $c$} if for every $\epsilon > 0$,
  there exists $\delta > 0$ such that, for every $x$ in the domain of
  $f$, if $\lvert x - c \rvert < \delta$, then $\lvert f x - f c \rvert < \epsilon$.
  \end{quote}

  \begin{enumerate}

  \item Write the definition formally, using logical connectives
    and quantifiers.

  \item Introduce functions and types to simplify the
    definition.

  \item Prove the following proposition: If \texttt{f} and
    \texttt{g} are continuous at \texttt{c}, \texttt{f\ +\ g} is
    continuous at \texttt{c}.

  \end{enumerate}
\end{exercise}

\begin{exercise}

  \textit{From exam 2017-08-22}

  Adequate notation for mathematical concepts and proofs
  (or ``50 shades of continuity'').

  A formal definition of ``$f : X \to ℝ$ is continuous'' and ``$f$ is
  continuous at $c$'' can be written as follows (using the helper
  predicate |Q|):

  \begin{spec}
    C(f)        =  ∀ c : X? Cat(f,c)
    Cat(f,c)    =  ∀ ε > 0? ∃ δ > 0? Q(f,c,ε,δ)
    Q(f,c,ε,δ)  =  ∀ x : X?  absBar(x - c) < δ  ⇒  absBar(f x - f c) < ε
  \end{spec}

  By moving the existential quantifier outwards we can introduce the
  function |getδ| which computes the required |δ| from |c| and |ε|:

  \begin{spec}
      C'(f)       =  ∃ getδ : X -> RPos -> RPos? ∀ c : X? ∀ ε > 0? Q(f,c,ε,getδ c ε)
  \end{spec}

  Now, consider this definition of \emph{uniform continuity}:

  \begin{quote}
    \textbf{Definition:} Let $X ⊆ ℝ$.  A function $f : X \to ℝ$ is
    \emph{uniformly continuous} if for every $\epsilon > 0$, there exists $\delta > 0$
    such that, for every $x$ and $y$ in the domain of $f$, if |absBar (x - y)
    < δ |, then |absBar (f x - f y) < ε|.
  \end{quote}

  \begin{enumerate}

  \item Write the definition of |UC(f)| = ``|f| is uniformly
    continuous'' formally, using logical connectives and quantifiers.
    Try to use |Q|.

  \item Transform |UC(f)| into a new definition |UC'(f)| by
    a transformation similar to the one from |C(f)| to |C'(f)|.
    Explain the new function |newδ| introduced.

  \item Prove that |∀ f : X -> ℝ? UC'(f) => C'(f)|. Explain
    your reasoning in terms of |getδ| and |newδ|.

  \end{enumerate}
\end{exercise}

\subsubsection{More exercises}


Preliminary remarks

\begin{itemize}
\item when asked to ``sketch an implementation'' of a function, you
  must explain how the various results might be obtained from the
  arguments, in particular, why the evidence required as output may
  result from the evidence given as input.
  %
  You may use all the facts you know (for instance, that addition is
  monotonic) without formalisation.

\item to keep things short, let us abbreviate a significant chunk of
  the definition of |a haslim L| (see
  \refSec{par:LimitOfSequence}) by
  \begin{spec}
     P : Seq X -> X -> RPos -> Prop
     P a L ε = Exists (N : ℕ) (Forall (n : ℕ) ((n ≥ N) → (absBar (an - L) < ε)))
  \end{spec}

\end{itemize}

\renewcommand{\labelenumi}{\alph{enumi}.}

\begin{exercise}\label{exc:NotConv}
  Consider the statement:

  The sequence |{an} = (0, 1, 0, 1, ...)| does not converge.

  \begin{enumerate}
  \item Define the sequence |{an}| as a function |a : ℕ → ℝ|.
  \item The statement ``the sequence |{an}| is convergent'' is
    formalised as
    \begin{spec}
      Exists (L : ℝ) (Forall (ε > 0) (P a L ε))
    \end{spec}

    The formalisation of ``the sequence |{an}| is not convergent'' is
    therefore

    \begin{spec}
      ¬ Exists (L : ℝ) (Forall (ε > 0) (P a L ε))
    \end{spec}

    Simplify this expression using the rules

    \begin{spec}
     ¬ (Exists x (P x)) ⟷  (Forall x (¬ (P x)))
     ¬ (Forall x (P x)) ⟷  (Exists x (¬ (P x)))
     ¬ (P → Q)  ⟷  P ∧ ¬ Q
    \end{spec}

    The resulting formula should have no |¬| in it (that's possible
    because the negation of |<| is |≥|).
  \item Give a functional interpretation of the resulting formula.
  \item Sketch an implementation of the function, considering two
    cases: |L ≠ 0| and |L = 0|.
  \end{enumerate}
\end{exercise}


\begin{exercise}
Same as Exercise \ref{exc:NotConv} but for |a = id|.
\end{exercise}

\begin{exercise}
  Consider the statement:

  The limit of a convergent sequence is unique.
  \begin{enumerate}
  \item There are many ways of formalising this in FOL.  For example:
    \begin{spec}
      let Q a L = Forall (ε > 0) (P a L ε)
      in  Forall (L₁ : ℝ) (Forall (L₂ : ℝ) ( (Q a L₁ ∧ Q a L₂) → L₁ = L₂) )
    \end{spec}

    i.e., if the sequence converges to two limits, then they must be equal, or

    \begin{spec}
      Forall (L₁ : ℝ) (Forall (L₂ : ℝ) ( Q a L₁ ∧ L₁ ≠ L₂  →  ¬ Q a L₂) )
    \end{spec}

    i.e., if a sequence converges to a limit, then it doesn't
    converge to anything that isn't the limit.

    Simplify the latter alternative to eliminate the negation and
    give functional representations of both.
  \item Choose one of the functions and sketch an implementation of
    it.
  \end{enumerate}
\end{exercise}

\begin{exercise}
  Propositions as polynomials.

  One way to connect logic to calculus is to view propositions as
  polynomials (in several variables).
  %
  The key idea is to represent the truth values by zero (False) and
  one (True) and each named proposition |P| by a fresh variable |p|.
  %
  To represent operations one just has to check that normal expression
  evaluation gives the right answer for zero and one.

  The simplest operation to represent is |And| which becomes
  multiplication: |P And Q| translates to \(p q\) as can be easily
  checked.
  %
  (Note that |p+q| does not represent any proposition, because its
  value would be |2| for |p=q=1|, but |2| does not represent any
  boolean.)

  How should |Not|, |Or|, and |Implies| be represented?

% Not p = 1-p
% Or p q = Not (And (Not p) (Not q)) = 1-(1-p)(1-q) = 1-(1-p-q+pq) = p+q-pq
% Implies p q = Not (And p (Not q)) = 1-p(1-q) = 1-p+pq

\end{exercise}
