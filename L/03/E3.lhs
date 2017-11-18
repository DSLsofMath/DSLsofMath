% TODO (DaHe): Exercise introducing deep vs shallow embedding using type classes
% also, maybe introduce the concept of using type classes to return a deep
% embedding by using the operators of the data type, and casting to the
% syntactic type

% TODO (DaHe): Exercise asking to implement a deep and a shallow embedding of some data
% type, using knowlege acquired from above

% TODO (DaHe): Describe a thing, ask to implement DSL for that thing by introducing
% data type, type class, evaluator, (similar to Q1 from old exams)


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
  the definition of |a haslim L| (see section
  \ref{par:LimitOfSequence}) by

  TODO: check why |(ε > 0) → | is included here but not in \ref{par:LimitOfSequence}
  %
  \begin{spec}
     P a ε L = (ε > 0) → Exists (N : ℕ) (Forall (n : ℕ) ((n ≥ N) → (|an - L| < ε)))
  \end{spec}

\end{itemize}

\renewcommand{\labelenumi}{\alph{enumi}.}

\begin{exercise}
  Consider the statement:

  The sequence |{an} = (0, 1, 0, 1, ...)| does not converge.

  \begin{enumerate}
  \item Define the sequence |{an}| as a function |a : ℕ → ℝ|.
  \item The statement ``the sequence |{an}| is convergent'' is
    formalised as
    \begin{spec}
      Exists (L : ℝ) (Forall (ε : ℝ) (P a ε L))
    \end{spec}

    The formalisation of ``the sequence |{an}| is not convergent'' is
    therefore

    \begin{spec}
      ¬ Exists (L : ℝ) (Forall (ε : ℝ) (P a ε L))
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
  Consider the statement:

  The limit of a convergent sequence is unique.
  \begin{enumerate}
  \item There are many ways of formalising this in FOL.  For example:
    \begin{spec}
      let Q a L = Forall (ε : ℝ) (P a ε L)
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
