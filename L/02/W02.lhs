\section{Week 2}

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


\subsection{A few words about pure set theory}

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

\begin{code}
vonNeumann 0        =  Empty
vonNeumann (n + 1)  =  Union  (vonNeumann n)
                              (Singleton (vonNeumann n))
\end{code}

\paragraph{Pairs}

Definition:  A pair |(a,b)| is encoded as |{{a},{a,b}}|.

% (a,b) \coloneqq \big\{\,\{a\},\{a,b\}\,\big\} ;

\subsection{Propositional Calculus}

Swedish: Satslogik

False, True, And, Or, Implies

\subsection{First Order Logic (predicate logic)}

Swedish: Första ordningens logik = predikatlogik

Adds term variables and functions, predicate symbols and quantifiers (sv: kvantorer).
