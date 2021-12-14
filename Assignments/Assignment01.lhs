\documentclass{article}
\usepackage{amsmath}
\usepackage{a4wide}
\usepackage{natbib}
\usepackage{url}
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8x]{inputenc}
\RequirePackage{ucs}
\RequirePackage{amsfonts}
%include polycode.fmt
%include ../L/dslm.format
\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt plus 2pt minus 1pt}
% See https://github.com/jgm/pandoc/issues/5801
\renewcommand{\linethickness}{0.5pt}
\providecommand{\textepsilon}{\ensuremath{\epsilon}}
\begin{document}
\title{DSLsofMath 2022: Assignment 1}
\author{Patrik Jansson and Sólrún Einardóttir and Víctor López Juan}
\date{}
\maketitle
\section{DSLsofMath 2022: Assignment 1}

In this assignment the focus is on the following three learning
outcomes:

\begin{itemize}

\item organize areas of mathematics in DSL terms
\item develop adequate notation for mathematical concepts
\item discuss and compare different software implementations of
  mathematical concepts
\end{itemize}

\subsection{DSLs, sets and von Neumann}
\label{dsls-sets-and-von-neumann}

In this assignment you will build up a domain-specific language (a
DSL) for finite sets.
%
The domain you should model is pure set theory where all members are
sets.

Define a datatype |TERM v| for the abstract syntax of set expressions with
variables of type |v| and a datatype |PRED v| for predicates over pure set
expressions.

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

\paragraph{Part 1.} |TERM| should have constructors for

\begin{itemize}

\item the |Empty| set
\item the one-element set constructor |Singleton|
\item |Union|, and |Intersection|
  \begin{itemize}
  \item you can also try |Powerset|
  \end{itemize}
\item set-valued variables (|Var :: v -> TERM v|)
\end{itemize}

|PRED| should have contructors for

\begin{itemize}

\item the two predicates |Elem|, |Subset|
\item the logical connectives |And|, |Or|, |Implies|, |Not|
\end{itemize}

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

\paragraph{Part 2.} A possible semantic domain for pure sets is

\begin{code}
newtype Set = S [Set]
\end{code}

Implement the evaluation functions

\begin{code}
eval   :: Eq v => Env v Set ->  TERM v   -> Set
check  :: Eq v => Env v Set ->  PRED v  -> Bool
\end{code}

\begin{code}
type Env var dom = [(var , dom)]
\end{code}

Note that the type parameter |v| to |TERM| is for the type of variables
in the set expressions, not the type of elements of the sets. (You can
think of pure set theory as ``untyped'' or ``unityped''.)

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

\paragraph{Part 3.} The von Neumann encoding of natural numbers as
sets is defined recursively as

\begin{spec}
  vonNeumann 0        =  Empty
  vonNeumann (n + 1)  =  Union  (vonNeumann n)
                                (Singleton (vonNeumann n))
\end{spec}

Implement |vonNeumann| and explore, explain and implement the following
``pseudocode'' claims as functions in Haskell:

\begin{spec}
  claim1 n1 n2  =  {- if |(n1 <= n2)|  then  |(n1 ⊆ n2)| -}
  claim2 n      =  {- |n = {0, 1, ..., n - 1}| -}
\end{spec}

You need to insert some embeddings and types and you should use the
|eval| and |check| functions. (For debugging it is useful to implement
a |show| function for |Set| which uses numerals to show the von
Neumann naturals.)

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

Admin:

\begin{itemize}
\item \emph{Submission}: Assignments are to be submitted via Canvas
\item \emph{Deadline}: Tueday 2020-02-04
\item \emph{Grading}: Discussions with each of the teams during the
  slot 2020-02-10, 8.30–12 or 13.30–17
\end{itemize}

Note: The examination will be in English.
\end{document}
