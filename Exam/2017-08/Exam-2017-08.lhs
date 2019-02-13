% -*- latex -*-
\documentclass[twoside]{article}
\usepackage{tikz-cd}
\usepackage{a4wide}
\usepackage{amsmath,amssymb}
\usepackage{mathrsfs}
\usepackage{url}
\usepackage{multicol}
\RequirePackage[T1]{fontenc}
\RequirePackage[mathletters]{ucs}
\RequirePackage[utf8x]{inputenc}
\RequirePackage{amsfonts}
\setlength{\parindent}{0ex}
\setlength{\parskip}{1ex plus 1 ex minus 0.5ex}

% the `doubleequals' macro is due to Jeremy Gibbons
\def\doubleequals{\mathrel{\unitlength 0.01em
  \begin{picture}(78,40)
    \put(7,34){\line(1,0){25}} \put(45,34){\line(1,0){25}}
    \put(7,14){\line(1,0){25}} \put(45,14){\line(1,0){25}}
  \end{picture}}}

\newcommand{\tyconsym}[1]{\mathrel{{:}{#1}{:}}}
%format :+:                    = "\tyconsym{+}"
%format :*:                    = "\tyconsym{*}"
%format :/:                    = "\tyconsym{/}"
%%% Use ? to end the variable binding part of forall and exists to avoid clash with . for composition
%format ? = ".~"
%format ?? = "?"
%format RPos                    =  "\mathbb{R}_{> 0}"
%format (abs (x)) = "|" x "|"

\newcommand\todo[1]{\textbf{TODO\{}#1\textbf{\}}}

%include polycode.fmt
%include forall.fmt
%format == = "\doubleequals"
\title{Domain Specific Languages of Mathematics\\Course codes: DAT326 / DIT982}
\author{Patrik Jansson}
\date{2017-08-22}

\begin{document}
\maketitle

\begin{description}
\item[Contact] Patrik Jansson (x5415)
\item[Results] Announced within 19 days
\item[Exam check] Fri. 2017-09-01 in EDIT 5468 at 12.30-12.55
\item[Aids] One textbook of your choice (e.g., Adams and Essex, or
  Rudin).  No printouts, no lecture notes, no notebooks, etc.
\item[Grades] 3: 40p, 4: 60p, 5: 80p, max: 100p
\end{description}

Remember to write legibly. Good luck!

\hfill

For reference: the DSLsofMath learning outcomes. Some are tested by
the hand-ins, some by the written exam.

\begin{itemize}
\item Knowledge and understanding
  \begin{itemize}
  \item design and implement a DSL (Domain Specific Language) for a
    new domain
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


\newpage

\begin{enumerate}
\item {[30pts]} Algebraic structure: a DSL for semirings.

  A semiring is a set |R| equipped with two binary operations |+| and
  |⋅|, called addition and multiplication, such that:
\begin{itemize}

\item |(R, +, 0)| is a commutative monoid with identity element |0|:

>     (a + b) + c = a + (b + c)
>     0 + a = a + 0 = a
>     a + b = b + a

\item |(R, ⋅, 1)| is a monoid with identity element |1|:

>     (a⋅b)⋅c = a⋅(b⋅c)
>     1⋅a = a⋅1 = a

\item Multiplication left and right distributes over |(R, +, 0)|:

>     a⋅(b + c) = (a⋅b) + (a⋅c)
>     (a + b)⋅c = (a⋅c) + (b⋅c)
>     a⋅0 = 0⋅a = 0

\end{itemize}

  \begin{enumerate}
  \item Define a type class |SemiRing| that corresponds to the
    semiring structure.

  \item Define a datatype |SR v| for the language of semiring
    expressions (with variables of type |v|) and define a |SemiRing|
    instance for it. (These are expressions formed from applying the
    semiring operations to the appropriate number of arguments, e.g.,
    all the left hand sides and right hand sides of the above
    equations.)

  \item Find two other instances of the |SemiRing| class. \label{item:inst}

  \item Give a type signature for, and define, a general evaluator for
    |SR v| expressions on the basis of an assignment function.

  \item Specialise the evaluator to the two |SemiRing| instances
    defined in (\ref{item:inst}).  Take three semiring expressions of
    type |SR String|, give the appropriate assignments and compute the
    results of evaluating, in each case, the three expressions.

  \end{enumerate}
  Each question carries 6pts.

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}


\item {[20pts]} Multiplication for matrices (from the matrix algebra DSL).

  Consider the following definition, from ``Linear Algebra'' by Donald
  H. Pelletier:

  \textbf{Definition:} If $A$ is an $m \times n$ matrix and $B$ is an
  $n \times p$ matrix, then the \emph{product}, $AB$, is an
  $m \times p$ matrix; the $(i, j)^{th}$ entry of $AB$ is the sum of
  the products of the pairs that are obtained when the entries from
  the $i^{th}$ row of the left factor, $A$, are paired with those from
  the $j^{th}$ column of the right factor, $B$.

  \begin{enumerate}
  \item{} [7pts] Introduce precise types for the variables involved:
    $A$, $m$, $n$, $B$, $p$, $i$, $j$. You can write |Fin n| for the
    type of the values |{0, 1, ..., n-1}|.
  \item{} [6pts] Introduce types for the functions |mul| and |proj|
    where $AB$ = |mul A B| and |proj i j M| = ``take the $(i, j)^{th}$
    entry of $M$''. What class constraints (if any) are needed on the
    type of the matrix entries in the two cases?
  \item{} [7pts] Implement |mul| in Haskell. You may use the functions
    |row| and |col| specified by |row i M| = ``the $i^{th}$ row of
    $M$'' and |col j M| = ``the $j^{th}$ column of $M$''. You don't
    need to implement them and here you can assume they return plain
    Haskell lists.
  \end{enumerate}

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

\newpage

\item {[25pts]} Consider the following differential equation:

    \[f'' \,t -3 \sqrt{2} * f' \, t + 4 * f \, t = 0, \quad f\, 0 = 2,\quad f'\, 0 = 3 \sqrt{2}\]

  \begin{enumerate}
  \item {[10pts]} Solve the equation assuming that |f| can be
    expressed by a power series |fs|, that is, use |integ| and the
    differential equation to express the relation between |fs|, |fs'|,
    and |fs''|.
    %
    What are the first three coefficients of |fs|?

  \item {[15pts]} Solve the equation using the Laplace transform.  You
    should need this formula (and the rules for linearity +
    derivative):

    \[ℒ\, (\lambda t.\, e^{\alpha*t})\, s  = 1 / (s - \alpha)\]
  \end{enumerate}

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

\item {[25pts]} Adequate notation for mathematical concepts and proofs
  (or ``50 shades of continuity'').

  A formal definition of ``$f : X \to ℝ$ is continuous'' and ``$f$ is
  continuous at $c$'' can be written as follows (using the helper
  predicate |Q|):

< C(f)        =  ∀ c : X? Cat(f,c)
< Cat(f,c)    =  ∀ ε > 0? ∃ δ > 0? Q(f,c,ε,δ)
< Q(f,c,ε,δ)  =  ∀ x : X?  abs(x - c) < δ  ⇒  abs(f x - f c) < ε

By moving the existential quantifier outwards we can introduce the
function |getδ| which computes the required |δ| from |c| and |ε|:

< C'(f)       =  ∃ getδ : X -> RPos -> RPos? ∀ c : X? ∀ ε > 0? Q(f,c,ε,getδ c ε)

Now, consider this definition of \emph{uniform continuity}:

\textbf{Definition:} Let $X ⊆ ℝ$.  A function $f : X \to ℝ$ is
\emph{uniformly continuous} if for every $ε > 0$, there exists $δ > 0$
such that, for every $x$ and $y$ in the domain of $f$, if |abs (x - y)
< δ|, then |abs (f x - f y) < ε|.

  \begin{enumerate}
  \item{} [5pts] Write the definition of |UC(f)| = ``|f| is uniformly
    continuous'' formally, using logical connectives and quantifiers.
    Try to use |Q|.
  \item{} [10pts] Transform |UC(f)| into a new definition |UC'(f)| by
    a transformation similar to the one from |C(f)| to |C'(f)|.
    Explain the new function |newδ| introduced.
  \item{} [10pts] Prove that |∀ f : X -> ℝ? UC'(f) => C'(f)|. Explain
    your reasoning in terms of |getδ| and |newδ|.
  \end{enumerate}


\end{enumerate}
\end{document}
