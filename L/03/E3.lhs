%
\begin{exercise}
  To get a feeling for the Lagrange equations, let |L(t, q, v) =
  m*v^2/2 + m*g*q|, compute |expand w|, perform the derivatives and check if the equation is satisfied for
  \begin{itemize}
  \item |w1 = id| or
  \item |w2 = sin| or
  \item |w3 = (q0-).(g*).(/2).(powTo 2)|
%TODO: check
  \end{itemize}
\end{exercise}
% \begin{exercise}
%TODO: perhaps add a few more Lagrange equation examples: pendulum, double pendulum, several particles in a gravitational field, etc.
% See blackboard/W3/20170130_154754.jpg
% \end{exercise}

%TODO (DaHe): Exercise introducing deep vs shallow embedding using type classes
% also, maybe introduce the concept of using type classes to return a deep
% embedding by using the operators of the data type, and casting to the
% syntactic type
%
% ---------------------------------------------------------------------------
% IDEA: Use the card/hand datatypes from the blackjack haskell lab, and
% implement a deep and shallow embedding for hands (the shallow one just returns
% the sum of the cards, based on some score :: Card -> Iteger). Then show that
% when we introduce a new rule into the game, where the combination (syntax) of
% the cards in hand matters, the shallow embedding is not enough.
%
% This is the data type I have in mind:
%
% data Card = Card {rank :: Rank, suit :: Suit}
%             deriving(Eq, Show)

% data Suit = Hearts | Spades | Diamonds | Clubs
%     deriving(Eq, Show)

% data Rank = Numeric Integer | Jack | Queen | King | Ace
%     deriving(Eq, Show)

% data Hand = Empty | Add Card Hand
%     deriving(Eq, Show)
%
% I believe this is given (at least it was during my year) at the start of a
% lab, so it shouldn't spoil any solution. Alternatively, we could just use
% a different but similar data type.
% ---------------------------------------------------------------------------


%TODO (DaHe): Exercise asking to implement a deep and a shallow embedding of some data
% type, using knowlege acquired from above

%TODO (DaHe): Describe a thing, ask to implement DSL for that thing by introducing
% data type, deep shallow embedding, evaluator


%TODO (DaHe): One or two exercieses presenting the definition of a
% mathematical concept / equation, and then ask student to type the variables and
% expressions involved. Should be split into parts, similar to exam questions.
% (This should probably come first)

\subsection{Exercises from old exams}

\begin{exercise}
    \textit{From exam 2016-Practice}

    Consider the following text from
    \citeauthor{maclane1986mathematics}'s \textit{Mathematics: Form
    and Function} (page 168):

    \begin{quote}
      If $z = g(y)$ and $y = h(x)$ are two functions with continuous
      derivatives, then in the relevant range $z = g(h(x))$ is a
      function of $x$ and has derivative
    \end{quote}

     $$z' (x) = g'(y) * h'(x)$$

    Give the types of the elements involved (|x|, |y|, |z|, |g|, |h|,
    |z'|, |g'|, |h'|, |*| and ').
\end{exercise}

\begin{exercise}
  \textit{From exam 2016-03-16}

    Consider the following text from
    \citeauthor{maclane1986mathematics}'s \textit{Mathematics: Form
    and Function} (page 182):

   \begin{quote}
     In these cases one tries to find not the values of |x| which
     make a given function |y = f(x)| a minimum, but the values of a given
     function |f(x)| which make a given quantity a minimum.  Typically,
     that quantity is usually measured by an integral whose integrand is
     some expression |F| involving both |x|, values of the function |y =
     f(x)| at interest and the values of its derivatives - say an
     integral
   \end{quote}

    $$∫_a^b F(y, y', x)dx,\quad y = f(x).$$

    Give the types of the variables involved (|x|, |y|, |y'|, |f|, |F|,
    |a|, |b|) and the type of the four-argument integration operator:

    $$∫_.^. \cdot d\cdot$$

\end{exercise}


\begin{exercise}
  \textit{From exam 2016-08-23}

  In the simplest case of probability theory, we start with a
  \textit{finite}, non-empty set $\Omega$ of \textit{elementary events}.  \textit{Events} are
  subsets of $\Omega$, i.e. elements of the powerset of $\Omega$, (that is,
  $\mathcal{P} \Omega$).  a \textit{probability function} |P| associates to each
  event a real number between 0 and 1, such that

  \begin{itemize}

      \item |P| |∅ = 0|, |P| $\Omega = 1$

      \item |A| and |B| are disjoint (i.e., |A ∩ B = ∅|),
      then: |P A + P B = P (A ∪ B)|.
  \end{itemize}

    Conditional probabilities are defined as follows (\textit{Elementary
    Probability 2nd Edition}, Stirzaker 2003):

    \begin{quote}
      Let |A| and |B| be events with |P B > 0|.  given that |B|
      occurs, the \textit{conditional probability} that |A| occurs is denoted
      by |P(A| $\vert $ |B)| and defined by

      |P(A| $\vert$ |B) = P(A ∩ B) / P B|
    \end{quote}

\begin{enumerate}

    \item What are the types of the elements involved in the definition of
        conditional probability? \\ (|P|, ∩, /, $\vert$)

    \item In the 1933 monograph that set the foundations of contemporary
        probability theory, Kolmogorov used, instead of |P(A| $\vert$ |B)|, the expression
        $P_A\, B$.  Type this expression.  Which notation do you prefer (provide
        a \textit{brief} explanation).

\end{enumerate}
\end{exercise}




\begin{exercise}
\textit{From exam 2017-03}

(Note that this exam question was later included as one of the
examples in this chapter, see \ref{sec:typePartialDerivative}. It is
kept here in case you want to check if you remember it!)

Consider the following text from page 169 of Mac Lane [1968]: %\cite{maclane1986mathematics}:

\begin{quote}
  [...] a function |z = f (x, y)| for all points |(x, y)| in some open
  set |U| of the cartesian |(x, y)|-plane.
%
  [...] If one holds |y| fixed, the quantity |z| remains just a
  function of |x|; its derivative, when it exists, is called the
  \emph{partial derivative} with respect to |x|.
%
  Thus at a point |(x, y)| in |U| this derivative for |h ≠ 0| is
\end{quote}

\[
∂ z / ∂ x  =  f'_{x} (x, y) =
              \lim_{h \to 0} (f (x + h, y) - f (x, y)) / h
\]

What are the types of the elements involved in the equation on the
last line?
% (\(∂ z / ∂ x\), \(f'_{x}\), \((x,y)\),\(\lim\), \ldots)
%
You are welcome to introduce functions and names to explain your reasoning.
\end{exercise}

%TODO (by DaHe) This has to do with matrices, which have not been covered yet
% in W3, so should this exercise be here or later?. It doesn't actually seem to
% use any of the matrix DSL, so I think it should be solvable at this point
\begin{exercise}
\textit{From exam 2017-08-22}

Multiplication for matrices (from the matrix algebra DSL).

  Consider the following definition, from ``Linear Algebra'' by Donald
  H. Pelletier:

  \textbf{Definition:} If $A$ is an $m \times n$ matrix and $B$ is an
  $n \times p$ matrix, then the \emph{product}, $AB$, is an
  $m \times p$ matrix; the $(i, j)^{th}$ entry of $AB$ is the sum of
  the products of the pairs that are obtained when the entries from
  the $i^{th}$ row of the left factor, $A$, are paired with those from
  the $j^{th}$ column of the right factor, $B$.

  \begin{enumerate}
  \item Introduce precise types for the variables involved:
    $A$, $m$, $n$, $B$, $p$, $i$, $j$. You can write |Fin n| for the
    type of the values |{0, 1, ..., n-1}|.
  \item Introduce types for the functions |mul| and |proj|
    where $AB$ = |mul A B| and |proj i j M| = ``take the $(i, j)^{th}$
    entry of $M$''. What class constraints (if any) are needed on the
    type of the matrix entries in the two cases?
  \item Implement |mul| in Haskell. You may use the functions
    |row| and |col| specified by |row i M| = ``the $i^{th}$ row of
    $M$'' and |col j M| = ``the $j^{th}$ column of $M$''. You don't
    need to implement them and here you can assume they return plain
    Haskell lists.
  \end{enumerate}

\end{exercise}

\begin{exercise}
  (Extra material outside the course.)
  %
  In the same direction as the Lagrangian case study in section
  \ref{sec:Lagrangian} there are two nice blog posts about Hamiltonian
  dynamics: one
  \href{https://blog.jle.im/entry/introducing-the-hamilton-library.html}{introductory}
  and one \href{https://blog.jle.im/entry/hamiltonian-dynamics-in-haskell.html}{more advanced}.
  %
  It is a good exercise to work through the examples in these posts.
\end{exercise}
