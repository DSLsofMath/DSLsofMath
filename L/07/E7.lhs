\newpage
\section{Exercises}

Search the chapter for tasks marked ``Exercise''.

\begin{exercise}
\label{exc:Mstarcompose}
Compute |((M*) . e ) g g'|.
\end{exercise}

\begin{exercise}
\label{exc:Mstarhomomorphismcompose}
Matrix-matrix multiplication is defined in order to ensure a
homomorphism from |(*)| to |(.)|.
%
\begin{spec}
Forall M (Forall M' (((M' * M)*) == (M' *) . (M *))
\end{spec}
or in other words
\begin{spec}
H2((*),(*),(.))
\end{spec}
%
Work out the types and expand the definitions to verify that this
claim holds.
%
Note that one |(*)| is matrix-vector multiplication and the other
is matrix-matrix multiplication.
\end{exercise}

\begin{exercise}
\label{exc:MMmultAssoc}
Show that matrix-matrix multiplication is associative.
\end{exercise}

\begin{exercise}
\label{exc:Dmatrixpowerseries}
With |G = Nat| for the set of indices, write the
(infinite-dimensional) matrix representing |D| for power series.
\end{exercise}

\begin{exercise}
\label{exc:matrixIntegPoly}
Write the matrix \(I_n\) associated with integration of polynomials of
degree |n|.
\end{exercise}
%**TODO: Add this exercise (was Bonus exercise in 2019)
% * With G = N (N for the natural numbers), implement the derivative Deriv and the integral Integ as infinite-dimensional matrices.
% * For extra credit, prove that Deriv * Integ = Id (Where Id i j = if i == j then 1 else 0 is the identity matrix).
%
\begin{exercise}
\label{exc:NonDetExample1}
In the context of \refSec{sec:NonDetSys}: start with |v0 = e 2 +
e 3| and iterate |M*| a number of times, to get a feeling for the
possible evolutions.
%
What do you notice?
%
What is the largest number of steps you can make before the result is
the origin vector (just zero)?
%

Now change |M| to |M'| by inverting the arrow from |2| to |4| and
repeat the exercise.
%
What changes?
%
Can you prove it?
\end{exercise}

\begin{exercise}
\label{exc:StocExample1}

In the context of the example matrix |M| in \refSec{sec:StocSys}:
starting from state |0|, how many steps do you need to take before the
probability is concentrated in state |6|?
%

Now change |M| to |M'| by inverting the arrow from |2| to |4| and
repeat the exercise.
%
What can you say about the long-term behaviour of the system now?

\end{exercise}

\begin{exercise}
\label{exc:StocExample1Impl}
In the context of the example matrix |M| in \refSec{sec:StocSys}:
implement the example.
%
You will need to define the transition function of type |G -> (G ->
[0, 1])| returning the probability of getting from |g| to |g'|, and
the associated matrix.
\end{exercise}

% \begin{exercise}
% \label{exc:}
% \end{exercise}
%
% \begin{exercise}
% \label{exc:}
% \end{exercise}
%
% \begin{exercise}
% \label{exc:}
% \end{exercise}
%
% \begin{exercise}
% \label{exc:}
% \end{exercise}
%
% \begin{exercise}
% \label{exc:}
% \end{exercise}

\subsection{Exercises from old exams}

\begin{exercise}

  \textit{From exam 2017-03-14}

  Consider a non-deterministic system with a transition
  function |f : G -> [G]| (for |G={0..5}|) represented in the
  following graph

  \[\begin{tikzcd}[cells={nodes=draw}]
       & 1 \arrow{rr} \arrow{dr} &     & 5 \\
  0 \arrow{ur} & & 3 \arrow{ur} & \\
       & 2 \arrow{ul} \arrow{ur} & & 4 \arrow{ll} \arrow{ul}
  \end{tikzcd}\]

  The transition matrix can be given the type |m :: G -> (G -> Bool)|
  and the canonical vectors have type |e i :: G -> Bool| for |i| in |G|.

  \begin{enumerate}

  \item (General questions.) What do the canonical vectors represent?
  %
    What about non-canonical ones?
  %
    What are the operations on |Bool| used in the matrix-vector
    multiplication?

  \item (Specific questions.) Write the transition matrix |m| of the
    system.
  %
    Compute, using matrix-vector multiplication, the result of three
    steps of the system starting in state |2|.
  \end{enumerate}
\end{exercise}
