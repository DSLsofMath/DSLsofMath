\subsection{Exercises}

Search the chapter for tasks marked ``Exercise''.

TODO: Maybe convert these to proper exercises.

\subsubsection{Exercises from old exams}

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
