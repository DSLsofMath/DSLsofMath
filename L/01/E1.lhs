\subsection{Exercises: complex numbers and DSLs}

%include E1_1.lhs
%include E1_2.lhs
%include E1_from_exams.lhs

%
\begin{exercise}
  Read the full chapter and complete the definition of the
  instance for |Num| for the datatype `ComplexSyn`.
  %
  Also add a constructor for variables to enable writing expressions
  like |(Var "z") :*: toComplex 1|.
\end{exercise}

\begin{exercise}
 Read the next few pages of Appendix I (in
  \citep{adams2010calculus}) defining the polar view of Complex Numbers
  and try to implement complex numbers again, this time based on
  magnitude and phase for the semantics.
\end{exercise}

\begin{exercise}
 Implement a simplifier |simp :: ComplexSyn r -> ComplexSyn r|
  that handles a few cases like |0 * x = 0|, |1 * x = x|, |(a + b) * c
  = a * c + b * c|, \ldots
  %
  What class context do you need to add to the type of |simp|?
\end{exercise}


\begin{exercise}


  If you have tPolymorphism TODO: prepare for the "tupling transform" by an exercise converting back and forth between

\begin{spec}
  a -> (b, c)
\end{spec}

and

\begin{spec}
  (a->b, a->c)
\end{spec}

\end{exercise}


\begin{exercise}
  TODO: formulate exercise to implement more efficient show using an
  accumulating parameter.
\end{exercise}

\begin{exercise}
  (From section \ref{sec:infseq}): How is |liftSeq1| related to
  |fmap|? |liftSeq0| to |conSeq|?
\end{exercise}
