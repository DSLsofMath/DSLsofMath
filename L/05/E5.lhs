\subsection{Exercises}

\begin{exercise}
  Fill in the gaps in lectures on polynomials and power series.  In
  particular:

  \begin{itemize}

    \item Give a derivation for the definition of |*| for polynomials % (Lecture 10)

    % TODO (by DaHe): What is meant by "check"? Test it using the DSL somehow,
      % or just convince that it is the case?
    \item Check that, with the definition of |x = [0, 1]| we
    really have
    \begin{spec}
      as = a0 + a1 * x + a2 * x^2 + ... + an * x^n
    \end{spec}

    % TODO(by DaHe) I don't think solve, idx ... are introduced until the next
    % chapter, so perhaps this exercise should be moved?
    \item Implement |idx'|, |sinx'| and |cosx'| using |solve| (Lecture 11)

    \item Complete the instance |Floating (PowerSeries a)| (Lecture 11).

  \end{itemize}
\end{exercise}
