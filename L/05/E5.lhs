\subsection{Exercises}

%
% TODO: (by DaHe): None of these exercises (except part of the last one) have
% much to do with this chapter at all, perhaps they should be moved? But that
% would leave this subsection pretty empty...
%
% TODO (DaHe) Check if there's any exam exercises that could go here.

\begin{exercise}
  Complete the instance declarations for |FunExp| (for |Num|,
  |Fractional|, and |Floating|).
\end{exercise}

\begin{exercise}
  Complete the instance declarations for |(Double, Double)|, deriving
  them from the homomorphism requirement for |apply| (from the end of
  the week 4 lecture notes).
\end{exercise}

\begin{exercise}
  We now have three different ways of computing the derivative of a
  function such as |f x = sin x + exp (exp x)| at a given point, say
  |x = pi|.
  \begin{enumerate}

    \item Find |e :: FunExp| such that |eval e = f| and use |eval'|.

    \item Find an expression of (the first version of) type |FD Double|
    and use |apply|.

    \item Apply |f| directly to the appropriate |(x, x')| and use |snd|.

\end{enumerate}

  Do you get the same result?
\end{exercise}

\begin{exercise}
  Fill in the gaps in lectures on polynomials and power series.  In
  particular:

  \begin{itemize}

    \item Give a derivation for the definition of |*| for polynomials (Lecture
    10)

    \item Check that, with the definition of |x = [0, 1]| in Lecture 10 we
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
