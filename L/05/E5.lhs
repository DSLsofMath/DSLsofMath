\subsection{Exercises}
\begin{exercise}
  Fill in the gaps in lectures on polynomials and power series.
  %
  In particular:

  \begin{itemize}

    \item Give a derivation for the definition of |*| for polynomials % (Lecture 10)

    \item Prove that, with the definition of |x = [0, 1]| we really
    have
%
    \begin{spec}
      as = a0 + a1 * x + a2 * x^2 + ... + an * x^n
    \end{spec}

  \end{itemize}
\end{exercise}
\begin{exercise}
  Complete the following definition
  \begin{spec}
    instance Num a => Num [a] where
      (+) = addP
      (*) = mulP
      -- ... TODO

    addP :: Num a => [a] -> [a] -> [a]
    addP = zipWith' (+)
    mulP :: Num a => [a] -> [a] -> [a]
    mulP = -- TODO
  \end{spec}
  %
  Note that |zipWith'| is almost, but not quite, the definition of
  |zipWith| from the standard Haskell prelude.
  % Hint: |zipWith as [] = []| but |zipWith' as [] = as|
  % blackboard/W5/20170213_114418.jpg
\end{exercise}


TODO: more exercises!
