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
\begin{exercise}
  Polynomial multiplication.
  %
  To get a feeling for the definition it can be useful to take it step
  by step, starting with some easy cases.
  %
  \begin{spec}
    mulP [] p = -- TODO
    mulP p [] = -- TODO
  \end{spec}
  % Answer: [] because 0*p=0
  \begin{spec}
    mulP [a] p = -- TODO
    mulP p [b] = -- TODO
  \end{spec}
  % Answer: map (a*) p and map (*b) p
  \begin{spec}
    mulP (0:as) p = -- TODO
    mulP p (0:bs) = -- TODO
  \end{spec}
  % Answer: 0:(mulP as p) and 0:(mulP p bs)
  Finally we reach the main case
  \begin{spec}
    mulP (a:as) q@(b:bs) = -- TODO
  \end{spec}
  % Answer: (a*b):(map (a*) bs `addP`  (as  `mulP`  q)

\end{exercise}


TODO: more exercises!
