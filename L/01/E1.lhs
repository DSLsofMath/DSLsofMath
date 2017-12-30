\newpage
\subsection{Exercises: complex numbers and DSLs}

%include E1_1.lhs
%include E1_2.lhs
%include E1_from_exams.lhs

%
\begin{exercise}
  Read the full chapter and complete the definition of the
  instance for |Num| for the datatype |ComplexSyn|.
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


\begin{exercise}\label{exc:tuplingE1}
  Functions and pairs (the ``tupling transform'').
  %
  From one function |f :: a -> (b, c)| returning a pair, you can
  always make a pair of two functions |pf :: (a->b, a->c)|.
  %
  Implement this transform:
  %
  \begin{spec}
    f2p :: (a -> (b, c)) -> (a->b, a->c)
  \end{spec}
  %

  Also implement the opposite transform:
  \begin{spec}
    p2f :: (a->b, a->c) -> (a -> (b, c))
  \end{spec}

  This kind of transformation if often useful, and it works also for
  |n|-tuples.

  % Solutions:
  %   f2p  =  \fg     ->  (fst . fg, snd . fg)
  %   p2f  =  \(f,g)  ->  \x -> (f x, g x)

\end{exercise}

\begin{exercise}
  There is also a ``dual'' to the tupling transform: to show this,
  implement these functions:
  %
  \begin{spec}
    s2p :: (Either b c -> a) -> (b->a, c->a)
    p2s :: (b->a, c->a) -> (Either b c -> a)
  \end{spec}
  %

  % Solutions:
  %   s2p  =  \fg     ->  (fg . Left, fg . Right)
  %   p2s  =  \(f,g)  ->  either f g

\end{exercise}

\begin{exercise}\label{exc:counting}
  Counting values.
  %
  Now assume we have |f2p|, |s2f|, etc used with three finite types
  with cardinalites |A|, |B|, and |C|.
  %
  (For example, the cardinality of |Bool| is |2|, the cardinality of
  |Weekday| is |7|, etc.)
  %
  Then what is the cardinality of |Either a b|? |(a, b)|? |a->b|? etc.
  %
  These rules for computing the cardinality suggests that |Either| is
  similar to sum, |(,)| is similar to product and |(->)| to (flipped)
  power.
  %
  These rules show that we can use many intuitions from high-school
  algebra when working with types.
\end{exercise}

\begin{exercise}\label{exc:funtupE1}
  Functions as tuples.
  %
  For any type |t| the type |Bool -> t| is basically ``the same'' as
  the type |(t,t)|.
  %
  Implement the two functions |isoR| and |isoL| forming an isomorphism:
  %
  \begin{spec}
    isoR :: (Bool -> t) -> (t, t)
    isoL :: (t, t) -> (Bool -> t)
  \end{spec}
  %
  and show that |isoL . isoR = id| and |isoR . isoL = id|.

% TODO: place this code in a list of solutions somewhere
% isoR f = (f False, f True)
% isoL (fa, tr) = \b-> case b of {False -> fa; True -> tr}
% TODO: complete the proof

% TODO: example: which function represents the pair (7, 3)?
% Answer: f False = 7; f True = 3

\end{exercise}
% \begin{exercise}
%   TODO: formulate exercise to implement more efficient |show| using an
%   accumulating parameter.
% \end{exercise}

\begin{exercise}\label{exc:fmap}
  From section \ref{sec:infseq}:
  %
  \begin{itemize}
  \item What does function composition do to a sequence? (composition
    on the left?, on the right?)
  \item How is |liftSeq1| related to |fmap|? |liftSeq0| to |conSeq|?
  \end{itemize}
%

\end{exercise}
