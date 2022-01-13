\newpage
\section{Exercises: Haskell, DSLs and expressions}
\label{sec:ch1ex}

%include E1_1.lhs
%include E1_2.lhs

\begin{exercise}\label{exc:counting}\textbf{[Important]}
  Counting values.
  %
  Assume we have three finite types |a|, |b|, |c| with cardinalities
  |A|, |B|, and |C|.
  %
  (For example, the \addtoindex{cardinality} of |Bool| is |2|, the cardinality of
  |Weekday| is |7|, etc.)
  %
  Then what is the cardinality of the types |Either a b|? |(a, b)|? |a->b|? etc.
  %
\index{Either@@|Either| type}%
\index{pair type}%
\index{function type}%
  %
  These rules for computing the cardinality suggests that |Either| is
  similar to sum, |(,)| is similar to product and |(->)| to (flipped)
  power.
  %
  These rules show that we can use many intuitions from high-school
  algebra when working with types.
\end{exercise}

\begin{exercise}{Counting |Maybe|s.} For each of the following types, enumerate and
  count the values:
\index{Maybe@@|Maybe| type}%
  \begin{enumerate}
  \item |Bool -> Maybe Bool|
  \item |Maybe Bool -> Bool|
  \item |Maybe (Bool, Maybe (Bool, Maybe Bool))|
  \end{enumerate}
%
  This is an opportunity to practice the learning outcome ``develop
  adequate notation for mathematical concepts'': what is a suitable
  notation for values of type |Bool|, |Maybe a|, |a->b|, |(a,b)|, etc.?
%
%TODO: Solve in the book to show ``suitable notation''. But then also remove as recommended hand-in exercise.
%
  For those interested, \citet{DBLP:conf/haskell/DuregardJW12} present
  a Haskell library
  (\href{https://hackage.haskell.org/package/testing-feat}{\texttt{testing-feat}})
  for enumerating syntax trees.
\end{exercise}

\begin{exercise}\label{exc:funtupE1}
  Functions as tuples.
\index{pair type}%
\index{function type}%
\index{Bool@@|Bool| (|BB|)}%
  %
  For any type |t| the type |Bool -> t| is basically ``the same'' as
  the type |(t,t)|.
  %
  Implement the two functions |isoR| and |isoL| forming an
  \addtoindex{isomorphism}:
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
% Proof:
%   LHS1
% =
%   isoL . isoR
% =
%   \f -> isoL (isoR f)
% =
%   \f -> isoL (f False, f True)
% =
%   \f -> \b-> case b of {False -> f False; True -> f True}
% Now we have two cases:
%   LHS1 f False
% =
%   case False of {False -> f False; True -> f True}
% =
%   f False
% =
%   id f False
% =
%   RHS1 f False
% ... and a very similar case for LHS1 f True

% Next law:   |isoR . isoL = id|
%   LHS2
% =
%   isoR . isoL
% =
%   \(x, y) -> isoR (isoL (x, y))
% =
%   \(x, y) -> isoR (\b-> case b of {False -> x; True -> y})
% =
%   \(x, y) -> let f = \b-> case b of {False -> x; True -> y} in (f False, f True)
% =
%   \(x, y) -> (x, y)
% =
%   id

% TODO: example: which function represents the pair (7, 3)?
% Answer: f False = 7; f True = 3

\end{exercise}
\begin{exercise}\label{exc:tuplingE1}\textbf{[Important]}
  Functions and pairs (the ``\addtoindex{tupling transform}'').
  %
  From one function |f :: a -> (b, c)| returning a pair, you can
  always make a pair of two functions |pf :: (a->b, a->c)|.
  %
\index{pair type}%
\index{function type}%
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
\index{Either@@|Either| type}%
\index{pair type}%
\index{function type}%
  %
  \begin{spec}
    s2p :: (Either b c -> a) -> (b->a, c->a)
    p2s :: (b->a, c->a) -> (Either b c -> a)
  \end{spec}
\end{exercise}
\begin{exercise}\label{exc:fmap}
  From \refSec{sec:infseq}:
  %
  \begin{itemize}
\index{function composition (|.|)}%
  \item What does function composition do to a \addtoindex{sequence}?
%
    More concretely: for a sequence |a| what is |a . (1+)|?
    %
    What is |(1+) . a|?

%(composition on the left?, on the right?)

\item How is |liftSeq1| related to |fmap|? |liftSeq0| to |conSeq|?
  \end{itemize}
%
\end{exercise}

\begin{exercise}
  Operator sections.
  %
  \index{operator section}%
  %
  Please fill out the remaining parts of this table with simplified
  expressions:
%
\begin{align*}
    |(1+)| &= |\x->1+x| \\
    |(*2)| &= |\x->x*2| \\
    |(1+).(*2)| &= \\
    |(*2).(1+)| &= \\
           &= |\x->x^2+1| \\
           &= |\x->(x+1)^2| \\
    |(a+).(b+)| &= \\
\end{align*}
%  Sections, flip, (.), and friends
\end{exercise}

%
\begin{exercise}
  Read the full chapter and complete the definition of the
  instance for |Num| for the datatype |ComplexSyn|.
  %
  Also add a constructor for variables to enable writing expressions
  like |(Var "z") :*: toComplex 1|.
\end{exercise}

% \begin{exercise}
%   TODO: formulate exercise to implement more efficient |show| using an
%   accumulating parameter.
% \end{exercise}

\begin{exercise}
  \label{exc:embedeval}

%if False
\begin{code}
import DSLsofMath.ComplexSyn
import DSLsofMath.CSem (Complex(C))
\end{code}
%endif
We can embed semantic complex numbers in the syntax:
%
\begin{code}
embed :: Complex r -> ComplexSyn r
embed (C (x, y)) = ToComplexCart x y
\end{code}
%
The embedding should satisfy a round-trip property:
%
|eval (embed s) == s| for all semantic complex numbers |s|.
%
What about the opposite direction: when is |embed (eval e) == e|?
%
Here is a diagram showing how the types and the functions fit together

\hspace{\mathindent}%
\begin{tikzcd}
  |ComplexSyn r| \arrow[d, bend left, "|eval|"]  \arrow[loop right, "|embed . eval|"] \\
  |ComplexSem r| \arrow[u, bend left, "|embed|"] \arrow[loop right, "|eval . embed|"]
\end{tikzcd}


Step 0: type the quantification: what is the type of |e|?

Step 1: what equality is suitable for this type?

Step 2: if you use ``equality up to eval'' --- how is the resulting
property related to the first round-trip property?
%See blackboard/W1/20170116_161148.jpg

\end{exercise}

\begin{exercise}
  Read the next few pages of Appendix I (in \citep{adams2010calculus})
  defining the polar view of Complex Numbers and try to implement
  complex numbers again, this time based on magnitude and phase for
  the semantics.
  %
  Try to use the same interface, so that you can import this new polar
  representation or the previous Cartesian representation without
  changing anything more than the import statement.
\end{exercise}

\begin{exercise}
  Implement a simplifier |simp :: ComplexSyn r -> ComplexSyn r| that
  handles a few cases like |0 * x = 0|, |1 * x = x|, |(a + b) * c = a
  * c + b * c|, etc.
  %
  What class context do you need to add to the type of |simp|?
  %
  This exercise is rather open-ended but it is recommended to start
  with a concrete view of what expected ``normal forms'' should be so
  that you can test it.
\end{exercise}

%**TODO: include as an early exercise
% Exercises:
% \begin{itemize}
% %*TODO: make this one of the numbered exercises
% \item implement |(*.)| for |ComplexD|
% \end{itemize}

%include E1_from_exams.lhs
