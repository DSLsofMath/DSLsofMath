\documentclass{article}
\usepackage{amsmath}
\usepackage{a4wide}
\usepackage{natbib}
\usepackage{url}
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8x]{inputenc}
\RequirePackage{ucs}
\RequirePackage{amsfonts}
%include polycode.fmt
%include ../L/dslm.format
\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt plus 2pt minus 1pt}
\providecommand{\textepsilon}{\ensuremath{\epsilon}}
% the `doubleequals' macro is due to Jeremy Gibbons
\def\doubleequals{\mathrel{\unitlength 0.01em
  \begin{picture}(78,40)
    \put(7,34){\line(1,0){25}} \put(45,34){\line(1,0){25}}
    \put(7,14){\line(1,0){25}} \put(45,14){\line(1,0){25}}
  \end{picture}}}
\begin{document}
\title{DSLsofMath 2017: Assignment 2}
\date{}
\maketitle
\section*{Optimisation using Newton's method}

This assignment is based on the lectures from weeks 3 and 4 (the
|FunExp| type, |eval|, |derive|, |D|, tupling, homomorphisms, |FD|, |apply|,
\ldots) so it pays off to work through those notes carefully.

\begin{enumerate}
\item The evaluation of the second derivative is given by

\begin{spec}
  eval'' = eval' . derive = eval . derive . derive
\end{spec}

\begin{enumerate}
\item Show that |eval''| is not a homomorphism from |FunExp| to |FunSem = REAL -> REAL|.

\item Given the following types

\begin{code}
type Tri a     = (a, a, a)
type TriFun a  = Tri (a->a)   -- = |(a->a, a->a, a->a)|
type FunTri a  = a -> Tri a   -- = |a -> (a, a, a)|
\end{code}

Define instances of |Num|, |Fractional|, |Floating|, for |Tri a| and
define a homomorphism |evalDD| from |FunExp| to |FunTri a| (for any
type |a| in |Floating|). You don't need to prove that it is a
homomorphism in this part.

\item Show that |evalDD| is a homomorphism for the case of
multiplication.

\end{enumerate}

\item Newton's method allows us to find zeros of a large class of
  functions in a given interval.  The following description of
  Newton's method follows \cite{bird1988introduction}, page 23:

\begin{code}
type REAL = Double
newton :: (REAL -> REAL) -> REAL -> REAL -> REAL
newton f eps x  =  if abs fx < eps
                      then x
                      else if fx' /= 0  then  newton f eps next
                                        else  newton f eps (x + eps)
          where  fx    = f x
                 fx'   = undefined -- |f' x| (derivative of |f| at |x|)
                 next  = x - (fx / fx')
\end{code}

\begin{enumerate}
\item
Implement Newton's method, using |Tri REAL -> Tri REAL| for the type
of the first argument.
%
In other words, use the code above to implement
%
\begin{spec}
newtonTri :: (Tri REAL -> Tri REAL) -> REAL -> REAL -> REAL
\end{spec}
%
in order to obtain the appropriate value for |f' x|.

\item
Test your implementation on the following functions:
%
\begin{code}
test0 x = x^2                   -- one (double) zero, in |0|
test1 x = x^2 - 1               -- two zeros, in |-1| and |1|
test2 = sin                     -- many, many zeros (|n*pi|)
test3 n x y = y^n - constTri x  -- |test3 n x| specifies the nth root of |x|
  -- where |constTri| is the embedding of |Const|
\end{code}

For each of these functions, apply Newton's method to a number of
starting points from a sensible interval.  For example:
%
\begin{spec}
  map (newton test1 0.001) [-2.0, -1.5 .. 2.0]
\end{spec}
%
but be aware that the method might not always converge!
%

For debugging is advisable to implement |newton| in terms of the minor
variation |newtonList|:
%
\begin{spec}
newton f eps x = last (newtonList f eps x)

newtonList :: (Fractional a, Ord a) => (a->a) -> a -> a -> [a]
newtonList f eps x = x : if ... then [] else ...
\end{spec}
\end{enumerate}

\item We can find the optima of a twice-differentiable function on an
  interval by finding the zeros of its derivative on that interval,
  and checking the second derivative.
%
  If |x0| is a zero of |f'|, then
  \begin{itemize}
  \item if |f'' x0 < 0|, then |x0| is a maximum
  \item if |f'' x0 > 0|, then |x0| is a minimum
  \item if |f'' x0 = 0|, then, if |f'' (x0 - ε) * f'' (x0 + ε) < 0|
    (i.e., |f''| changes its sign in the neighbourhood of |x0|), |x0|
    is an inflection point (not an optimum)
  \item otherwise, we don't know
  \end{itemize}

  Use Newton's method to find the optima of the test functions from
  point 2.
%
  That is, implement a function

\begin{spec}
optim :: (Tri REAL -> Tri REAL) -> REAL -> REAL -> Result REAL
\end{spec}

so that |optim f eps x| uses Newton's method to find a zero of |f'|
starting from |x|.
%
If |y| is the result (i.e. |f' y| is within |eps| of |0|), then check
the second derivative, returning |Maximum y| if |f'' y < 0|, |Minimum
y| if |f'' y > 0|, and |Dunno y| if |f'' = 0|.

As before, use several starting points.

Hint: you might want to modify the code you've written for Newton's
method at point 2.

\end{enumerate}

\section*{Formalities}
\begin{description}
\item [Submission:] Assignments are to be submitted via Fire:
  https://dslm-lp3-18.frs.cse.chalmers.se
\item [Deadline:] Tuesday, 2017-02-27, 23:59.
\item [Grading:] Discussions with each of the teams during one of the
  slots 2018-03-02.
\end{description}
\bibliographystyle{abbrvnat}
\bibliography{../L/ref}
\end{document}
