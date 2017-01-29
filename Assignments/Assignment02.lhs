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
\begin{document}
\title{DSLsofMath 2017: Assignment 2}
\date{}
\maketitle
\section*{Optimisation using Newton's method}

\begin{enumerate}
\item The evaluation of the second derivative is given by

\begin{spec}
  eval'' = eval' . derive = eval . derive . derive
\end{spec}

\begin{enumerate}
\item Show that |eval''| is not a homomorphism.

\item What datatype is needed to have a homomorphism from |Expr| in
  this case?
%
  Show this for the case of multiplication.

\item \label{datatypes} What datatype is needed for the homomorphism
  that is analogous to apply?
%
  Give instances of the numerical classes (|Num|, |Fractional|,
  |Floating|) for this datatype, as well as an embedding of |Const|
  and |Id|.
\end{enumerate}

\item Newton's method allows us to find zeros of a large class of
  functions in a given interval.  The following description of
  Newton's method follows \cite{bird1988introduction}, page 23:

\begin{code}
newton :: (Double -> Double) -> Double -> Double -> Double
newton f eps x  =  if abs fx < eps
                      then x
                      else if fx' /= 0  then  newton f eps next
                                        else  newton f eps (x + eps)
          where  fx              = f x
                 fx'             = undefined -- |f' x| (derivative of |f| at |x|)
                 next            = x - (fx / fx')
\end{code}

Implement Newton's method, using the datatype you introduced above for
computing the derivatives.
%
In other words, use the code above to implement
\begin{spec}
newton :: (FDD Double -> FDD Double) -> Double -> Double -> Double
\end{spec}

where |FDD a| is the datatype from above, in order to obtain the
appropriate value for |f' x|.

Test your implementation on the following functions:

\begin{code}
test0  = x^2                 -- one (double) zero, in |0|
test1  = x^2 - 1             -- two zeros, in |-1| and |1|
test2  = sin                 -- many, many zeros (|n*pi|)
test3 n x y = y^n - fddId x  -- nth root of |x|
  -- where |fddId| is the embedding of |Id|
\end{code}

For each of these functions, apply Newton's method to a number of
starting points from a sensible interval.  For example:
%
\begin{spec}
  fmap (newton test1 0.001) [-2.0, -1.5 .. 2.0]
\end{spec}
%
but be aware that the method might not always converge!

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
optim :: (FDD Double -> FDD Double) -> Double -> Double -> Result Double
\end{spec}

so that |optim f eps x| uses Newton's method to find a zero of |f'|
starting from |x|.
%
If |y| is the result (i.e. |f' y| is within |eps| of 0), then check
the second derivative, returning |Maximum y| if |f'' y < 0|, |Minimum
y| if |f'' > y|, and |Dunno y| if |f'' = 0|.

As before, use several starting points.

Hint: you might want to modify the code you've written for Newton's
method at point 2.

\end{enumerate}

\section*{Formalities}
\begin{description}
\item [Submission:] Assignments are to be submitted via Fire:

  \begin{quote}\url{https://dsls-lp3-17.frs.cse.chalmers.se/login}\end{quote}
\item [Deadline:] Tuesday, 2016-02-28, 23:59.
\item [Grading:] Discussions with each of the teams during the
  exercises session of Friday, 2016-03-03.
\end{description}
\bibliographystyle{abbrvnat}
\bibliography{../L/ref}
\end{document}
