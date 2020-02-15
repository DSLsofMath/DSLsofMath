\newpage
\subsection{Exercises}

%
% TODO: Add some exercises to get comfortable with the stuff that is used in the
% exam questions below.
%

% TODO (by DaHe): How to phrase this question better? It feels a bit clumsy
% right now.

\begin{exercise} \label{exc:findFunExp}

  As shown at the start of the chapter, we can find expressions |e :: FunExp|
  such that |eval e = f| automatically using the assignment |e = f Id|. This is
  possible thanks to the |Num|, |Fractional|, and |Floating| instances of
  |FunExp|. Use this method to find |FunExp| representations of the functions
  below, and show step by step how the application of the function to |Id| is
  evaluated in each case.

  \begin{enumerate}

    \item |f1 x = x^2 + 4|

    \item |f2 x = 7 * exp( 2 + 3*x )|

    \item |f3 x = 1 / (sin x + cos x)|

  \end{enumerate}

\end{exercise}

\begin{exercise} \label{exc:derivFunExp}

  For each of the expressions |e :: FunExp| you found in
  Exercise~\ref{exc:findFunExp}, use |derive| to find an expression
  |e' :: FunExp| representing the derivative of the expression, and
  verify that |e'| is indeed the derivative of |e|.

\end{exercise}

\begin{exercise}
  At the start of this chapter, we saw three different ways of computing the
  value of the derivative of a function at a given point:

  \begin{enumerate}

    \item Using |FunExp|

    \item Using |FD|

    \item Using pairs

  \end{enumerate}

  Try using each of these methods to find the values of |f1' 2|, |f2'
  2|, and |f3' 2|, i.e. the derivatives of each of the functions in
  Exercise~\ref{exc:findFunExp}, evaluated at the point 2. You can
  verify that the result is correct by comparing it with the
  expressions |e1'|, |e2'| and |e3'| that you found in
  \ref{exc:derivFunExp}.

\end{exercise}

\begin{exercise}
  The exponential function |exp t = e^t| has the property that |{-"\int"-} exp t
  dt = exp t + C|. Use this fact to express the functions below as |PowerSeries|
  using |integ|. \textit{Hint: the definitions will be recursive}.

  \begin{enumerate}
    \item |\t -> exp t  |
    \item |\t -> exp (3*t)|
    \item |\t -> 3 * exp (2*t)|
  \end{enumerate}

\end{exercise}

\begin{exercise}
  In the chapter, we saw that a representation |expx :: PowerSeries| of the
  exponential function can be implemented using |solve| as |expx = solve 1 (\f ->
  f)|. Use the same method to implement power series representations of the
  following functions:

  \begin{enumerate}
    \item |\t -> exp (3*t)|
    \item |\t -> 3 * exp (2*t)|
  \end{enumerate}

\end{exercise}

\begin{exercise} \leavevmode

  \begin{enumerate}

    \item Implement |idx'|, |sinx'| and |cosx'| using |solve|

    \item Complete the instance |Floating (PowerSeries a)|

  \end{enumerate}

\end{exercise}

\begin{exercise}

  Consider the following differential equation:

  $$f''\, t + f'\, t - 2 * f\, t = e^{3 * t},\quad f\, 0 = 1,\quad f'\, 0 = 2$$

  We will solve this equation assuming that |f| can be expressed by a power
  series |fs|, and finding the three first coefficients of |fs|.

  \begin{enumerate}
    \item Implement |expx3 :: PowerSeries REAL|, a power series representation of
      $e^{3 * t}$

    \item Find an expression for |fs''|, the second derivative of |fs|, in terms
      of |expx3|, |fs'|, and |fs|.

    \item Find an expression for |fs'| in terms of |fs''|, using |integ|.

    \item Find an expression for |fs| in terms of |fs'|, using |integ|.

    \item Use |takePoly| to find the first three coefficients of |fs|. You can
      check that your solution is correct using a tool such as MATLAB or
      WolframAlpha, by first finding an expression for |f t|, and then getting
      the Taylor series expansion for that expression.

  \end{enumerate}
\end{exercise}

\begin{exercise}
  \textit{From exam 2016-03-15}

  Consider the following differential equation:

  $$f''\, t - 2 * f'\, t + f\, t = e^{2 * t},\quad f\, 0 = 2,\quad f'\, 0 = 3$$

  Solve the equation assuming that |f| can be expressed by a power series |fs|,
  that is, use |deriv| and |integ| to compute |fs|.  What are the first three
  coefficients of |fs|?

\end{exercise}

\begin{exercise}
  \textit{From exam 2016-08-23}

  Consider the following differential equation:

  $$f''\, t - 5 * f'\, t + 6 * f\, t = e^t,\quad f\, 0 = 1,\quad f'\, 0 = 4$$

  Solve the equation assuming that |f| can be expressed by a power series |fs|,
  that is, use |deriv| and |integ| to compute |fs|.  What are the first three
  coefficients of |fs|?

\end{exercise}

\begin{exercise}
  \textit{From exam 2016-Practice}

  Consider the following differential equation:

  $$ f''\, t - 2 * f'\, t + f\, t - 2 = 3 * e^{2 * t},\quad f\, 0 = 5,\quad f'\,
  0 = 6 $$

  Solve the equation assuming that |f| can be expressed by a power series |fs|,
  that is, use |deriv| and |integ| to compute |fs|.  What are the first three
  coefficients of |fs|?

\end{exercise}

\begin{exercise}
  \textit{From exam 2017-03-14}

  Consider the following differential equation:

  $$f'' \,t + 4*f\, t = 6*\cos\, t, \quad f\, 0 = 0,\quad f'\, 0 = 0$$

  Solve the equation assuming that |f| can be expressed by a power series |fs|,
  that is, use |integ| and the differential equation to express the relation
  between |fs|, |fs'|, |fs''|, and |rhs| where |rhs| is the power series
  representation of |(6*).cos|.
 %
  What are the first four coefficients of |fs|?

\end{exercise}

\begin{exercise}
  \textit{From exam 2017-08-22}

  Consider the following differential equation:

  $$f'' \,t -3 \sqrt{2} * f' \, t + 4 * f \, t = 0, \quad f\, 0 = 2,\quad f'\, 0
  = 3 \sqrt{2}$$

  Solve the equation assuming that |f| can be
  expressed by a power series |fs|, that is, use |integ| and the
  differential equation to express the relation between |fs|, |fs'|,
  and |fs''|.
  %
  What are the first three coefficients of |fs|?
\end{exercise}
