\subsection{Exercises}

%
% TODO: Add some exercises to get comfortable with the stuff that is used in the
% exam questions below.
%

% TODO (by DaHe): How to phrase this question better? It feels a bit clumsy
% right now.

\begin{exercise} \label{ex:findFunExp}

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

\begin{exercise} \label{ex:derivFunExp}

  For each of the expressions |e :: FunExp| you found in exercise
  \ref{ex:findFunExp}, use |derive| to find an expression |e' :: FunExp|
  representing the derivative of the expression, and verify that |e'| is indeed
  the derivative of |e|.
\end{exercise}

\begin{exercise}
  At the start of this chapter, we saw three different ways of computing the
  value of the derivative of a function at a given point:

  \begin{enumerate}

    \item Using |FunExp|

    \item Using |FD|

    \item Using pairs

  \end{enumerate}

  Try using each of these methods to find the values of |f1' 2|, |f2' 2|, and
  |f3' 2|, i.e. the derivatives of each of the functions in exercise
  \ref{ex:findFunExp}, evaluated at the point 2. You can verify that the result
  is correct by comparing it with the expressions |e1'|, |e2'| and |e3'| that
  you found in \ref{ex:derivFunExp}.

\end{exercise}

\begin{exercise} \leavevmode

  \begin{enumerate}

    \item Implement |idx'|, |sinx'| and |cosx'| using |solve|

    \item Complete the instance |Floating (PowerSeries a)|

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
  \textit{From practice exam}

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

