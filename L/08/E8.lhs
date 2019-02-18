\newpage
\subsection{Exercises}

\begin{exercise}
\label{exc:LaplaceDk}
Starting from the ``Laplace-D'' law
\begin{spec}
ℒ f' s  =  s * ℒ f s - f 0
\end{spec}
Derive a general formula for |ℒ {-"f^{(k)} "-} s|.
\end{exercise}

\begin{exercise}
  Find the Laplace transforms of the following functions:
%
  \begin{enumerate}
    \item $\lambda t. \, 3*e^{5 * t}$
    \item $\lambda t. \, e^{\alpha * t} - \beta$
    \item $\lambda t. \, e^{(t + \frac{\pi}{6})}$
  \end{enumerate}

\end{exercise}

\begin{exercise}  \leavevmode
  \begin{enumerate}
   %
    \item Show that:
      \begin{enumerate}
        \item $\sin t = \frac{1}{2*i} \left(  e^{i*t} - e^{-i*t}\right)$
        \item $\cos t = \frac{1}{2} \left(  e^{i*t} + e^{-i*t}\right)$
      \end{enumerate}

    \item Find the Laplace transforms
      $\mathcal{L}\, (\lambda t. \, \sin t)$ and $\mathcal{L}\,  (\lambda t.  \, \cos t)$

  \end{enumerate}
\end{exercise}

% TODO (by DaHe): Add some exercises on inverse Laplace transforms and partial
% fraction decomposition

\subsubsection{Exercises from old exams}

\begin{exercise}
  \textit{From exam 2016-03-15}

  Consider the following differential equation:

  $$f''\, t - 2 * f'\, t + f\, t = e^{2 * t},\quad f\, 0 = 2,\quad f'\, 0 = 3$$

  Solve the equation using the Laplace transform.  You
  should need only one formula (and linearity):

  $$\mathcal{L}\, (\lambda t.\, e^{\alpha*t})\, s  = 1 / (s - \alpha)$$
\end{exercise}


\begin{exercise}
  \textit{From exam 2016-08-23}

  Consider the following differential equation:

  $$f''\, t - 5 * f'\, t + 6 * f\, t = e^t,\quad f\, 0 = 1,\quad f'\, 0 = 4$$

  Solve the equation using the Laplace transform.  You
  should need only one formula (and linearity):

  $$\mathcal{L}\, (\lambda t.\, e^{\alpha*t})\, s  = 1 / (s - \alpha)$$
\end{exercise}

\begin{exercise}
  \textit{From exam 2016-Practice}

  Consider the following differential equation:

  $$f''\, t - 2 * f'\, t + f\, t - 2 = 3 * e^{2 * t},\quad f\, 0 = 5,\quad f'\, 0 = 6$$

  Solve the equation using the Laplace transform.  You
  should need only one formula (and linearity):

  $$\mathcal{L}\, (\lambda t.\, e^{\alpha*t})\, s  = 1 / (s - \alpha)$$
\end{exercise}

\begin{exercise}
  \textit{From exam 2017-03-14}

  Consider the following differential equation:

  $$f'' \,t + 4*f\, t = 6*\cos\, t, \quad f\, 0 = 0,\quad f'\, 0 = 0$$

  Solve the equation using the Laplace transform.  You
  should need only two formulas (and linearity):

  $$\mathcal{L}\, (\lambda t.\, e^{\alpha*t})\, s  = 1 / (s - \alpha)$$

  $$2 * \cos \,t = e^{i*t} + e^{-i*t}$$
\end{exercise}


\begin{exercise}
  \textit{From exam 2017-08-22}

  Consider the following differential equation:

  $$f'' \,t -3 \sqrt{2} * f' \, t + 4 * f \, t = 0, \quad f\, 0 = 2,\quad f'\, 0 = 3 \sqrt{2}$$

  Solve the equation using the Laplace transform.  You
  should need only one formula (and linearity):

  $$\mathcal{L}\, (\lambda t.\, e^{\alpha*t})\, s  = 1 / (s - \alpha)$$
\end{exercise}
