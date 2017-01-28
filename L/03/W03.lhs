\section{Week 3}

% (Based on ../../2016/Lectures/Lecture05 )
% Show "Functional Differential Geometry" p16.

\subsection{Types in mathematics}\label{types-in-mathematics}

Types are sometimes mentioned explicitly in mathematical texts:

\begin{itemize}
\item \(x ∈ ℝ\)
\item \(\sqrt{\phantom{x}} : ℝ_{≥0} → ℝ_{≥0}\)
\item \((\_)² : ℝ → ℝ\) or, alternatively but \emph{not} equivalently
\item \((\_)² : ℝ → ℝ_{≥0}\)
\end{itemize}

The types of ``higher-order'' operators are usually not given
explicitly:

\begin{itemize}
\item |lim : (ℕ → ℝ) → ℝ| for \(lim_{n → ∞} \{a_n\}\)
\item \(d/dt : (ℝ → ℝ) → ℝ → ℝ\)
\item sometimes, instead of \(df/dt\) one sees \(f'\) or \(\dot{f}\) or |D f|
\item \(∂f/∂x_i : (ℝⁿ → ℝ) → ℝⁿ → ℝ\)
\item we mostly see \(∂f/∂x\), \(∂f/∂y\), \(∂f/∂z\) etc. when, in the
  context, the function \(f\) has been given a definition of the form
  \(f (x, y, z) = \ldots\)
\item
  a better notation which doesn't rely on the names given to the
  arguments was popularised by Landau in Landau (1934) (English edition
  Landau (2001)): D₁ for the partial derivative with respect to x₁, etc.
\item
  Exercise: for f : ℝ² → ℝ define D₁ and D₂ using only D.
\end{itemize}

\subsection{Type inference and understanding}
\label{type-inference-and-understanding}

From (Sussman and Wisdom 2013):

\begin{quote}
  A mechanical system is described by a Lagrangian function of the
  system state (time, coordinates, and velocities).
%
  A motion of the system is described by a path that gives the
  coordinates for each moment of time.
%
  A path is allowed if and only if it satisfies the Lagrange
  equations.
%
  Traditionally, the Lagrange equations are written

\[
  \frac{d}{dt} \frac{∂L}{∂\dot{q}} - \frac{∂L}{∂q} = 0
\]

What could this expression possibly mean?

\end{quote}

To start answering the question, we start typing the elements involved:

\begin{enumerate}
\item \(∂L / ∂q\) suggests that |L| is a function of at least a pair
  of arguments:
\begin{code}
  L : ℝⁿ → ℝ,    n ≥ 2
\end{code}

This is consistent with the description: ``Lagrangian function of the
system state (time, coordinates, and velocities)''.
%
So we can take |n = 3|:

\begin{code}
  L : ℝ³ → ℝ
\end{code}

\item \(∂L / ∂q\) suggests that \(q\) is the name of a real variable,
  one of the three arguments to \(L\).
%
  In the context, which we do not have, we would expect to find
  somewhere the definition of the Lagrangian as

\begin{code}
  L (t, q, v) = ...
\end{code}
\item therefore, \(∂L / ∂q\) should also be a function of a triple of
  arguments:

\begin{code}
  ∂L / ∂q : ℝ³ → ℝ
\end{code}

It follows that the equation expresses a relation between
\emph{functions}, therefore the \(0\) on the right-hand side is
\emph{not} the real number \(0\), but rather the constant function
\(0\):

\begin{code}
  const 0  :  ℝ³ → ℝ
  const 0 (t, q, v) = 0
\end{code}

\item We now have a problem: \(d / dt\) can only be applied to
  functions of \emph{one} real argument \(t\), and the result is a
  function of one real argument:

\[
\frac{d}{dt} \frac{∂L}{∂\dot{q}}  :  ℝ → ℝ
\]

Since we subtract from this the function \(∂L / ∂q\), it follows that
this, too, must be of type \(ℝ → ℝ\), contradiction.

\item The expression \(∂L / ∂\dot{q}\) appears to also be malformed.
%
  We would expect a variable name where we find \(\dot{q}\), but
  \(\dot{q}\) is the same as \(dq / dt\), a function.

\item Looking back at the description above, we see that the only
  candidate for an application of \(d/dt\) is ``a path that gives the
  coordinates for each moment of time''.
%
  Thus, the path is a function of time, let us say

\[
w  :  ℝ → ℝ, \mbox{where \(w(t)\) is a coordinate at time \(t\)}
\]

We can now guess that the use of the plural form ``equations'' might
have something to do with the use of ``coordinates''.
%
In an \(n\)-dimensional space, a position is given by \(n\)
coordinates.
%
A path would be a function

\begin{code}
    w  :  ℝ → ℝⁿ
\end{code}

which is equivalent to \(n\) functions of type \(ℝ → ℝ\).
%
We would then have an equation for each of them.

\item The Lagrangian is a ``function of the system state (time,
  coordinates, and velocities)''.
%
  If we have a path, then the coordinates at any time are given by the
  path.
%
  The velocity is the derivative of the path, also fixed by the path:

%format dotq = "\dot{q}"
\begin{code}
q  :  ℝ → ℝ
q t  =  w t

dotq : ℝ → ℝ
dotq t = dw / dt
\end{code}

The equations do not use a function \(L : ℝ³→ ℝ\), but rather

\begin{spec}
  L ∘ expand w  :  ℝ → ℝ
\end{spec}

where the ``combinator'' |expand| is given by

\begin{spec}
  expand      :  (ℝ → ℝ) → ℝ → ℝ³
  expand w t  =  (t, w t, D w t)
\end{spec}

\item Similarly, using |D₁|, |D₂|, |D₃| instead of |∂L/∂t| etc., we
  have that, instead of |∂L/∂q| what is meant is

\begin{spec}
  D₂ L ∘ expand w  :  ℝ → ℝ
\end{spec}

and instead of |∂L/∂dotq|

\begin{spec}
  D₃ L ∘ expand w  : ℝ → ℝ
\end{spec}

The equation becomes

\begin{spec}
  D (D₃ L ∘ expand w)  -  D₂ L ∘ expand w  =  0
\end{spec}

a relation between functions of type |ℝ → ℝ|.
%
In particular, the right-hand |0| is the constant function

\begin{spec}
  const 0  :  ℝ → ℝ
\end{spec}

\end{enumerate}

\subsection{Types in Mathematics (Part II)}
