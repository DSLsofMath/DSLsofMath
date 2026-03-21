%if False
\begin{code}
{-# LANGUAGE RebindableSyntax #-}
module Solution_202603_Proof where
import Prelude (Bool, Int, Rational, Eq((==)), (!!), otherwise, error, fromIntegral, toInteger)
import DSLsofMath.Algebra
\end{code}
%endif
\paragraph{Solution}

\begin{enumerate}
\item \textbf{Warm-up (5p):}
Tracing |deriv [(0, 5), (2, 3)]|:
\begin{spec}
  deriv ((0, 5) : [(2, 3)])
= -- since n == 0, we drop the constant term and recurse per the |if| branch
  deriv [(2, 3)]
= -- since n == 2 (not 0), we use the |else| branch
  (2-1, 3 * fromIntegral 2) : deriv []
= -- arithmetic and base case
  (1, 6) : []
= [(1, 6)]
\end{spec}
Result: |[(1, 6)]| (representing the polynomial $6x$).


\item \textbf{Equational Reasoning for mulX (10p):}
We proceed by structural induction on the list |p|.

\textbf{Base case} (|p = []|):
\begin{spec}
    eval (mulX []) x
  = -- def. of mulX
    eval [] x
  = -- def. of eval []
    0
  = -- Ring algebra (zero is the annihilator for (*))
    x * 0
  = -- def. of eval [] (backwards)
    x * eval [] x
\end{spec}

\textbf{Inductive step} (|p = (n, a) : nas|):
Assume the Induction Hypothesis (IH): |eval (mulX nas) x == x * eval nas x|

\begin{spec}
    eval (mulX ((n, a) : nas)) x
  = -- def. of mulX
    eval ((n+1, a) : mulX nas) x
  = -- def. of eval ((n, a) : nas)
    a * (x ^+ (n+1)) + eval (mulX nas) x
  = -- Apply IH
    a * (x ^+ (n+1)) + x * eval nas x
  = -- Property of exponents: |x ^+ (n+1) == x * (x ^+ n)|
    a * (x * (x ^+ n)) + x * eval nas x
  = -- Ring algebra: |a * (x * b) == x * (a * b)|
    x * (a * (x ^+ n)) + x * eval nas x
  = -- distributivity of (*) over (+)
    x * (a * (x ^+ n) + eval nas x)
  = -- def. of eval ((n, a) : nas) (backwards)
    x * eval ((n, a) : nas) x
\end{spec}
This completes the proof.


\item \textbf{Homomorphism for addP (5p):}
\textbf{Claim: True.} Let |op = addP|. 
From the specifications |H2(eval, addP, (+))| and |H1(eval, deriv, D)|, we know |addP| and |deriv| correctly implement polynomial addition and differentiation. 
Because the semantic derivative is a linear operator, $D(f + g) = D f + D g$. Translating this back to the syntax via our homomorphisms guarantees that the syntactic derivative of a sum is the sum of the syntactic derivatives. 
Thus, |deriv (addP p q) == addP (deriv p) (deriv q)| holds, meaning |deriv| is a homomorphism from |addP| to |addP|.


\item \textbf{Homomorphism for mulP (5p):}
\textbf{Claim: False} (|not (Exists op)|). We prove this by contradiction. 
Assume such a binary operator |op| exists. 

Because |deriv| and |mulP| satisfy |H1(eval, deriv, D)| and |H2(eval, mulP, (*))|, the semantic product rule $D(f \cdot g) = (D f) \cdot g + f \cdot (D g)$ tells us that the derivative of a product inherently depends on the original functions, not just their derivatives. 

To show a formal contradiction syntactically, let |q = [(1, 1)]| ($x$) and let |p| be any constant $c$ (|[(0, c)]|).
\begin{itemize}
    \item Then |mulP p q = [(1, c)]| ($cx$), and |deriv (mulP p q) = [(0, c)]| ($c$).
    \item However, |deriv p = []| ($0$) and |deriv q = [(0, 1)]| ($1$) for \textbf{any} choice of $c$.
\end{itemize}

If |op| were a homomorphism, then |op (deriv p) (deriv q)| would have to equal |deriv (mulP p q)|. This implies:
\begin{spec}
  op [] [(0, 1)] == [(0, c)]
\end{spec}
This would mean that the binary function |op|, when given the exact same two inputs (|[]| and |[(0, 1)]|), must return a different result for every possible value of $c$. This contradicts the definition of a well-defined function. Therefore, no such |op| can exist.
\end{enumerate}
