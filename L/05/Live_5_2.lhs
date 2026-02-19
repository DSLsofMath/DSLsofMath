\begin{code}
module Live_5_2 where
import qualified Prelude
import Prelude ()
-- import DSLsofMath.Algebra
\end{code}
Domain-Specific Languages of Mathematics course
Chapter 5 (Week 5), Lecture 2, Live coding part.

The Ring of polynomials (as lists of coefficients).

0. Define the DSL (types for syntax & semantics, eval)
1. Define the Ring methods for polynomials (& Power Series)
2. Defined methods for derivative and integral

----------------
0. Define the DSL (types for syntax & semantics, eval)
newtype P ...
\begin{code}
\end{code}

----------------
1. Define (some of) the Ring methods for polynomials

Ring a = (Additive a, AddGroup a, Multiplicative a)
          (+), zero,  negate,     (*), one

\begin{code}
\end{code}

----------------
2. Define methods for derivative and integral

Spec. of derP: H₁(evalP, derP, D)
 which expands to
  forall cs. evalP (derP cs) = D (evalP cs)

\begin{code}
\end{code}

Specification of integ:
  forall c, cs. derP (integP c cs) = cs

\begin{code}
\end{code}

Example: Power Series and the exponential

\begin{code}
\end{code}
takeP :: Int -> Poly a -> Poly a
takeP n (P as) = P (take n as)
