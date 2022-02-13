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
\begin{code}
\end{code}

----------------
1. Define the Ring methods for polynomials

Ring a = (Additive a, AddGroup a, Multiplicative a)
          (+), zero,  negate,     (*), one

\begin{code}
\end{code}

----------------
2. Define methods for derivative and integral

Spec. of derP:
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

























A. Appendix: From Ring to Field.

\begin{spec}
instance Field a => MulGroup (Poly a) where recip = recipP
recipP :: Field a => Poly a -> Poly a
recipP (Poly as) = Poly (recipL as)

recipL :: Field a => [a] -> [a]
recipL [] = error "recipL: division by zero"
recipL (a:as) = r
  where  r  = b:bs
         b  = recip a
         bs = scaleL (negate b) (mulL as r)

test1 :: Field a => Poly a
test1 = takeP 5 (recip (one-xP))
\end{spec}
