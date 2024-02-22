\begin{code}
module Live_6_1 where
import qualified Prelude
import Prelude (Int, Double, Maybe(Nothing, Just), Show, Eq, 
                error, reverse, (.), dropWhile, (==), length, zipWith)
import DSLsofMath.Algebra
type REAL = Double
\end{code}
Domain-Specific Languages of Mathematics course
Chapter 6 (Week 6), Lecture 1, Live coding part.

The Ring of polynomials (as lists of coefficients).

0. Define the DSL (types for syntax & semantics, eval)
1. Define the Ring methods for polynomials & Power Series
2. Defined methods for derivative and integral

----------------
0. Define the DSL (types for syntax & semantics, eval)

Domain(s): polynomial functions and power series

\begin{code}
newtype Poly a = P [a] deriving Show
type PSem a = a->a
evalP :: Ring a => Poly a -> (a->a)
evalP = error "evalP: TODO"

type PS = Poly 
evalPS :: Ring a => Int -> PS a -> (a->a)
evalPS = error "evalPS: TODO"

takePS :: Int -> PS a -> Poly a
takePS = error "takePS: TODO"
\end{code}

----------------
1. Define the Ring methods for Poly and PS

Reminder:
Ring a = (Additive a,  AddGroup a, Multiplicative a)
      ~= (((+), zero),  negate,    ((*), one)      )


\begin{code}
\end{code}

----------------
2. Define methods for derivative and integral

Spec. of derP: H₁(evalP, derP, D)
 which expands to 
  forall cs. evalP (derP cs) = D (evalP cs)

\begin{code}
derP :: Ring a => Poly a -> Poly a
derP (P as) = P (derL as)

derL :: Ring a => [a] -> [a]
derL [] = []
derL (_a:as) = zipWith (*) countUp as

countUp :: Ring a => [a]
countUp = Prelude.iterate (one+) one
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
