\begin{code}
module Live_5_2 where
import qualified Prelude
import Prelude (Show, const, id, error, zipWith, Int, take, Rational)
import DSLsofMath.Algebra (
  Additive(zero, (+)),
  AddGroup,
  Multiplicative((*),one),
  Ring,
  Field, (/)
  )
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
newtype P a = P [a] deriving Show -- don't use trailing zeros
type S a = a -> a
evalP :: (Additive a, Multiplicative a) =>   P a -> S a
evalP (P cs) = evalL cs

type L a = [a]
evalL :: (Additive a, Multiplicative a) =>   L a -> S a
evalL []      x  = zero
evalL (c:cs)  x  = c  +  x * evalL cs x

xP :: Ring a => P a
xP = P xL

xL :: Ring a => L a
xL = [zero,one]
\end{code}

----------------
1. Define (some of) the Ring methods for polynomials

Ring a = (Additive a, AddGroup a, Multiplicative a)
          (+), zero,  negate,     (*), one

\begin{code}
zeroP :: P a
zeroP = P zeroL
zeroL :: L a
zeroL = []

addP :: Additive a => P a ->  P a ->  P a
addP (P as) (P bs) = P (addL as bs)

addL :: Additive a =>  L a -> L a -> L a
addL [] bs = bs
addL as [] = as
addL (a:as) (b:bs) = (a+b) : addL as bs
-- not the same a zipWith (+)
-- could be "zipWithLonger (+)"

instance Additive a => Additive (P a) where zero = zeroP; (+) = addP

oneP :: Multiplicative a => P a
oneP = P oneL
oneL :: Multiplicative a => L a
oneL = [one]

mulP :: Multiplicative a => P a ->  P a ->  P a
mulP (P as) (P bs) = P (mulL as bs)

mulL :: Multiplicative a =>  L a -> L a -> L a
mulL = error "TODO mulL undefined"

instance Multiplicative a => Multiplicative (P a) where one = oneP; (*) = mulP
\end{code}

----------------
2. Define methods for derivative and integral

Spec. of derP: H₁(evalP, derP, D)
 which expands to
  forall cs. evalP (derP cs) = D (evalP cs)

\begin{code}
derP :: Ring a => P a -> P a
derP (P as) = P (derL as)

derL :: Ring a => L a -> L a
derL [] = []    -- D 0 = 0
derL (_:as) = zipWith (*) as oneUp

oneUp :: Ring a => [a]
oneUp = countUp one

countUp :: Ring a => a -> [a]
countUp start = start : countUp (one+start)
\end{code}

Specification of integ:
  forall c, cs. derP (integP c cs) = cs
  forall c, cs. integP c' (derP (c:cs)) = c':cs


\begin{code}
integP :: Field a => a -> P a -> P a
integP a (P as) = P (integL a as)

integL :: Field a => a -> L a -> L a
integL c cs = c : zipWith (/) cs oneUp
\end{code}

Example: Power Series and the exponential

        exp =          D exp
integ 1 exp = exp


exp 0 = 1


\begin{code}
type PowSer = P

expP :: Field a => PowSer a
expP = integP one expP
-- expL = 1 : 1/1 : 1/2 : 1/(2*3) : 1/(1*2*3*4) : ...

takeP :: Int -> PowSer a -> P a
takeP n (P as) = P (take n as)
\end{code}
