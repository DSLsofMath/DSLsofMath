\begin{code}
{-# LANGUAGE FlexibleContexts #-}
module Live_5_2 where
import qualified Prelude
import Prelude (Int, Rational, error, map, Show, Eq, zipWith, (==))
import DSLsofMath.Algebra
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
newtype Poly a = P [a] deriving (Eq, Show) -- egentligen bara ändliga listor
-- P :: [a] -> Poly a
-- Syntax   = Poly a  ~= [a]
-- Semantik = a -> a
evalP :: Ring a => Poly a  -> (a -> a)
evalP (P as) = evalL as

evalL :: Ring a =>     [a] -> (a -> a)
evalL []     _x  = zero
evalL (a:as)  x  = a + x*(evalL as x)
\end{code}

----------------
1. Define (some of) the Ring methods for polynomials

Ring a = (Additive a, AddGroup a, Multiplicative a)
          (+), zero,  negate,     (*), one

\begin{code}
instance Additive a => Additive (Poly a) where zero = zeroP; (+) = addP

zeroP :: Additive a => Poly a
zeroP = P zeroL

zeroL :: Additive a => [a]
zeroL = []

addP :: Additive a => Poly a -> Poly a -> Poly a
addP (P as) (P bs) = P (addL as bs)

addL :: Additive a => [a] -> [a] -> [a]
--addL []     []     = []
addL []     bs     = bs  -- 0+p=0
addL as     []     = as  -- p+0=p
addL (a:as) (b:bs) = (a+b) : addL as bs

instance AddGroup a => AddGroup (Poly a) where negate = negateP
negateP :: AddGroup a => Poly a -> Poly a
negateP (P as) = P (negateL as)

negateL :: AddGroup a => [a] -> [a]
negateL = map negate
  -- "beräkna -p"

instance Ring a => Multiplicative (Poly a) where one = oneP; (*) = mulP
oneP :: Multiplicative a => Poly a
oneP = P oneL

oneL :: Multiplicative a => [a]
oneL = []

mulP :: Multiplicative a => Poly a -> Poly a -> Poly a
mulP (P as) (P bs) = P (mulL as bs)

mulL :: Multiplicative a => [a] -> [a] -> [a]
mulL = error "TODO"

scaleP :: Ring a => a -> Poly a -> Poly a
scaleP s (P as) = P (scaleL s as)
-- scaleP 2 (P [1,2,3]) == P [2,4,6]
scaleL :: Ring a => a -> [a] -> [a]
scaleL s = map (s*)
\end{code}

----------------
2. Define methods for derivative and integral

Spec. of derP:
  forall cs. evalP (derP cs) = D (evalP cs)

-- 
-- D (\x->1+2x+3x²) == \x-> 2+6x
--        0 2  3*2*x¹
\begin{code}
derP :: Ring a => Poly a -> Poly a
derP (P as) = P (derL as)

derL :: Ring a => [a] -> [a]
derL []     = []
derL (_:as) = zipWith (*) as countUp

countUp :: Ring a => [a]
countUp = one : map (one+) countUp

p1, p1' :: Poly Rational
p1  = P (map fromInteger [1,2,3])
p1' = P (map fromInteger [2,6])
test1 = derP p1 == p1'
\end{code}

Specification of integ:
  forall c, cs. derP (integP c cs) = cs

\begin{code}
integP :: Field a => a -> Poly a -> Poly a
integP c (P cs) = P (integL c cs)

integL :: Field a => a -> [a] -> [a]
integL c cs = c : zipWith (/) cs countUp
\end{code}

Example: Power Series and the exponential

\begin{code}
type PS = Poly  -- men jag menar att PS innehåller även oändliga listor
takeP :: Int -> PS a -> Poly a
takeP n (P as) = P (Prelude.take n as)

-- approximation genom att klippa efter n termer
evalPS :: Ring a => Int -> PS a -> (a->a)
evalPS n ps = evalP (takeP n ps)

expPS :: Field a => PS a
expPS = integP one expPS  -- "solve the diff. eq. D exp = exp && exp 0 = 1"

appExpPS :: Field a => Int -> Poly a
appExpPS n = takeP n expPS
\end{code}

expPSL = integL one expPSL
       = 1 : zipWith (/) expPSL countUp
       = 1 : (1/1) : (1/1/2) : (1/1/2/3) ...
       = 1 : (1/1) : (1/(1*2)) : (1/(1*2*3)) ...


P [1 % 1          -- 1 / factorial n
   1 % 1
   1 % 2
   1 % 6
   1 % 24
   1 % 120
   1 % 720
   1 % 5040
   1 % 40320
   1 % 362880]


-- D exp = exp
-- I one exp = exp

exp 0 = e⁰ = 1


























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
