\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DSLsofMath_Exam_2021_03_P2_Laplace where
import DSLsofMath.Algebra
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..))
newtype Poly a = Poly [a] deriving (Show,Eq)
type PowerSeries = Poly

\end{code}

P4. [25pts] Consider the following differential equations:

  f' = f + g,              f(0) = 1
  g' = exp + 4g - 2f,      g(0) = 3

----------------
Sol. method 1: using power series:

From equations:

\begin{code}
fs' = fs + gs
gs' = exps + scale 4 gs + scale (-2) fs
\end{code}

From spec. of derivative + initial conditions:

\begin{code}
fs = integ 1 fs'
gs = integ 3 gs'
\end{code}


We also know

\begin{code}
exps = integ 1 exps
\end{code}

Now fill in the lists of coefficients incrementally:

Step 1:

exps = 1 :
fs   = 1 :
gs   = 3 :
fs'  = 1+3:
gs'  = 1+4*3-2*1:

Simplify + steg 2:

exps = 1 :  1/1:
fs   = 1 :  4/1:
gs   = 3 : 11  :
fs'  = 4 : 15  :
gs'  = 11: 1+4*11-2*4:

Simplify again + step 3:

(Name prefix "h" for "hand-computed version".)
\begin{code}
hexps, hfs, hfs', hgs, hgs' :: Field a => Poly a
hexps = Poly $ 1 :  1 :  1/2 : u
hfs   = Poly $ 1 :  4 : 15/2 : u
hgs   = Poly $ 3 : 11 : 37/2 : u
hfs'  = Poly $ 4 : 15 : u -- ...
hgs'  = Poly $ 11: 37 : u -- ...

u = undefined
\end{code}

Answer: first three coefficients of fs are [1, 4, 15/2]

*** OK - next solution method using Laplace transform

f' = f + g,                f(0) = 1
g' = exp + 4g - 2f,        g(0) = 3

We know

  L f'  s = - f 0 + s*L f s                   (L-D lemma)

thus for our functions f and g we get

  L f'  s = -1 + s*L f s      (L-D lemma for f)
  L g'  s = -3 + s*L g s      (L-D lemma for g)

and we also have

  L exp s = 1 / (s-1)         (L-exp lemma)

Now apply the transform to both equations and use linearity:

  L f' s = L f s + L g s
  L g' s = L exp s + 4*L g s - 2*L f s

Use (L-D f), (L-D g), and (L-exp):

  -1 + s*L f s = L f s + L g s
  -3 + s*L g s = 1 / (s-1) + 4*L g s - 2*L f s

Introduce shorter names: F = L f, G = L g

  -1 + s*F s = F s + G s
  -3 + s*G s = 1/(s-1) + 4*G s - 2*F s

Collect terms

  (s-1)*F s - 1 = G s
      2*F s + (s-4)*G s = 3 + 1/(s-1)

Substitute G s from the first into the second eq:

  2*F s + (s-4)*((s-1)*F s - 1) = 3 + 1/(s-1)

collect terms in the 2nd eq.

  (2 + (s-4)*(s-1))*F s = 3 + 1/(s-1) + (s-4)

simplify and factor

  (s-2)*(s-3)*F s = (s-1) + 1/(s-1)

thus we get (but multiplying both sides by (s-1)):

  (s-1)*(s-2)*(s-3)*F s = (s-1)^2 + 1

Ansatz: F s = A/(s-1) + B/(s-2) + C/(s-3)
gives us

  A*(s-2)*(s-3) + B*(s-1)*(s-3) + C*(s-1)*(s-2) = (s-1)^2 + 1

Try with s=1, s=2, s=3:

s=1:  A*(1-2)*(1-3) = (1-1)^2 + 1 <=> 2*A = 1
s=2:  B*(2-1)*(2-3) = (2-1)^2 + 1 <=>  -B = 2
s=3:  C*(3-1)*(3-2) = (3-1)^2 + 1 <=> 2*C = 5

Thus we get A = 1/2, B=-2, C=5/2, with

  F s = A/(s-1) + B/(s-2) + C/(s-3)

Transforming back we get

  f  t  = ( 1/2)*exp t - 2*exp (2*t) +  (5/2)*exp (3*t)

from which we can compute f' and g = f' - f:

  f' t  = ( 1/2)*exp t - 4*exp (2*t) + (15/2)*exp (3*t)
  g  t  =              - 2*exp (2*t) +      5*exp (3*t)

Checking the initial conditions:
  f 0 =  ( 1/2) - 2 + (5/2) = 1 -- OK!
  g 0 =         - 2 +     5 = 3 -- OK!

We don't need to check the first diff. eq (f' = f + g) because we just
used it to define g.

Checking the other diff. equation (g' = exp + 4g - 2f):

  LHS = g' t = - 4*exp (2*t) +     15*exp (3*t)

  RHS =  exp t + 4*g t - 2*f t
      =  exp t - 8*exp (2*t) +     20*exp (3*t)
       - exp t + 4*exp (2*t) -      5*exp (3*t)
      =        - 4*exp (2*t) +     15*exp (3*t)
      = LHS -- OK!

** External sanity check:

Wolfram Alpha says:

f(t) = 1/2 e^t (-4 e^t + 5 e^(2 t) + 1)
     = 1/2 e^t - 2 e^{2t} + 5/2 e^{3t}

OK!

================================================================
Helper code - not part of the exam question:
\begin{code}
byHaskell, byHand :: Poly Rational
byHaskell = takePoly 3 fs
byHand    = takePoly 3 hfs
main = print (byHaskell == byHand)

integ  ::  Field a => a -> PowerSeries a -> PowerSeries a
integ  a0 (Poly as)  =  Poly (integL a0 as)

integL :: Field a => a -> [a] -> [a]
integL c cs = c : zipWith (/) cs oneUp

oneUp :: Ring a => [a]
oneUp = one : map (one+) oneUp

evalPoly :: Ring a => Poly a -> (a -> a)
evalPoly (Poly [])        _   =  0
evalPoly (Poly (a:as))    x   =  a + x * evalPoly (Poly as) x

evalPS :: Ring a => Int -> PowerSeries a -> (a -> a)
evalPS n as = evalPoly (takePoly n as)

takePoly :: Int -> PowerSeries a -> Poly a
takePoly n (Poly xs) = Poly (take n xs)

\end{code}

\begin{code}
instance Additive a => Additive (Poly a) where
  (+) = addPoly
  zero = Poly []

instance Ring a => Multiplicative (Poly a) where
  (*) = mulPoly
  one = Poly [one]

instance AddGroup a => AddGroup (Poly a) where
  negate = negPoly

addPoly :: Additive a => Poly a -> Poly a -> Poly a
addPoly (Poly xs) (Poly ys) = Poly (addList xs ys)

addList :: Additive a => [a] -> [a] -> [a]
addList = zipWithLonger (+)

zipWithLonger :: (a->a->a) -> ([a] -> [a] -> [a])
zipWithLonger _   []      bs      = bs  -- |0+bs == bs|
zipWithLonger _   as      []      = as  -- |as+0 == as|
zipWithLonger op  (a:as)  (b:bs)  = op a b : zipWithLonger op as bs

mulPoly :: Ring a => Poly a -> Poly a -> Poly a
mulPoly (Poly xs) (Poly ys) = Poly (mulList xs ys)

mulList :: Ring a => [a] -> [a] -> [a]
mulList  []      _       =  []    -- |0*bs == 0|
mulList  _       []      =  []    -- |as*0 == 0|
mulList  (a:as)  (b:bs)  =  (a * b) :  addList  (scaleList a  bs)
                                                (mulList as   (b:bs))
scale :: Ring a => a -> Poly a -> Poly a
scale s (Poly as) = Poly (scaleList s as)

scaleList :: Multiplicative a => a -> [a] -> [a]
scaleList a = map (a*)

negPoly :: AddGroup a => Poly a -> Poly a
negPoly = polyMap negate

polyMap :: (a->b) -> (Poly a -> Poly b)
polyMap f (Poly as)   = Poly (map f as)

polyCons :: a -> Poly a -> Poly a
polyCons x (Poly xs) = Poly (x:xs)
\end{code}
