2f + 2f' + f'' = 0,  f 0 = 1,    f' 0 = -1
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

fs, fs',fs'' :: Field a => Poly a
fs   = integ   1   fs'
fs'  = integ (-1)  fs''
fs'' = scale (-2) (fs' + fs)

-- By hand:

hfs, hfs',hfs'' :: Field a => Poly a
hfs   = Poly $   1  : (-1) :  0  : 1/3 : u
hfs'  = Poly $ (-1) :   0  :  1  : u
hfs'' = Poly $   0  :   2  :  u

u = undefined
\end{code}

----------------
Laplace:

  2f + 2f' + f'' = 0,  f 0 = 1,    f' 0 = -1

  L (2f + 2f' + f'') s = L 0 s
<=> -- Laplace linear
  2*L f s + 2*L f' s + L f'' s = 0
<=> -- L f' s  = -f 0  + s*L f  s  = -1 + s*L f s
    -- L f'' s = -f' 0 + s*L f' s  = 1 + s*(-1+s*L f s)
    -- Let F = L f
  2*F s + 2*(-1 + s*F s) + 1 + s*(-1+s*F s) = 0
<=> -- collect terms
  (2+2*s+s^2)*F s - 2 + 1 - s = 0
<=> -- factor, simplify, let s1 = -1+i, s2 = -1-i
  (s-s1)*(s-s2)*F s = 1 + s
<=>
  F s = (s+1)/(s-s1)/(s-s2)

Partial fraction decomposition:

Ansatz: F s = A/(s-s1) + B/(s-s2)

Multiply by (s-s1)*(s-s2):

  s+1 = A*(s-s2) + B*(s-s1)
-- Find two equations for A and B using s=s1, s=s2
  s1+1 = A*(s1-s2) =  2*i*A   => A =  i/(2*i)  = 1/2
  s2+1 = B*(s2-s1) = -2*i*B   => B = -i/(-2*i) = 1/2

Thus

  F s = 1/2*(recip (s-s1) + recip (s-s2))

and we recognize a lin. comb. of transformed exponentials:

  f t = 1/2*(exp (s1*t) + exp (s2*t))

{- optional: simplify further:
  exp((-1+i)*t) = exp(-t)*exp(i*t)  = exp(-t)*(cos t + i*sin t)
  exp((-1-i)*t) = exp(-t)*exp(-i*t) = exp(-t)*(cos t - i*sin t)

thus
  1/2*(exp (s1*t) + exp (s2*t))
=
  1/2*(exp(-t)*(cos t + i*sin t) + exp(-t)*(cos t - i*sin t))
=
  1/2*exp(-t)*(cos t + i*sin t + cos t - i*sin t)
=
  1/2*exp(-t)*2*cos t
=
  exp(-t)*cos t
-}

Check solution in the main equation:
  f t   = 1/2*(     exp (s1*t) +      exp (s2*t))
  f' t  = 1/2*(  s1*exp (s1*t) +   s2*exp (s2*t))
  f'' t = 1/2*(s1^2*exp (s1*t) + s2^2*exp (s2*t))

  2*f t + 2*f' t + f'' t
=
      (     exp (s1*t) +      exp (s2*t))
 +    (  s1*exp (s1*t) +   s2*exp (s2*t))
 +1/2*(s1^2*exp (s1*t) + s2^2*exp (s2*t))
=
  1/2*(s1^2 + 2*s1 + 2)*exp (s1*t)
  1/2*(s2^2 + 2*s2 + 2)*exp (s2*t)
=
  1/2*0*exp (s1*t) + 1/2*0*exp (s2*t)
=
  0

Check the initial conditions:
  f 0  = 1/2*(   exp (s1*0) +    exp (s2*0)) = 1 -- OK
  f' 0 = 1/2*(s1*exp (s1*0) + s2*exp (s2*0)) = (s1+s2)/2 = -2/2 = -1 -- OK

Thus all the equations are satisfied.


Helper code - not part of the exam question:
\begin{code}
byHaskell, byHand :: Poly Rational
byHaskell = takePoly 4 fs
byHand    = takePoly 4 hfs
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
