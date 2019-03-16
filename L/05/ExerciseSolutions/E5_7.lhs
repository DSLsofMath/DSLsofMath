E5_7:  What are the ring operations on |P A|?

These extensions are only needed for some bonus code.
\begin{code}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module E5_3 where

class Ring r where
  add   ::  r -> r -> r
  neg   ::  r -> r
  zero  ::  r
  mul   ::  r -> r -> r
  one   ::  r

type Nat = Integer -- intended for natural numbers only
newtype P a = P (Nat -> a) -- Polynomial coefficients

instance Ring a => Ring (P a) where
  add = addP
  neg = negP
  zero = zeroP

  mul = mulP
  one = oneP

addP :: Ring a => P a -> P a -> P a
addP (P p) (P q) = P (\i -> add (p i) (q i))
negP :: Ring a => P a -> P a
negP (P p) = P (neg . p)
zeroP :: Ring a => P a
zeroP = P (\_-> zero)

oneP :: Ring a => P a
oneP = P (bool2ring . (0==))

mulP :: Ring a => P a -> P a -> P a
mulP (P p) (P q) = P (\k -> sumTo k (\i -> mul (p i) (q (k-i))))

-- helpers:

sumTo :: Ring a => Nat -> (Nat -> a) -> a
sumTo (-1)  f  =  zero
sumTo n     f  =  add (sumTo (n-1) f) (f n)

bool2ring :: Ring a => Bool -> a
bool2ring True   =  one
bool2ring False  =  zero

-- Bonus (not part of the exercise)

integ :: Fractional a => a -> P a -> P a
integ c (P p) = P ip
  where ip i = if i==0 then c else p (i-1) / (fromInteger i)

instance Num a => Ring a where add=(+); neg=negate; zero=0; mul=(*); one=1

expP = integ 1 expP

evalP :: Ring a => Nat -> P a -> a -> a
evalP n (P p) x = sumTo n (\i -> mul (p i) (powR x i))

powR :: Ring a => a -> Nat -> a
powR x 0 = one
powR x n  |  even n     = help
          |  otherwise  = mul x help
  where  half = div n 2
         powhalf = powR x half
         help = mul powhalf powhalf

\end{code}
