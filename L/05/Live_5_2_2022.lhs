\begin{code}
{-# LANGUAGE FlexibleContexts #-}
module Live_5_2 where
import qualified Prelude
import Prelude (error, Show, Double, map, take, Int, Rational, (.))
import DSLsofMath.Algebra (Ring,Additive((+),zero), Multiplicative((*),one),
                           Field, AddGroup(negate), (-), (^+), (/))
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
newtype Poly a = P [a] deriving Show
eval :: Ring a => Poly a -> (a -> a)
eval (P cs) = evalL cs

evalL :: Ring a => [a] -> (a -> a)
evalL []      = zero
evalL (a:as)  = evalCons a (evalL as)

evalCons :: Ring a => a -> (a -> a) -> (a -> a)
evalCons a0 p = \x -> a0 + x * p x

xP :: Ring a => Poly a
xP = P [zero, one]
\end{code}

----------------
1. Define the Ring methods for polynomials

Ring a = (Additive a, AddGroup a, Multiplicative a)
          (+), zero,  negate,     (*), one

\begin{code}
instance Additive a => Additive       (Poly a) where zero=zeroP; (+)=addP
instance AddGroup a => AddGroup       (Poly a) where negate=negateP
instance Ring a     => Multiplicative (Poly a) where one=oneP;   (*)=mulP

zeroP :: Poly a;   zeroP = P zeroL
zeroL :: [a];      zeroL = []

oneP :: Ring a => Poly a;   oneP = P oneL
oneL :: Ring a => [a];      oneL = [one]

two :: Ring a => a
two = one+one

addP :: Additive a => Poly a -> Poly a -> Poly a
addP (P as) (P bs) = P (addL as bs)

addL :: Additive a => [a] -> [a] -> [a]
addL = zipWithLonger (+)

zipWithLonger :: (a->a->a) -> ([a] -> [a] -> [a])
zipWithLonger op = zWL
  where  zWL []      bs      = bs
         zWL as      []      = as
         zWL (a:as)  (b:bs)  = (op a b) : zWL as bs

mulP :: Ring a => Poly a -> Poly a -> Poly a
mulP (P as) (P bs) = P (mulL as bs)

mulL :: Ring a => [a] -> [a] -> [a]  
mulL [] bs = []
mulL as [] = []
mulL (a:as) bs = addL (scaleL a bs) (zero:mulL as bs)

scaleL :: Ring a => a -> [a] -> [a]
scaleL c = map (c*)

negateP :: AddGroup a => Poly a -> Poly a
negateP (P as) = P (negateL as)

negateL :: AddGroup a => [a] -> [a]
negateL = map negate
\end{code}

  (a:as)*bs
=
  ([a]+(0:as))*bs
= -- distributivity  (a+b)*c == (a*c)+(b*c)
  ([a]*bs)+((0:as)*bs)
=
  (scaleL a bs) + 0:(as*bs)
=
  addL (scaleL a bs) (0:(mulL as bs))



Lemma1: [a]*bs == scaleL a bs == map (a*) bs
Lemma2: ((0:as)*bs) == 0: (as*bs)


----------------
2. Define methods for derivative and integral

Specification of derP:       forall cs. evalP (derP cs) == D (evalP cs)

\begin{code}
p1 :: Ring a => Poly a
p1 = (xP-one)^+2

derP :: Ring a => Poly a -> Poly a
derP (P as) = P (derL as)

derL :: Ring a => [a] -> [a]
derL [] = []
derL (_:as) = Prelude.zipWith (*) as fromOne

fromOne :: Ring a => [a]
fromOne = Prelude.iterate (one+) one
\end{code}

derP (P [1,-2,1])    ==  P [-2,2]
      \x->1-2*x+x^2      \x-> -2 + 2*x

Specification of integP:     forall c, cs. derP (integP c cs) == cs

\begin{code}
integP :: Field a => a -> Poly a -> Poly a
integP a (P as) = P (integL a as)

integL :: Field a => a -> [a] -> [a]
integL a as = a : Prelude.zipWith (/) as fromOne
\end{code}

Example: Power Series and the exponential

D exp = exp
exp zero = one

\begin{code}
type PS = Poly

expP :: Field a => Poly a
expP = integP one expP

takeP :: Int -> PS a -> Poly a
takeP n (P as) = P (take n as)

evalA :: Ring a => Int -> Poly a -> (a -> a)
evalA n = eval . takeP n

testExp :: Double
testExp = evalA 20 expP 1 - Prelude.exp 1
\end{code}



take :: Int -> [a] -> [a]

P [1 % 1
  ,1 % 1
  ,1 % 2   -- 2==    2*1
  ,1 % 6   -- 6==  3*2*1
  ,1 % 24] --24==4*3*2*2

  1 / factorial i





































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
