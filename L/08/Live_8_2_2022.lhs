\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DSLsofMath.Live_8_2 where
import DSLsofMath.Algebra (Additive(..), AddGroup(..), (-),
                           Multiplicative(..), MulGroup(..), (/), (^),
                           Ring, Field, exp, fromInteger)
import DSLsofMath.PSDS hiding (mapP, scaleP)
import qualified Prelude -- hide everything by default
import Prelude (Bool(..), Eq(..), Show(..), Integer, Rational,
                pi, Double, Functor(fmap),
                error, const, id, (.), map, take, iterate, ($))
type REAL = Double
\end{code}

Domain-Specific Languages of Mathematics
Lecture 8.2: Connecting
+ Power Series    (Week 5),
+ Complex numbers  (Week 1),
+ Vector spaces     (Week 7), and
+ Laplace transforms (Week 8).

Reminder: exp, sin, cos as power series:

\begin{code}
expP, sinP, cosP :: Field a => PS a
expP = integP one  expP
sinP = integP zero cosP
cosP = integP one (negate sinP)
\end{code}

Reminder: vector spaces:
\begin{code}
infixr 7 *^  -- scale

class (Field s, AddGroup v) => VectorSpace v s where
  (*^) :: s -> v -> v

scale :: VectorSpace v s => s -> v -> v
scale = (*^)
\end{code}

New: Power series as a vector space

\begin{code}
instance Field a => VectorSpace (PS a) a where (*^) = scaleP

scaleP :: Multiplicative a => a -> PS a -> PS a
scaleP c = mapP (c*)

mapP :: (a->b) -> (PS a -> PS b)
mapP f (P as) = P (map f as)
\end{code}
eP :: Int -> PS a
eP i = P [0,0,0,0,....,1,0,...]

Can we combine power series with complex numbers?
\begin{code}
newtype Complex r = C (r, r)  deriving (Eq, Show)
  -- Complex REAL, but we also could have Complex Rational, ...
re, im :: Complex a -> a
re (C (x, _)) = x
im (C (_, y)) = y

i :: Ring a => Complex a
i = C (zero, one)

zeroC :: Additive r => Complex r
zeroC = C (zero, zero)

addC :: Additive a => Complex a -> Complex a -> Complex a
addC (C (x1, y1)) (C (x2, y2)) = C (x1+x2, y1+y2)

instance Additive r => Additive (Complex r) where zero = zeroC; (+) = addC

instance Field a => VectorSpace (Complex a) a where (*^) = scaleC
-- two-dimensional vector space over the scalars a

scaleC :: Multiplicative a => a -> Complex a -> Complex a
scaleC c = mapC (c*)

mapC :: (a->b) -> (Complex a -> Complex b)
mapC f (C (x, y)) = C (f x, f y) 

eC1, eC2 :: Ring a => Complex a
eC1 = one -- C (one, zero)
eC2 = i   -- C (zero, one) 

embed :: Ring a => a -> a -> Complex a
embed x y = scaleC x eC1 + scaleC y eC2

prop_Complex :: (Eq a, Ring a) => Complex a -> Bool
prop_Complex z = embed (re z) (im z) == z
\end{code}
every complex number z can be written as z = x+i*y  -- x, y :: a
but here it should be z = scale x ec1 + scale y ec2

AddGroup
\begin{code}
instance AddGroup r => AddGroup (Complex r) where  negate = negateC

negateC :: AddGroup r => Complex r -> Complex r
negateC = mapC negate
\end{code}

----------------
Multiplicative, Ring
\begin{code}
instance Ring r => Multiplicative (Complex r) where one = oneC; (*) = mulC

oneC :: Ring r => Complex r
oneC = C (one, zero)

mulC :: Ring a => Complex a -> Complex a -> Complex a
mulC (C (x1, y1)) (C (x2, y2)) = C (real, imag)
  where  real = x1*x2 - y1*y2
         imag = x1*y2 + x2*y1
\end{code}

----------------
MulGroup, Field
\begin{code}
instance Field r => MulGroup (Complex r) where recip = recipC

recipC :: Field a => Complex a -> Complex a
recipC (C (a, b)) = scaleC c (C (a, negate b))
  where  c = recip (a^2+b^2)
\end{code}

  recip (C (a, b))
=
  recip (a+ib)
=
  1/(a+ib)
= -- multiply by (a-ib)/(a-ib)
  (a-ib) / ((a+ib)*(a-ib))   
= -- simplify  (using (x+y)*(x-y)=x²-y²
  (a-ib) / ((a^2-(ib)^2)
= -- simplify 
  (a-ib) / (a^2+b^2)
= -- simplify 
  scaleC (recipR (a^2+b^2)) (a-ib)
= -- REAL -> COMPLEX -> COMPLEX
  scaleC (recipR (a^2+b^2)) (C (a, negate b))



Some test values
\begin{code}
expf, sinf, cosf :: Field s => s -> s
expf = evalPS 30 expP
sinf = evalPS 30 sinP
cosf = evalPS 30 cosP

ei :: Field s => Complex s
ei = expf i  -- = 1 + i + i^2/2 + i^3/6 ....

c1, s1 :: Field s => s
c1 = cosf one
s1 = sinf one
\end{code}
-- ei = C (0.5403023058681398, 0.8414709848078965)
      c1 = 0.5403023058681398
                          s1 = 0.8414709848078965

ei == C (c1, s1)

expf i == C (cosf 1, sinf 1)

----------------
* Relating exp, sin, and cos as power series

let   expa a x = exp (a*x)
then  D (expa a) x = a * expa a x
then  D (expa a) = scaleP a (expa a)
and   expa a 0 = exp (a*0) = exp 0 = 1

Thus we can find the power series by integP + recursion:
\begin{code}
expa :: Field a => a -> PS a
expa a = integP one (scaleP a (expa a))
-- expa a = integP one (a*^expa a)

expi :: Field a => PS (Complex a)
expi = expa i   
   
cosPsinP :: Field a => PS (Complex a)
cosPsinP = cosP + scaleP i sinP

eqP :: Eq a => Prelude.Int -> PS a -> PS a -> Bool
eqP n (P as) (P bs) = take n as == take n bs

test :: Bool
test = eqP 8 expi (cosPsinP :: PS (Complex Rational))
\end{code}
--    re         imag
P [C (one  ,     zero ),
   C (zero ,     one  ),
   C ((-1) % 2,  zero ),
   C (zero ,  (-1) % 6),
   C (1 % 24,    zero ),
   C (zero ,     1 % 120),
   C ((-1) % 720,zero ),
   C (zero ,  (-1) % 5040)]


----------------
Connecting to Laplace of sin, cos
  (on Jamboard)

We now know that
  expi = cosP + scaleP i sinP
  exp (i*  x ) = cos   x  + i * sin   x      -- eq1
  exp (i*(-x)) = cos (-x) + i * sin (-x)
  exp (-i*x)   = cos   x  - i * sin   x      -- eq2

Take the average of eq1 and eq2:
  (exp (i*x) + exp(-i*x))/2 =     cos x
and similarly
  (exp (i*x) - exp(-i*x))/2 = i * sin x
or equivalently
  cos x = (exp (i*x) + exp(-i*x))/2
and 
  sin x = (exp (i*x) - exp(-i*x))/(2*i)

We know the Laplace transform of \x->exp (a*x) is \s->1/(s-a)

Thus we get
  L cos s = (1/(s-i) + 1/(s-(-i)))/2
          = (1/(s-i) + 1/(s+i))/2 
          = ((s+i) + (s-i))/((s-i)*(s+i)*2)
          = (2*s)/((s²-i²)*2)
          = s/(s²+1)

\begin{code}
instance Field a => VectorSpace a a where (*^) = (*)
\end{code}

