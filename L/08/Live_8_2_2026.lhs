\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE RebindableSyntax #-}  -- make it possible to use 0 for zero, 1 for one, etc.
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
default (Rational, Integer)    -- Prefer Rational coefficients when possible
\end{code}

Domain-Specific Languages of Mathematics
Lecture 8.2: Connecting
+ Power Series    (Week 5),       -- is a vector space
+ Complex numbers  (Week 1),      -- is also a vector space
+ Vector spaces     (Week 7), and
+ Laplace transforms (Week 8).    -- is a LinTrans between vector spaces

Reminder: exp, sin, cos as power series:

\begin{code}
expP, sinP, cosP :: Field a => PS a
expP = integP one  expP
sinP = integP zero cosP
cosP = integP one (negate sinP)
\end{code}

Reminder: vector spaces:
\begin{code}
class (Field s, AddGroup v) => VectorSpace v s where
  scale :: s -> v -> v
\end{code}

----------------
* Step 1: Power series as a vector space

\begin{code}
instance Field a => VectorSpace (PS a) a where scale = scaleP

scaleP :: Multiplicative a => a -> PS a -> PS a
scaleP c = mapP (c*)

mapP :: (a->b) -> (PS a -> PS b)
mapP f (P as) = P (map f as)
\end{code}

----------------
* Step 2: Connect back to complex numbers - another vector space

\begin{code}
newtype Complex r = C (r , r)  deriving (Eq, Show)

-- Projections: we want
--      LinTrans(re, Complex r, r)
--   && LinTrans(im, Complex r, r)
re, im :: Complex a -> a
re (C (r, _i)) = r
im (C (_r, i)) = i

zeroC :: Additive r => Complex r
zeroC = C (zero, zero)   -- to satisfy the LinTran from above

-- to satisfy the LinTran from above
addC :: Additive a => Complex a -> Complex a -> Complex a
addC (C (x1, y1)) (C (x2, y2)) = C (x3, y3)
  where   x3 = x1 + x2
          y3 = y1 + y2

instance Additive r => Additive (Complex r) where zero = zeroC; (+) = addC

instance Field a => VectorSpace (Complex a) a where scale = scaleC

scaleC :: Multiplicative a => a -> Complex a -> Complex a
scaleC c = mapC (c*)

mapC :: (a->b) -> (Complex a -> Complex b)
mapC f (C (x, y)) = C (f x, f y)
\end{code}

AddGroup
\begin{code}
instance AddGroup r => AddGroup (Complex r) where  negate = negateC

negateC :: AddGroup r => Complex r -> Complex r
-- negateC (C (x, y)) = C (negate x, negate y)
negateC = mapC negate
\end{code}

----------------
Multiplicative, Ring
\begin{code}
instance Ring r => Multiplicative (Complex r) where one = oneC; (*) = mulC

oneC :: Ring r => Complex r
oneC = C (one, zero)

-- What are the basis vectors?
i :: Ring a => Complex a
i = C (zero, one)

mulC :: Ring a => Complex a -> Complex a -> Complex a
mulC (C (x1, y1)) (C (x2, y2)) = C (x1*x2 - y1*y2, x1*y2 + y1*x2)
\end{code}
z = " x + y*i "  math notation
z = scaleC x oneC + scaleC y i
 =~ linComb [x,y] [oneC,i]


----------------
MulGroup, Field


\begin{code}
instance Field r => MulGroup (Complex r) where recip = recipC

conjugate :: AddGroup a => Complex a -> Complex a
conjugate (C (x, y)) = C (x, negate y)

-- Magnitude squared
magSq :: Ring a => Complex a -> a
magSq (C (x, y)) = x*x + y*y

recipC :: Field a => Complex a -> Complex a
recipC z = scaleC factor (conjugate z)
  where factor = recip (magSq z)

ex1, ex2 :: Ring a => Complex a
ex1 = oneC + i
ex2 = ex1 + oneC
\end{code}
Spec.: forall z. (recipC z) * z == oneC
  (recipC z) * z == oneC  -- has angle 0
  angle (recipC z) == negate (angle z)
 conjugate
  abs (recipC z)  == recip (abs z)




----------------
* Step 3: Can we combine power series with complex numbers?

Some test values
\begin{code}
expf, sinf, cosf :: Field s => s -> s
expf = evalPS 20 expP
sinf = evalPS 20 sinP
cosf = evalPS 20 cosP

ei :: Field s => Complex s
ei = expf i

c1, s1 :: Field s => s
c1 = cosf one
s1 = sinf one
\end{code}
expf i = C (0.5403023058681398,0.8414709848078965)
sinf 1 =                                        0.8414709848078965
cosf 1 =      0.5403023058681398
expf i = scaleC (cosf 1) oneC    + scaleC (sinf 1) i

-- expf i == C (cosf 1, sinf 1)

----------------
* Relating exp, sin, and cos as power series

We want to define "eᵃˣ" = \x -> exp (a*x) as a power series (for all a)
  expa  :: Field a => a -> PS a
  expaf :: Field a => a -> (a ->a)

  Spec.   expaf a x = exp (a*x)
  then  D (expaf a) x = a * expaf a x
  and   expaf a 0 = exp (a*0) = exp 0 = 1

Thus we can find the power series by integP + recursion:

\begin{code}
test :: PS (Complex Rational)
test =
  P [ C (1        ,0   )
    , C (0        ,1    )
    , C (-1/2     ,0   )
    , C (0        ,-1/6)
    , C (1/ 24    ,0   )
    , C (0        ,1/120)
    , C (-1/720   ,0   )
    , C (0        ,(-1)/ 5040)
    , C (1/40320  , 0   )
    , C (0        ,1/ 362880)]
\end{code}
takeP 10 sinP ==
P [0 % 1
  , 1 % 1
  , 0 % 1
  , (-1) % 6
  , 0 % 1
  , 1 % 120
  , 0 % 1
  , (-1) % 5040
  , 0 % 1
  , 1 % 362880]



\begin{code}
expa :: Field a => a -> PS a
expa a = integP one (scaleP a (expa a))

expi :: Field a => PS (Complex a)
expi = expa i

cosPisinP :: Field a => PS (Complex a)
cosPisinP = cosP + scaleP i sinP

test2 = (takeP 100 cosPisinP :: PS (Complex Rational)) ==
        (takeP 100 expi)
\end{code}
(***) :: Complex a -> PS (Complex a) -> PS (Complex a)
scaleP :
----------------
Connecting to Laplace of sin, cos
  (on blackboard)

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
\end{code}

Scalars can be seen as 1-dim. vectors:
\begin{code}
instance Field a => VectorSpace a a where scale = (*)
\end{code}
