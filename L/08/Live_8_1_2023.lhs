\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DSLsofMath.Live_8_1_2023 where
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
+ Power Series    (Week 5),       -- is a vector space
+ Complex numbers  (Week 1),      -- is also a vector space
+ Vector spaces     (Week 7), and
+ Laplace transforms (Week 8).    -- is a LinTrans between vector spaces

solutions to a linear ODE
  f'' + 4*f' + f = 0    -- (without starting conditions)

say ps1 is a power series solution
and ps2 is another series solution

(scaleP alpha ps1 + scaleP beta ps2) is also a solution

Reminder: exp, sin, cos as power series:

\begin{code}
expP, sinP, cosP :: Field a => PS a
expP = integP one  expP
sinP = integP zero cosP
cosP = integP one (negate sinP)
\end{code}

Reminder: vector spaces:
\begin{code}
infixr 7 *^

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

Can we combine power series with complex numbers?
\begin{code}
newtype Complex r = C (r , r)  deriving (Eq, Show)

re, im :: Complex a -> a
re (C (xr, _ )) = xr
im (C (_ , xi)) = xi
-- LinTrans(re, Complex r, r)
-- LinTrans(im, Complex r, r)

i :: Ring a => Complex a
i = C (zero, one)

zeroC :: Additive r => Complex r
zeroC = C (zero, zero)

addC :: Additive a => Complex a -> Complex a -> Complex a
addC (C (xr, xi)) (C (yr, yi)) = C (xr+yr, xi+yi)

instance Additive r => Additive (Complex r) where zero = zeroC; (+) = addC

instance Field a => VectorSpace (Complex a) a where (*^) = scaleC

scaleC :: Multiplicative a => a -> Complex a -> Complex a
--scaleC c (C (xr, xi)) = C (c*xr, c*xi)
scaleC c = mapC (c*)

mapC :: (a->b) -> (Complex a -> Complex b)
mapC f (C (xr, xi)) = C (f xr, f xi)
\end{code}

AddGroup
\begin{code}
instance AddGroup r => AddGroup (Complex r) where  negate = negateC

negateC :: AddGroup r => Complex r -> Complex r
negateC = mapC negate
-- negateC = scaleC (negate one)
\end{code}

----------------
Multiplicative, Ring
\begin{code}
instance Ring r => Multiplicative (Complex r) where one = oneC; (*) = mulC

oneC :: Ring r => Complex r
oneC = C (one, zero)
-- (one, i) are base vectors for Complex r   (every z = scale (re z) one + scale (im z) i)

mulC :: Ring a => Complex a -> Complex a -> Complex a
mulC (C (xr, xi)) (C (yr, yi)) = C ((xr*yr - xi*yi), (xr*yi + xi*yr))
\end{code}
  (xr + i*xi) * (yr * i*yi)
= -- distribute (*)
  (xr*yr) + (xr*i*yi) + (i*xi*yr) + (i*xi*i*yi)
= -- collect terms (polynomial in i)
  (xr*yr) + (xr*yi + xi*yr)*i + (xi*yi)*i^2
= -- i^2 = -1
  (xr*yr - xi*yi)*1 + (xr*yi + xi*yr)*i

----------------
MulGroup, Field
\begin{code}
instance Field r => MulGroup (Complex r) where recip = recipC

recipC :: Field a => Complex a -> Complex a
recipC (C (xr, xi)) = C (xr/m2, negate xi/m2)
  where  m2 = xr^2 + xi^2
\end{code}
   1/(xr + i*xi)
=
  (xr-i*xi) / ((xr + i*xi)*(xr-i*xi))
= konjugatregeln
  (xr-i*xi) / (xr^2 - (i*xi)^2)
= förenkla
  (xr-i*xi) / (xr^2 + xi^2)





Some test values
\begin{code}
expf, sinf, cosf :: Field s => s -> s
expf = evalPS 30 expP
sinf = evalPS 30 sinP
cosf = evalPS 30 cosP

ei :: Field s => Complex s
ei = expf i

c1, s1 :: Field s => s
c1 = cosf one
s1 = sinf one
\end{code}
-- expf i == C (cosf 1, sinf 1)

----------------
* Relating exp, sin, and cos as power series

let   expa a x = exp (a*x)
then  D (expa a) x = a * expa a x
and   expa a 0 = exp (a*0) = exp 0 = 1

Thus we can find the power series by integP + recursion:
\begin{code}
expa :: Field a => a -> PS a
expa a = integP one (scaleP a (expa a))

expi :: Field a => PS (Complex a)
expi = expa i


cosPsinP :: Field a => PS (Complex a)
cosPsinP = error "TODO implement cos + i sin"
\end{code}

-- expi
P [ C (1,        0)
  , C (0,        1)
  , C ((-1)%2,   0)
  , C (0,      (-1)%6)
  , C (1%24,     0)
  ]
-- sin
P [0
  ,1
  ,0
  ,(-1) % 6
  ,0
  ]



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
\end{code}

Scalars can be seen as 1-dim. vectors:
\begin{code}
instance Field a => VectorSpace a a where (*^) = (*)
\end{code}
