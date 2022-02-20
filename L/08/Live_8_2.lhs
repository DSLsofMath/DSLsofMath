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
scaleP = error "TODO scaleP"

mapP :: (a->b) -> (PS a -> PS b)
mapP = error "TODO mapP"
\end{code}

Can we combine power series with complex numbers?
\begin{code}
newtype Complex r = C (r , r)  deriving (Eq, Show)

re, im :: Complex a -> a
re = error "TODO re"
im = error "TODO im"

i :: Ring a => Complex a
i = error "TODO i"

zeroC :: Additive r => Complex r
zeroC = error "TODO zeroC"

addC :: Additive a => Complex a -> Complex a -> Complex a
addC = error "TODO addC"

instance Additive r => Additive (Complex r) where zero = zeroC; (+) = addC

instance Field a => VectorSpace (Complex a) a where (*^) = scaleC

scaleC :: Multiplicative a => a -> Complex a -> Complex a
scaleC = error "TODO scaleC"

mapC :: (a->b) -> (Complex a -> Complex b)
mapC = error "TODO mapC"
\end{code}

AddGroup
\begin{code}
instance AddGroup r => AddGroup (Complex r) where  negate = negateC

negateC :: AddGroup r => Complex r -> Complex r
negateC = error "TODO negateC"
\end{code}

----------------
Multiplicative, Ring
\begin{code}
instance Ring r => Multiplicative (Complex r) where one = oneC; (*) = mulC

oneC :: Ring r => Complex r
oneC = error "TODO oneC"

mulC :: Ring a => Complex a -> Complex a -> Complex a
mulC = error "TODO mulC"
\end{code}

----------------
MulGroup, Field
\begin{code}
instance Field r => MulGroup (Complex r) where recip = recipC

recipC :: Field a => Complex a -> Complex a
recipC = error "TODO recipC"
\end{code}


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
expa = error "TODO expa"

expi :: Field a => PS (Complex a)
expi = error "TODO expi"

cosPsinP :: Field a => PS (Complex a)
cosPsinP = error "TODO implement cos + i sin"
\end{code}


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

\begin{code}
instance Field a => VectorSpace a a where (*^) = (*)
\end{code}

