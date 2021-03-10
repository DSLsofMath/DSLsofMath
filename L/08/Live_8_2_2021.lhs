\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
module DSLsofMath.Live_8_2 where
import DSLsofMath.Algebra (Additive(..), AddGroup(..), (-),
                           Multiplicative(..), MulGroup(..), (/), (^),
                           Ring, Field, exp, fromInteger)
import DSLsofMath.W05
import DSLsofMath.W06
import qualified Prelude -- hide everything by default
import Prelude (Bool(..), Eq(..), Show(..), Integer,
                error, const, id, map, take, iterate, ($))
\end{code}

Reminder: exp, sin, cos as power series:

\begin{code}
e, s, c :: Field a => PowerSeries a
e = integ one e
s = integ zero c
c = integ one (negate s)
\end{code}

Can we combine these with complex numbers?
\begin{code}
newtype Complex r = C (r , r)
  deriving (Eq, Show)

i :: Ring a => Complex a
i = C (zero, one)

zeroC :: Additive r => Complex r
zeroC = C (zero, zero)

addC :: Additive a => Complex a -> Complex a -> Complex a
addC (C (x1,y1)) (C (x2,y2)) = C (x1+x2,y1+y2)

instance Additive r => Additive (Complex r) where
  zero = zeroC
  (+) = addC
\end{code}

AddGroup
\begin{code}

instance AddGroup r => AddGroup (Complex r) where
  negate = negateC

negateC :: AddGroup r => Complex r -> Complex r
negateC (C (re, im)) = C (negate re, negate im)

\end{code}

Ring
\begin{code}
instance Ring r => Multiplicative (Complex r) where
  one = oneC
  (*) = mulC

oneC :: Ring r => Complex r
oneC = C (one, zero)

mulC :: Ring a => Complex a -> Complex a -> Complex a
mulC = error "TBD"
\end{code}
Use "polynomials in i" for the specification: i^2 = -1

Field
\begin{code}
instance Field r => MulGroup (Complex r) where
  recip = recipC

recipC :: Field a => Complex a -> Complex a
recipC = error "TBD"
\end{code}


Some test values
\begin{code}
ei :: Field s => Complex s
ei = evalPS 10 e i

c1, s1 :: Field s => s
c1 = evalPS 10 c 1
s1 = evalPS 10 s 1
\end{code}





































  (a1+b1*i)*(a2+b2*i)
= -- distribute
  (a1*a2)+(a1*b2*i)+(b1*i*a2)+(b1*i*b2*i)
= -- collect terms
  a1*a2 + (a1*b2+a1*b1)*i + b1*b2*i^2
= -- def. of i^2
  a1*a2 + (a1*b2+a1*b1)*i - b1*b2
= -- collect terms
  (a1*a1 - b1*b2) + (a1*b2+a1*b1)*i


\begin{spec}
mulC (C (a1,b1)) (C (a2,b2)) = C (a1*a2 - b1*b2,  a1*b2+a2*b1)

recipC (C (a, b)) = C (x, y)
  where  sc = recip (a^2 + b^2)
         x = a*sc
         y = negate (b*sc)
\end{spec}

Spec. of recipC:

recip (a+b*i) = x+y*i
  where (x+y*i)*(a+b*i) = 1

Start from
  (x+y*i)*(a+b*i) = 1
multipliply by (a-b*i)
  (x+y*i)*(a+b*i)*(a-b*i) = (a-b*i)
simplify (conjugate rule)
  (x+y*i)*(a^2 - (b*i)^2) = (a-b*i)
simplify
  (x+y*i)*(a^2 + b^2) = (a-b*i)
divide by the real number a^2 + b^2
  x+y*i = scale (recip (a^2 + b^2)) (a-b*i)
