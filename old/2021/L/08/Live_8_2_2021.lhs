\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
module DSLsofMath.Live_8_2 where
import DSLsofMath.Algebra (Additive(..), AddGroup(..), (-),
                           Multiplicative(..), MulGroup(..), (/), (^),
                           Ring, Field, exp, fromInteger)
import DSLsofMath.W05
import DSLsofMath.W06 hiding (expf, sinf, cosf)
import qualified Prelude -- hide everything by default
import Prelude (Bool(..), Eq(..), Show(..), Integer, Rational, pi,
                error, const, id, map, take, iterate, ($))
\end{code}
Lecture 8.2: Connecting Power Series (Week 6) and Complex numbers (Week 1)

Reminder: exp, sin, cos as power series:

\begin{code}
e, s, c :: Field a => PowerSeries a
e = integ 1 e
s = integ 0 c
c = integ 1 (negate s)
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
mulC (C (x0,y0)) (C (x1,y1)) = C ( x0*x1 - y0*y1 ,
                                   x0*y1 + y0*x1 )
\end{code}
Use "polynomials in i" for the specification: i^2 = -1
   (x0+y0*i)*(x1+y1*i)
=
   (x0*x1)+(x0*y1*i)+(y0*i*x1)+(y0*i*y1*i)
=
   x0*x1 + (x0*y1+y0*x1)*i + y0*y1*i^2
=  -- def. of i^2
   x0*x1 + (x0*y1+y0*x1)*i - y0*y1
=
   (x0*x1 - y0*y1) + (x0*y1+y0*x1)*i

----------------
Field
\begin{code}
instance Field r => MulGroup (Complex r) where
  recip = recipC

recipC :: Field a => Complex a -> Complex a
recipC (C (a,b)) = C (a/m2, negate (b/m2))
  where m2 = a^2+b^2
\end{code}
  recip (a+b*i)
=
  1/(a+b*i)
= -- mul. med (a-b*i)
  (a-b*i)/((a+b*i)*(a-b*i))
= -- förenkla (konjugatregeln)
  (a-b*i)/(a^2-(b*i)^2)
= -- förenkla med i^2=-1
  (a-b*i)/(a^2+b^2)
= -- låt m2 = (a^2+b^2)
  a/m2 - (b/m2)*i










Some test values
\begin{code}
expf, sinf, cosf :: Field s => s -> s
expf = evalPS 30 e
sinf = evalPS 30 s
cosf = evalPS 30 c

ei :: Field s => Complex s
ei = expf i

c1, s1 :: Field s => s
c1 = cosf 1
s1 = sinf 1
-- expf i == C (cosf 1, sinf 1)
scaleC :: Multiplicative a => a -> Complex a -> Complex a
scaleC s (C (x, y)) = C (s*x, s*y)
\end{code}

exp (scaleC pi i) == C (-1, 0)
exp(pi*i)

exp (a   *i) == cos a + i * sin a
exp ((-a)*i) == cos a - i * sin a

exp (a*i) + exp (-a*i) == 2*cos a

cos a = (exp (i*a) + exp (-i*a))/2

L cos s = L (\a -> (exp (i*a) + exp (-i*a))/2) s
        = (L (\a -> exp (i*a)) s + L (\a -> exp (-i*a)) s)/2
        = (1/(s-i) + 1/(s-(-i)))/2
        = (1/(s-i) + 1/(s+i))/2
        = ((s+i + s-i)/((s-i)*(s+i)))/2
        = 2*s/(s^2-i^2)/2
        = s/(s^2+1)







































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
