%if False
\begin{code}
module Mat where
import Prelude (Int, Double, Show, Eq, Bool, (==), (&&),
                error, (.), zipWith, map, head, tail, take)
import qualified Prelude
import DSLsofMath.Algebra
\end{code}
%endif

Consider the following implementation of 2-by-2 matrices:

\begin{code}
data M a = M a a a a
  deriving (Eq, Show)
det :: Ring a => M a -> a
det (M a00 a01 a10 a11) = a00*a11 - a01*a10

diagM :: Additive a => a -> M a
diagM c = M c zero zero c

zeroM :: Additive a => M a
zeroM  =  diagM zero
oneM :: Ring a => M a
oneM   =  diagM one

addM :: Additive a => M a -> M a -> M a
addM  (M a00 a01 a10 a11) (M b00 b01 b10 b11) =
      M (a00+b00) (a01+b01) (a10+b10) (a11+b11)

mulM :: Ring a => M a -> M a -> M a
mulM (M  a00  a01
         a10  a11)  (M  b00  b01
                        b10  b11) =
  M  (a00*b00  + a01*b10)  (a00*b01  + a01*b11)
     (a10*b00  + a11*b10)  (a10*b01  + a11*b11)

instance Additive a => Additive (M a) where zero = zeroM; (+) = addM
instance Ring a => Multiplicative (M a) where one = oneM; (*) = mulM

propDet :: (Eq a, Ring a) => M a -> M a -> Bool
propDet m1 m2 = det (m1*m2) == det m1 * det m2
\end{code}

Proof
\begin{code}
proofDet1  m1@(M  a00  a01
                  a10  a11)  m2@(M  b00  b01
                                    b10  b11) =
  [ det (m1*m2)
  , -- def. matM
    det (  M  (a00*b00  + a01*b10)  (a00*b01  + a01*b11)
              (a10*b00  + a11*b10)  (a10*b01  + a11*b11)  )
  , -- def. det
      (a00*b00  + a01*b10)*(a10*b01  + a11*b11)
   -  (a00*b01  + a01*b11)*(a10*b00  + a11*b10)
  , -- expand
        a00*b00*a10*b01  +  a01*b10*a10*b01  +  a00*b00*a11*b11  +  a01*b10*a11*b11
   - (  a00*b01*a10*b00  +  a00*b01*a11*b10  +  a01*b11*a10*b00  +  a01*b11*a11*b10)
  , -- simplify (1st-5th=0, 4th-8th=0) + reorder products
        a01*a10  *  b01*b10  +  a00*a11  *  b00*b11
   - (  a00*a11  *  b01*b10  +  a01*a10  *  b00*b11)
  , -- rewrite
    (a00*a11 - a01*a10)*(b00*b11 - b01*b10)
  , -- def. det
    det m1 * det m2
  ]
\end{code}

Not part of the exam: some test code.

\begin{code}
m1, m2, m3 :: Ring a => M a
m1 = M zero one two three
m2 = M one two three four
m3 = m1*m2

three, four :: Ring a => a
three  = two + one
four   = two + two

countUp :: Ring a => a -> [a]
countUp n = n : countUp (n+one)
\end{code}
