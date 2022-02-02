{-# LANGUAGE RebindableSyntax, TypeSynonymInstances #-}
module DSLsofMath.W08 where
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..),sum)
import DSLsofMath.W05
import qualified DSLsofMath.W06
import DSLsofMath.W06 (Taylor, integ, sinx, sinf, cosx, cosf)
import DSLsofMath.Algebra
import Data.Complex

expx :: Field a => PowerSeries a
expx = integ 1 expx

expf :: Field a => a -> a
expf = evalPS 100 expx

i :: Ring a => Complex a
i = zero :+ one

ex1 :: Field a => Complex a
ex1 = expf i

compScale :: Ring a => a -> PowerSeries a -> PowerSeries a
compScale c (Poly as) = Poly (zipWith (*) as (iterate (c*) 1))

type PSC a = PowerSeries (Complex a)
expix :: Field a => PSC a
expix = compScale i expx

cosxisinx :: Field a => PSC a
cosxisinx = cosx + Poly [i] * sinx

ex2, ex2' :: Field a => PSC a
ex2   = takePoly 8 expix
ex2'  = takePoly 8 cosxisinx

test2 :: Bool
test2 = ex2 == (ex2' :: PSC Rational)

check2 :: Bool
check2 = ex2 == coeff2

coeff2 ::  PSC Rational
coeff2 =   Poly [      1            :+ 0, {-"\qquad"-}  0 :+     1,
                   (-  1  /  2)     :+ 0, {-"\qquad"-}  0 :+ (-  1  /  6),
                       1  /  24     :+ 0, {-"\qquad"-}  0 :+     1  /  120,
                   (-  1  /  720)   :+ 0, {-"\qquad"-}  0 :+ (-  1  /  5040)]

ex2R :: Poly Rational
ex2R = takePoly 8 cosx

ex2I :: Poly Rational
ex2I = takePoly 8 sinx

derivT :: Taylor a -> Taylor a
derivT = tail

integT :: a -> Taylor a -> Taylor a
integT = (:)

val ::  Additive a => Taylor a  ->  a
val (a:_)   =   a
val _       =   zero

expT, sinT, cosT :: Transcendental a => Taylor a -> Taylor a
expT  as  = integT  (exp  (val as))  (expT as   * derivT as)
sinT  as  = integT  (sin  (val as))  (cosT as   * derivT as)
cosT  as  = integT  (cos  (val as))  (-sinT as  * derivT as)

instance Transcendental a => Transcendental (Taylor a) where
   pi   =  [pi]; exp  =  expT;  sin  =  sinT;  cos  =  cosT

instance Field a => MulGroup (Taylor a) where
  recip = recipStream

recipStream :: Field a => Taylor a -> Taylor a
recipStream [] = error "recipStream: divByZero"
recipStream (f0:f') = g
  where  g   = g0:g'
         g0  = recip f0
         g'  = negate (f' * g*g)

expTx :: Taylor REAL
expTx = expT [0,1]
testExpT = take 5 expTx == [1.0, 1.0, 1.0, 1.0, 1.0]

expix3 :: Taylor (Complex Double)
expix3 = exp (i * x)            where  i  = [0 :+ 1]; x  = [0,1]

ex2alt3 = take 10 expix3
testex2alt3 = ex2alt3 ==  take 10 (concat (repeat [1, i, -1, -i]))

cosxisinx3 :: Taylor (Complex Double)
cosxisinx3 = cos x + i * sin x  where  i  = [0 :+ 1];  x  = [0,1]

ex2'alt3 = take 10 cosxisinx3

fs :: Field a => PowerSeries a
fs = integ 1 fs'
   where  fs'   = integ 0 fs''
          fs''  = exp3x + 3 * fs' - 2 * fs
exp3x :: Field a => PowerSeries a
exp3x = compScale 3 expx

