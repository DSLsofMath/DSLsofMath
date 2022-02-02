{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DSLsofMath.W06 where
import DSLsofMath.FunExp hiding (eval, derive)
import DSLsofMath.W05
import DSLsofMath.Algebra
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..))
import Prelude (abs)
import Data.Complex ()

evalAll :: Transcendental a => FunExp -> [a -> a]
evalAll e = (evalFunExp e) : evalAll (derive e)

help a b as bs = as * (b : bs) + (a : as) * bs

mulStream :: Ring a => Stream a -> Stream a -> Stream a
mulStream []        _         = []
mulStream _         []        = []
mulStream (a : as)  (b : bs)  = (a*b) :  (as * (b : bs) + (a : as) * bs)

type Stream a = [a]
instance Additive a => Additive (Stream a) where
  zero  = repeat zero
  (+)   = addStream
instance AddGroup a => AddGroup (Stream a) where
  negate = negStream
instance Ring a => Multiplicative (Stream a) where
  one   = one : zero
  (*)   = mulStream

addStream :: Additive a => Stream a -> Stream a -> Stream a
addStream = zipWithLonger (+)

negStream :: AddGroup a => Stream a -> Stream a
negStream = map negate

type Taylor a = Stream a

toMaclaurin :: Ring a => PowerSeries a -> Taylor a
toMaclaurin (Poly as) = zipWith (*) as factorials

fromMaclaurin :: Field a => Taylor a -> PowerSeries a
fromMaclaurin as = Poly (zipWith (/) as factorials)

factorials :: Ring a => [a]
factorials = factorialsFrom 0 1

factorialsFrom :: Ring a => a -> a -> [a]
factorialsFrom n factn = factn : factorialsFrom (n+1) (factn * (n + 1))

ex3, ex4 :: (Eq a, Field a) => Taylor a
ex3 = toMaclaurin (x^3 + two * x)
ex4 = toMaclaurin sinx

ida a = toMaclaurin (evalP (X :+: Const a))

d f a = take 10 (toMaclaurin (evalP (f (X :+: Const a))))

dP f a = toMaclaurin (f (idx + Poly [a]))

integT :: a -> Taylor a -> Taylor a
integT = (:)

integ  ::  Field a => a -> PowerSeries a -> PowerSeries a
integ  a0 (Poly as)  =  Poly (integL a0 as)

integL :: Field a => a -> [a] -> [a]
integL c cs = c : zipWith (/) cs oneUp

type PS a = PowerSeries a
  
solve :: Field a => a -> (PS a -> PS a) -> PS a
solve f0 g = f              -- solves |f' = g f|, |f 0 = f0|
  where f = integ f0 (g f)

idx  ::  Field a => PS a
idx  =   solve 0 (\_f -> 1)         -- \(f'(x) = 1\), \(f(0) = 0\)

expx  ::  Field a => PS a
expx  =   solve 1 (\f -> f)         -- \(f'(x) = f(x)\), \(f(0) = 1\)
expf  ::  Field a => a -> a
expf  =   evalPS 100 expx

testExp :: Double
testExp = maximum (map diff [0,0.001..1::Double])
  where diff = abs . (expf - exp)  -- using the function instance for |exp|

testExpUnits :: Double
testExpUnits =  testExp / epsilon

epsilon :: Double  -- one bit of |Double| precision
epsilon = last (takeWhile (\x -> 1 + x /= 1) (iterate (/2) 1))

sinx,  cosx  :: Field a =>  PS a
sinx  =  integ 0 cosx
cosx  =  integ 1 (-sinx)

sinf,  cosf  :: Field a =>  a -> a
sinf  =  evalPS 100 sinx
cosf  =  evalPS 100 cosx

sx,cx::[Double]
sx = 0  :  1      :  neg 0           :  frac (neg 1) 6  :  error "TODO"
cx = 1  :  neg 0  :  frac (neg 1) 2  :  0               :  error "TODO"

instance (Eq a, Transcendental a) => Transcendental (PowerSeries a) where
   pi   =  Poly [pi]
   exp  =  expPS
   sin  =  sinPS
   cos  =  cosPS

expPS, sinPS, cosPS :: (Eq a, Transcendental a) => PS a -> PS a
expPS  as  = integ  (exp  (val as))  (expPS as   * deriv as)
sinPS  as  = integ  (sin  (val as))  (cosPS as   * deriv as)
cosPS  as  = integ  (cos  (val as))  (-sinPS as  * deriv as)

val ::  Additive a => PS a  ->  a
val (Poly (a:_))   =   a
val _              =   zero

evalP :: (Eq r, Transcendental r) => FunExp -> PS r
evalP (Const x)    =  Poly [fromRational (toRational x)]
evalP (e1 :+: e2)  =  evalP e1 + evalP e2
evalP (e1 :*: e2)  =  evalP e1 * evalP e2
evalP X            =  idx
evalP (Negate e)   =  negate  (evalP e)
evalP (Recip e)    =  recip   (evalP e)
evalP (Exp e)      =  exp     (evalP e)
evalP (Sin e)      =  sin     (evalP e)
evalP (Cos e)      =  cos     (evalP e)

evalFunExp  ::  Transcendental a => FunExp -> a -> a
evalFunExp  (Const alpha)  =   const (fromRational (toRational alpha))
evalFunExp  X              =   id
evalFunExp  (e1 :+: e2)    =   evalFunExp e1  +  evalFunExp e2    
evalFunExp  (e1 :*: e2)    =   evalFunExp e1  *  evalFunExp e2    
evalFunExp  (Exp e)        =   exp     (evalFunExp e)             
evalFunExp  (Sin e)        =   sin     (evalFunExp e)
evalFunExp  (Cos e)        =   cos     (evalFunExp e)
evalFunExp  (Recip e)      =   recip   (evalFunExp e)
evalFunExp  (Negate e)     =   negate  (evalFunExp e)

derive     (Const _)      =  Const 0
derive     X              =  Const 1
derive     (e1 :+: e2)    =  derive e1  :+:  derive e2
derive     (e1 :*: e2)    =  (derive e1  :*:  e2)  :+:  (e1  :*:  derive e2)
derive     (Recip e)      =  let re = Recip e in Negate (re:*:re) :*: derive e
derive     (Negate e)     =  Negate (derive e)
derive     (Exp e)        =  Exp e :*: derive e
derive     (Sin e)        =  Cos e :*: derive e
derive     (Cos e)        =  Const (-1) :*: Sin e :*: derive e

instance Additive FunExp where
  (+)   =  (:+:)
  zero  = Const 0

instance AddGroup FunExp where
  negate x  = Const (-1) * x

instance Multiplicative FunExp where
  (*)  =  (:*:)
  one  = Const 1

instance MulGroup FunExp where
  recip = Recip

instance Transcendental FunExp where
  pi   =  Const pi
  exp  =  Exp
  sin  =  Sin
  cos  =  Cos

instance Additive a => Additive (a, a) where
  (f, f') + (g, g')  = (f + g,  f' + g')
  zero               = (zero,   zero)

instance AddGroup a => AddGroup (a, a) where
  negate (f, f')     = (negate f, negate f')

instance Ring a => Multiplicative (a,a) where
  (f, f') * (g, g')  = (f * g,  f' * g + f * g')
  one                = (one,zero)

instance Field a => MulGroup (a, a) where
  (f, f') / (g, g')  = (f / g,  (f' * g - g' * f) / (g * g))

instance Transcendental a => Transcendental (a, a) where
  pi = (pi, zero)
  exp  (f, f')       = (exp f,  (exp f) * f')
  sin  (f, f')       = (sin f,  cos f * f')
  cos  (f, f')       = (cos f,  -(sin f) * f')

