{-# LANGUAGE FlexibleInstances, PartialTypeSignatures #-}
module DSLsofMath.W03 where
import Prelude hiding (Num(..),Fractional(..), Floating(..))
import DSLsofMath.Algebra (Additive(..),AddGroup(..),(-),
                           Multiplicative(..), MulGroup(..))
import DSLsofMath.SimpleFunExp (FunExp(..))

data X   -- |X| must include the interval |[a,b]| of the reals
data Y   -- another subset of the reals

f :: X -> Y
f = undefined

(x, deriv, ff, a, b, int) = undefined

x   :: X
y   :: X -> Y;    y   =  f
y'  :: X -> Y;    y'  =  deriv f
deriv :: (X -> Y) -> (X -> Y)

data Z   -- Probably also some subset of the real numbers
ff :: (X -> Y) -> (X -> Y) -> X -> Z

a, b :: X
integral = int a b expr
  where  expr x = ff y y' x

int :: X -> X -> (X -> Z) -> Z

ff2 :: Y -> Y -> X -> Z
ff2 = undefined
integral2 = int a b expr
  where  expr x = ff2 y y' x
           where  y   = f x
                  y'  = deriv f x

fromInteger :: (AddGroup a, Multiplicative a) => Integer -> a
fromInteger n  | n < 0      = negate (fromInteger (negate n))
               | n == 0     = zero
               | otherwise  = one + fromInteger (n - 1)

derive :: FunExp -> FunExp
derive     (Const alpha)  =  Const 0
derive     X              =  Const 1
derive     (e1 :+: e2)    =  derive e1  :+:  derive e2
derive     (e1 :*: e2)    =  (derive e1  :*:  e2)  :+:  (e1  :*:  derive e2)

