{-# LANGUAGE FlexibleInstances, PartialTypeSignatures #-}
module DSLsofMath.SimpleFunExp where
{-
import Prelude hiding (Num(..),Fractional(..), Floating(..))
import DSLsofMath.Algebra (Algebraic(..),Transcendental(..),
                           Additive(..),AddGroup(..),(-),
                           Multiplicative(..), MulGroup(..))
import DSLsofMath.SimpleFunExp ()
-}
import DSLsofMath.W01 (Env, evalEnv)
powTo n = (^n)
powTo' = powTo . fromIntegral
type R = Integer
type REAL = Double
type ℝ = REAL
type ℤ = Integer

data FunExp  =  Const REAL
             |  X
             |  FunExp  :+:  FunExp
             |  FunExp  :*:  FunExp

eval  ::  FunExp         ->  REAL  -> REAL
eval      (Const alpha)      x     = alpha
eval      X                  x     = x
eval      (e1 :+: e2)        x     = eval e1 x  +  eval e2 x
eval      (e1 :*: e2)        x     = eval e1 x  *  eval e2 x

type FunExpS = REAL -> REAL

bigsum :: ℤ -> ℤ -> (ℤ -> R) -> R

bigsum low high f = sum [f i | i <- [low..high]]

sumOfSquares n = bigsum 1 n (powTo 2)

data MVExp = Va String | Ad MVExp MVExp | Di MVExp MVExp

v   = Va  "v"
e1  = Ad  v   v
e2  = Di  e1  e1

evalMVExp  :: MVExp   -> Env String QQ -> Maybe QQ

type QQ = Rational

mayAdd  :: Maybe QQ -> Maybe QQ -> Maybe QQ
mayAdd  (Just a)  (Just b)  =  Just (a+b)
mayAdd  _         _         =  Nothing

mayDiv  :: Maybe QQ -> Maybe QQ -> Maybe QQ
mayDiv  (Just a)  (Just 0)  =  Nothing
mayDiv  (Just a)  (Just b)  =  Just (a/b)
mayDiv  _         _         =  Nothing

evalMVExp e env = eval e
  where  
    eval  (Va  x)       =  lookup x env   -- same as |evalEnv env x|
    eval  (Ad  e1  e2)  =  mayAdd  (eval e1)  (eval e2)
    eval  (Di  e1  e2)  =  mayDiv  (eval e1)  (eval e2)

data PExp v = V v | A (PExp v) (PExp v) | D (PExp v) (PExp v)
evalPExp :: Eq v => PExp v -> Env v QQ -> Maybe QQ
evalPExp e env = eval e
  where  
    eval  (V  x)       =  lookup x env
    eval  (A  e1  e2)  =  mayAdd  (eval e1)  (eval e2)
    eval  (D  e1  e2)  =  mayDiv  (eval e1)  (eval e2)

