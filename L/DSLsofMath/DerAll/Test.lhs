\begin{code}
{-# LANGUAGE RankNTypes #-}
module DSLsofMath.DerAll.Test where
import Prelude hiding (Num(..),(^),Fractional(..),Floating(..))
import DSLsofMath.Algebra
  (Additive(..), two, AddGroup(..), (-), Multiplicative(..), (^+), MulGroup(..), (^),
   Transcendental(..), Ring, Field)
import DSLsofMath.FunExp(FunExp(..), REAL)
import DSLsofMath.Simplify(simplify, size)
import DSLsofMath.DerAll

-- testing code

f x = (x-one)^+2


preEq :: Int -> DS FunExp -> DS FunExp -> Bool
preEq n as bs = preSimp n as == preSimp n bs

preSimp :: Int -> DS FunExp -> DS FunExp
preSimp n = take n . map simplify

polyh1 :: Int -> (forall a. Transcendental a => a -> a) -> Bool
polyh1 n f = preEq n (f (derAll X)) (derAll (f X))

structuralHomomorphism n =
  [ polyh1 n sin
  , polyh1 n cos
  , polyh1 n exp
  , polyh1 n recip
  , polyh1 n negate
  , polyh1 n (one+)
  , polyh1 n (+one)
  , polyh1 n (two*)
  , polyh1 n (*two)
  ]

polyh1' :: Int -> (FunExp -> FunExp) -> (DS FunExp -> DS FunExp) -> Bool
polyh1' n f fDS = preEq n (fDS (derAll X)) (derAll (f X))

test' n =
  [ polyh1' n sin    sin
  , polyh1' n cos    cos
  , polyh1' n exp    exp
  , polyh1' n (one+) (one+)
  , polyh1' n (+one) (+one)
  , polyh1' n (two*) (two*)
  , polyh1' n (*two) (*two)
  ]


lhs1 n = take n (map simplify (recip (derAll X)))
rhs1 n = take n (map simplify (derAll (recip X)))

tests = test' 5
\end{code}
