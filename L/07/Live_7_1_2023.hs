{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DSLsofMath.W07 where
import DSLsofMath.Algebra
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..),sum)
type REAL = Double
type ℕ = Int

infixr 7 *^

class (Field s, AddGroup v) => VectorSpace v s where
  (*^) :: s -> v -> v  -- scale

instance Field s => VectorSpace s s where (*^) = (*) -- s -> s -> s

newtype Vector s g    = V (g -> s) deriving (Additive, AddGroup)

infix 9 !
(!) :: Vector s g -> g -> s
(V f) ! axel = f axel

instance Field s => VectorSpace (Vector s g) s  where  (*^) = scaleV

scaleV :: Multiplicative s => s -> Vector s g -> Vector s g
scaleV c (V v) = V w
  where w axel = c * (v axel)

e :: (Eq g, Ring s) => g -> Vector s g
e i = V (\j -> i `is` j)

is :: (Eq g, Ring s) => g -> g -> s
is i j = if i == j then one else zero

{-

1 0 0
0 1 0
0 0 1

-}

linComb :: (Finite g, VectorSpace v s)  => (g->s) -> (g->v) -> v
linComb v e = sum (map (\j -> v j *^ e j) finiteDomain)

-- dot product
linComb1 :: (Finite g, Field s)          => (g->s) -> (g->s) -> s
linComb1 = linComb

data A = X | Y

ex1 :: A -> REAL
ex1 X = 4
ex1 Y = 1

v1 = V ex1

----------------

type Finite g = (Bounded g, Enum g, Eq g)
finiteDomain :: Finite a => [a]
finiteDomain = [minBound..maxBound]



linLaw :: (Finite g,
           Eq v',
           VectorSpace v s,
           VectorSpace v' s) =>
  (g -> v) -> (v -> v') -> (g -> s) -> Bool
linLaw e f v = f (linComb v e) == linComb v (f . e)

type Matrix s g g' = g' -> Vector s g

mulMV :: (Finite g, Field s) => Matrix s g g'  ->  Vector s g  ->  Vector s g'
mulMV = error "TODO"

transpose :: Matrix s i j -> Matrix s j i
transpose = error "TODO"

data Periodic where
  Sin  :: Positive  -> Periodic
  Cos  :: Natural   -> Periodic
  deriving Eq

type Positive  = Integer
type Natural   = Integer

testf x = 3*sin x + cos (2*x) - 1
testv :: Vector REAL Periodic
testv = (3::REAL) *^ e (Sin 1) + e (Cos 2) - e (Cos 0)

v :: Vector REAL Periodic
v = V vf
  where  vf (Sin  1)  = 3
         vf (Cos  2)  = 1
         vf (Cos  0)  = -1
         vf _         = 0


toL :: Finite g => Vector s g -> [s]
toL (V v) = map v finiteDomain

instance (Finite g, Show s) => Show (g->s)        where  show = showFun
instance (Finite g, Show s) => Show (Vector s g)  where  show = showVec

showVec :: (Finite g, Show s) => Vector s g -> String
showVec (V v) = showFun v

showFun :: (Finite a, Show b) => (a->b) -> String
showFun f = show (map f finiteDomain)

dot' ::  (Finite g, Ring s) =>
         (g->s) -> (g->s) -> s
dot' v w = sum (map (v * w) finiteDomain)

mulMV' ::  (Finite g, Ring s) =>
           Mat s g g' ->  Vec s g  ->  Vec s g'
mulMV' m v  =  dot' v . m


type Mat s r c = c -> r -> s
type Vec s r = r -> s

linComb' :: (Finite g, VectorSpace v s) => (g->s) -> (g->v) -> v
linComb' a v = sum (map (\j -> a j *^ v j) finiteDomain)

mulMV'' ::  (Finite g, Field s) =>
           Mat s g g' ->  Vec s g  ->  Vec s g'
mulMV'' m v  =  linComb' v . m

checkTypes3 :: (Finite b, Field s) => Mat s a b -> Mat s b c -> a -> [Vec s c]
checkTypes3 m1 m2 i =
  [ getCol (mulM m2 m1) i
  , evalMV m2 (getCol m1 i)
  ]

mulM :: (Finite b, Field s) => Mat s b c -> Mat s a b -> Mat s a c
mulM m2 m1 = flip (evalMV m2 . flip m1)

evalMV :: (Finite a, Field s) => Mat s a b -> Vec s a -> Vec s b
evalMV m v = linComb v . m


mulMM' ::  (Finite b, Ring s) =>
           Mat s b c   ->  Mat s a b  ->  Mat s a c
mulMM' m1 m2 = \r c -> mulMV' m1 (getCol m2 c) r

transpos :: Mat s g g' -> Mat s g' g
transpos m i j = m j i

getCol :: Mat s g g' -> g -> Vec s g'
getCol = transpos

getRow :: Mat s g g' -> g' -> Vec s g
getRow = id


