module P1 where
import Prelude
-- a)
class Field f where
  mul   ::  f -> f -> f
  add   ::  f -> f -> f
  zer   ::  f
  one   ::  f
  neg   ::  f -> f
  rec   ::  f -> f
-- b)
data F v  =  Mul  (F v)  (F v)  |  Rec  (F v)  |  One
          |  Add  (F v)  (F v)  |  Neg  (F v)  |  Zer
          |  V v    deriving Show
{- Or, with {-# LANGUAGE GADTs #-} at the top:
data F v where
  Mul   ::  F v -> F v -> F v
  Add   ::  F v -> F v -> F v
  Zer   ::  F v
  One   ::  F v
  Neg   ::  F v -> F v
  Rec   ::  F v -> F v
  V     ::  v -> F v
  deriving Show
-}
instance Field (F v) where
  mul = Mul; add = Add; zer = Zer; one = One; neg = Neg; rec = Rec
-- c)
-- instance Fractional a => Field a where
instance Field Double where
  mul = (*); add = (+); zer = 0; one = 1; neg = negate; rec = recip
instance Field Bool where
  mul = (&&); add = (/=); zer = False; one = True; neg = id; rec = id
-- d)
eval :: Field f => (v->f) -> F v -> f
eval f (Mul x y) = mul (eval f x) (eval f y)
eval f (Add x y) = add (eval f x) (eval f y)
eval f (Zer)     = zer
eval f (One)     = one
eval f (Neg x)   = neg (eval f x)
eval f (Rec x)   = rec (eval f x)
eval f (V v)     = f v
-- e)
evalD :: (v->Double) -> F v -> Double
evalD = eval
evalB :: (v->Bool) -> F v -> Bool
evalB = eval

ex1, ex2, ex3, x :: F String
ex1 = add x (rec x)
ex2 = mul x (neg x)
ex3 = mul ex1 ex2
x = V "x"

assD :: String -> Double
assD "x" = 1
assB :: String -> Bool
assB "x" = True
testD = map (evalD assD) [ex1, ex2, ex3] == [2, -1, -2]
testB = map (evalB assB) [ex1, ex2, ex3] == [False, False, False]

main = print (testD && testB)
