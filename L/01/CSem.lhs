\begin{code}
module DSLsofMath.CSem where

newtype ComplexSem r = CS (r , r)    deriving Eq

re :: ComplexSem r      ->  r
re z @ (CS (x , y))   =   x

im :: ComplexSem r      ->  r
im z @ (CS (x , y))   =   y

(+.) :: Num r =>  ComplexSem r -> ComplexSem r -> ComplexSem r
(CS (a , b)) +. (CS (x , y))  =  CS ((a + x) , (b + y))

(*.) :: Num r =>  ComplexSem r -> ComplexSem r -> ComplexSem r
CS (ar, ai) *. CS (br, bi) = CS (ar*br - ai*bi, ar*bi + ai*br)

instance Show r => Show (ComplexSem r) where
  show = showCS

showCS :: Show r => ComplexSem r -> String
showCS (CS (x, y)) = show x ++ " + " ++ show y ++ "i"
\end{code}
