Learning outcomes:

* Power Series:
    * "polynomial" division
    * definitions of deriv and integ
    * solving differential equations "by hand"
    * expx, sinx, cosx

\begin{code}
module DSLsofMath.Live_6_1 where
import DSLsofMath.FunExp hiding (eval, f)
import DSLsofMath.W05
import DSLsofMath.W06
import DSLsofMath.Simplify

-- x :: Num a => Poly a
-- x = [0,1]

p1 :: Poly REAL
p1 = 1 - x

p2 :: Poly REAL
p2 = x^2 + 1

hej1 :: PowerSeries REAL
hej1 = 1/p1

type PS a = PowerSeries a
der :: Fractional a => PS a -> PS a
der cs = zipWithP (*) (numbersFrom 1) (tailP cs)


test = takePoly 10 (1 / Cons 1 (Single (-1))^2)

numbersFrom :: Num a => a -> PS a
numbersFrom n = Cons n (numbersFrom (n+1))

tailP :: PS a -> PS a
tailP (Cons a as) = as

zipWithP ::
    (   a ->    b ->    c ) ->
    (PS a -> PS b -> PS c )
zipWithP op (Single x)   (Single y)   = Single  (op x y)
zipWithP op (Single x)   (Cons y _)   = Single  (op x y)
zipWithP op (Cons x _)   (Single y)   = Single  (op x y)
zipWithP op (Cons x xs)  (Cons y ys)  = Cons    (op x y)  (zipWithP op xs ys)

hej1' = der hej1

hej = integ hej 1

si = integ co    0
co = integ (-si) 1

\end{code}
test1' = takePoly 10 (der hej1)

data Poly a = Single a | Cons a (Poly a)

si = [0,  1, -0  ,-1/6, 0    ]
co = [1,  0,  1/2, -0 , -1/24]

-- sanity check:

si 0 = 0,   si' 0 = 1
co 0 = 1,   co' 0 = 0
