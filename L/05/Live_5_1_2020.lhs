\begin{code}
module Live_5_1_2020 where
import Data.Complex
import Data.Ratio
import Text.Printf
import DSLsofMath.FunNumInst

type Poly a = [a]
evalP :: Num a => [a] -> (a -> a)
evalP []      x = 0
evalP (a0:as) x = a0 + x*evalP as x

ch2P = [-1,0,2]

instance Num a => Num [a] where
  (+) = addP

addP :: Num a => Poly a -> Poly a -> Poly a
addP as bs = zipWithLonger (+) as bs

zipWithLonger :: (a->a->a) -> [a] -> [a] -> [a]
zipWithLonger op [] bs = bs  -- 0+p == p
zipWithLonger op as [] = as  -- p+0 == p
zipWithLonger op (a:as) (b:bs) = op a b : zipWithLonger op as bs

\end{code}

Några algebraiska egenskaper:

eval [a] = const a
eval (0:as) = \x-> x * eval as x = id*eval as
eval (a:as) = eval ([a] + (0:as)) = eval [a] + id * eval as = const a + id * eval as

\begin{code}
-- A family of polynomial(?) functions (Chebyshev)
ch :: Floating a => Integer -> a -> a
ch n x = cos (fromInteger n*acos x)
\end{code}

A polynomial = abstract syntax
A polynomial function = semantics :: REAL -> REAL


----------------------------------------------------------------
-- Not part of the lecture (but useful helper code)

From polynomial function to a polynomial (parsing?):

Specification: (naming: "lave" = reverse "eval")
  lave :: Fractional a => Int -> (a->a) -> Poly a
  forall x. eval (lave n f) x = f x

(Should work if f is actually a polynomial function of degree n.)

General idea: evaluate the polynomial at (n+1) points and solve for the coefficients.
Best choice of points are (n+1)-roots of unity.

\begin{code}

i = 0:+1
coeff :: Integer -> Integer -> Integer -> Complex Double
coeff n j k = exp(2*pi*i*fromRational ((j*k) % (n+1)))
  where
lave n f = as
  where (_xs, _ys, as) = lave' n f

lave' n f = (xs, ys, map a [0..n])
  where
    xs = map (coeff n (-1)) [0..n]
    ys = map f xs
    a j = sum (zipWith (*) ys (map (coeff n j) [0..n])) / fromInteger (n+1)

test :: Integer -> [String]
test n = map ((printf "%5.1f").realPart) (lave n (ch n))
\end{code}
