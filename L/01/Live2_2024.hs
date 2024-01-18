{-
examples of floats and doubles
which are solutions of equations
they actually cannot be if
interpreted as rational numbers.

'goldenRatio' solves the equation
  goldenRatio * (goldenRatio - 1) = 1
  (which only has irrational solutions)
  but not:
  goldenRatio * goldenRatio - goldenRatio = 1
-}
goldenRatio :: Float
goldenRatio = 1.618034

-- here are some square roots that
-- really have to be irrational
sqrOf17 :: Double
sqrOf17 = 4.123105625617661

-- complex numbners following Adams & Essex
type REAL = Double

data CB where 
  I_CB :: CB
  PlusI1 :: REAL -> REAL -> CB -- a+bi
  PlusI2 :: REAL -> REAL -> CB -- a+ib

ex1 = PlusI1 3 2
ex2 = PlusI1 (7/2) (-2/3)

equalCB :: CB -> CB -> Bool
equalCB (PlusI1 a b) (PlusI1 x y) =
  a == x && b == y

toCB :: REAL -> CB
toCB a = PlusI1 a 0

showCB :: CB -> String
showCB I_CB = "i"
showCB (PlusI1 x y) = show x ++ "+" ++ show y ++ "i"

instance Show CB where
  show = showCB







-- revised (sane) complex numbers
data CC where 
  PlusI :: REAL -> REAL -> CC -- a+bi
  
z = PlusI 1 1
w = PlusI 1 (-1)

equalCC :: CC -> CC -> Bool
equalCC (PlusI a b) (PlusI x y) =
  a == x && b == y

add :: CC -> CC -> CC
add (PlusI a b) (PlusI x y) = PlusI (a+x) (b+y)

mul :: CC -> CC -> CC
mul (PlusI a b) (PlusI x y) =
  PlusI (a*x - b * y) (b*x + a*y)

re, im :: CC -> REAL
re (PlusI a b) = a
im (PlusI a b) = b

toCC :: REAL -> CC
toCC a = PlusI a 0


showCC :: CC -> String
showCC (PlusI x y) = show x ++ "+" ++ show y ++ "i"

instance Show CC where
  show = showCC

-- type of  complex number expressions
data CE where
  FromREAL :: REAL -> CE
  I        :: CE               -- I^2=-1
  Add, Mul :: CE -> CE -> CE
  deriving Show

plusIForCB :: REAL -> REAL -> CE
plusIForCB a b = Add (FromREAL a) (Mul I (FromREAL b))

zE = plusIForCB 1 1

eval :: CE -> CC
eval I            = PlusI 0 1
eval (FromREAL a) = PlusI a 0
eval (Add e1 e2)  = add (eval e1) (eval e2)
eval (Mul e1 e2)  = mul (eval e1) (eval e2)

v = PlusI 0.9 0.1
  
-- powers of complex numbers 
pow :: Integer -> CC -> CC
pow n z =
  if n > 0
  then mul z (pow (n-1) z)
  else PlusI 1 0

{- what about mul :: CC -> CC -> CC?
 we know
    (FromREAL a) * (FromREAL b) = FromREAL (a*b)
  and CC should be distributive:
    z * (w + v) = z * w + z * v
  [blackboard calculation using distributivity and I^2=-1 showed that:
    (a+bi) * (x + yi) = (ax-by) + (bx+ay)i
-}

{-
  Here is the expression that was used to
  create  the picture at the end of the lecture:

  map (\ n -> add (PlusI 100 100) (mul (PlusI 100 0) (pow n v)) ) [1..100]

  and the stub for the svg:

<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    version="1.1">
    <title>Show complex numbers</title>
    <desc>Something nice.</desc>
    <rect width="200" height="200" fill="white" />
    <path d="M 190.0 110.0 L 180.0 118.0, ..." fill="transparent" stroke="black" stroke-width="2px"/>

</svg>

-}
