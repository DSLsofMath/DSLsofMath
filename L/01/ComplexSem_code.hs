module DSLsofMath.ComplexSem where

data ImagUnits = I

i :: ImagUnits
i = I

showIU ::  ImagUnits       ->  String
showIU     I               =   "i"

data ComplexA  =  CPlus1 REAL REAL ImagUnits  -- the form |a + bi|
               |  CPlus2 REAL ImagUnits REAL  -- the form |a + ib|

showCA ::  ComplexA       ->  String
showCA     (CPlus1 x y i)  =  show x ++ " + " ++ show y ++ showIU i
showCA     (CPlus2 x i y)  =  show x ++ " + " ++ showIU i ++ show y

type REAL = Double

testC1 :: [ComplexA]
testC1 =  [  CPlus1 3 2 I  ,    CPlus1 (7/2) (-2/3) I
          ,  CPlus2 0 I pi ,    CPlus1 (-3) 0 I
          ]
testS1 = map showCA testC1

toComplex :: REAL -> ComplexA
toComplex x = CPlus1 x 0 I

data ComplexB = CPlusB REAL REAL ImagUnits

data ComplexC = CPlusC REAL REAL

instance Eq ComplexC where
    CPlusC a b == CPlusC x y = a == x && b == y

newtype ComplexD = CD (REAL, REAL)   deriving Eq

re :: ComplexD        ->  REAL
re z @ (CD (x , y))   =   x

im :: ComplexD        ->  REAL
im z @ (CD (x , y))   =   y

addD :: ComplexD -> ComplexD -> ComplexD
addD (CD (a , b)) (CD (x , y))  =  CD ((a + x) , (b + y))

