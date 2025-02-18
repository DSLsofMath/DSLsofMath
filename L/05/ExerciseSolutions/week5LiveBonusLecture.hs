type PolynomialCoefficients a = Int -> a

fromList :: (Num a) => [a] -> PolynomialCoefficients a 
fromList as n | n < length as =  as !! n 
              | otherwise     =  0 


idPoly :: (Num a) => PolynomialCoefficients a
idPoly 1 = 1
idPoly _ = 0

zeroPoly :: (Num a) => PolynomialCoefficients a
zeroPoly = const 0

onePoly :: (Num a) => PolynomialCoefficients a
onePoly 0 = 1 
onePoly _ = 0

recipPoly :: (Fractional a , Num a) => PolynomialCoefficients a -> PolynomialCoefficients a 
-- mulPoly f (recip f)   == onePoly
-- Note onePoly is defined differently on 0 than on any other number, 
-- so we can look at the value at 0 specifically
-- mulPoly (recip f) f 0 == onePoly 0  == 1
-- mulPoly (recip f) f 0 == sum [(recip f i ) * f (0 -i) | i <- [ 0 .. 0] ]
--                       == recip f 0 * f 0
--                       == 1  
-- recip f 0 = 1 / f 0 
recipPoly f 0 = 1 / (f 0)
--recipPoly f 1 = - ((recipPoly f) 0) * (f 1) / (f 0)
--recipPoly f 2 = - (((recipPoly f) 0) * (f 2) + ((recipPoly f) 1 * f (1))) / (f 0 )
--
--Now assume n != 0 
--mulPoly (recip f) f n == onePoly n == 0
--mulPoly (recip f) f n == sum[ (recip f i) * f (n - i ) | i <- [ 0 .. n]]
--We're making a recursive definition of recip f n, and we may assume recip f k is defined for k < n
--so we know the value  of ( (recip f k) * f (n - k )  ) for k <0 [ 0 .. n-1]
--mulPoly (recip f) f n == sum[ (recip f i) * f (n - i ) | i <- [ 0 .. n]] =
--                         sum[ (recip f i) * f (n - i ) | i <- [ 0 .. n-1]] + recip f n * f ( n- n)
--                      == sum[ (recip f i) * f (n - i ) | i <- [ 0 .. n-1]] + recip f n * f 0
--                      == 0 
-- so recip f n = -  (sum[ (recip f i) * f (n - i ) | i <- [ 0 .. n-1]]) / f 0
--
recipPoly f n = - (sum [ (recipPoly f i) * f (n - i) | i <- [ 0 .. n-1] ] )/ (f 0)

sumPoly :: (Num a) =>  PolynomialCoefficients a -> PolynomialCoefficients a -> PolynomialCoefficients a
sumPoly f g n = f n + g n

mulPoly :: (Num a) => PolynomialCoefficients a -> PolynomialCoefficients a -> PolynomialCoefficients a
mulPoly f g n = sum [ ( f i ) * (g (n - i))   | i <- [ 0 .. n] ]




magicnumber :: Int 
magicnumber = 5

showPoly :: (Show a) => PolynomialCoefficients a -> String
showPoly f = concat (take magicnumber [show (c) ++ " X^" ++ show (n) ++ " + " | (n , c) <- pcToList f  ] ) where
  pcToList :: PolynomialCoefficients a -> [ (Int , a) ]
  pcToList f = [ (n , f n) | n <- [0..]]

evalPoly :: (Num a) => PolynomialCoefficients a -> a -> a 
evalPoly f x = sum (take magicnumber [ (f n) * (x^n) |  n <- [ 0 ..]])
