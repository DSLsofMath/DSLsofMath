{-# LANGUAGE FlexibleInstances #-}
-- |

module CG1 where

type M2 = (Double,Double
          ,Double,Double)
type V2 = (Double,Double)
type S = Double

instance Num a => Num ((,) a a) where
  fromInteger x = (fromInteger x,fromInteger x)
  (a,b) + (x,y) = (a + x,b + y)
  (a,b) * (x,y) = (a * x,b * y)
  abs (a,b) = (abs a,abs b)
  signum = undefined
  negate (a,b) = (-a,-b)

instance Fractional a => Fractional (a,a) where
  fromRational a = (fromRational a,fromRational a)
  recip (a,b) = (recip a,recip b)

-- | Inner product
innerProd :: V2 -> V2 -> S
innerProd (a,b) (c,d) = a*c + b*d

preProd :: V2 -> M2 -> V2
preProd (a,b) (x,y
              ,z,w) = (a * x + b * z
                      ,a * y + b * w)

postProd :: M2 -> V2 -> V2
postProd (x,y
         ,z,w) (a,b) = (x * a + y * b
                       ,z * a + w * b)

-- | v^T A v = ppm v A
--
prePostProd :: V2 -> M2 -> S
prePostProd v a = innerProd (preProd v a) v

transpose :: M2 -> M2
transpose (x,y
          ,z,w) = (x,z
                  ,y,w)

-- a matrix m is postive definite if for all x . x^Tmx > 0
prop_positive_definite :: M2 -> V2 -> Bool
prop_positive_definite m = \v -> prePostProd v m > 0

-- | Example vect and matrix
a :: M2
a = (3,2
    ,2,6)

b :: V2
b = (2,8)

c :: S
c = 0

-- quadratic form
f x = 0.5 * prePostProd x a - innerProd b x + c

-- gradient of quadradic form
f' x = 0.5 * postProd (transpose a) x + 0.5 * postProd a x - b

-- error, how far from goal are we
-- e_i = x_i - x
-- residual, how far from the correct value of b
-- r_i = b - Ax_i = -Ae_i
-- "the error transformed by A into the same space as b"


-- steepest descent takes a step
-- x_1 = x_0 + alpha*r_0
-- how big should alpha be?
sdStep x = let r = b - postProd a x
               alpha = innerProd r r / prePostProd r a
               x' = x + (alpha,alpha) * r
           in x'
