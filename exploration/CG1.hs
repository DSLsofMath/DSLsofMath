{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Exploring optimisation as presented in "An introduciton to the
-- conjugate gradient method without the agonizing pain"

module CG1 where

import Prelude hiding (id)
-- import Test.QuickCheck

-- | S for Scalar
type S = Double

-- | for simplicity only 2 by 2 matrices for now
type M2 = ( S, S
          , S, S)

-- | vectors of dimension 2
type V2 = (S, S)

instance Num a => Num (a, a) where
  fromInteger x = (fromInteger x, fromInteger x)
  (a, b) + (x, y) = (a + x, b + y)
  (a, b) * (x, y) = (a * x, b * y)
  abs (a, b) = (abs a, abs b)
  signum = undefined
  negate (a, b) = (-a, -b)

instance Fractional a => Fractional (a, a, a, a) where
  fromRational a = (fromRational a, fromRational a, fromRational a, fromRational a)
  recip (a, b, c, d) = (recip a, recip b, recip c, recip d)

instance Num a => Num (a, a, a, a)  where
  fromInteger x = ( fromInteger x, fromInteger x
                  , fromInteger x, fromInteger x)
  (a, b, c, d) + (x, y, z, w) = ( a+x, b+y
                                , c+z, d+w)
  (a, b, c, d) * (x, y, z, w) = ( a*b+b*z, a*y+b*w
                                , c*x+d*z, c*y+d*w) -- matrix multiplication
  abs (a, b, c, d) = (abs a, abs b, abs c, abs d)
  signum = undefined
  negate (a, b, c, d) = (negate a, negate b, negate c, negate d)

instance Fractional a => Fractional (a, a) where
  fromRational a = (fromRational a, fromRational a)
  recip (a, b) = (recip a, recip b)


-- | The identity matrix
id :: M2
id = ( 1, 0
     , 0, 1)

inner :: V2 -> V2 -> S
inner (a, b) (c, d) = a*c + b*d

-- | Pre-multiply with vector
preMul :: V2 -> M2 -> V2
preMul (a, b) ( x, y
              , z, w) = ( a * x + b * z
                        , a * y + b * w)

-- | Post-multiply with vector
postMul :: M2 -> V2 -> V2
postMul ( x, y
        , z, w) (a, b) = ( x * a + y * b
                         , z * a + w * b)

-- | v^T A v = ppm v A
--
prePostMul :: V2 -> M2 -> S
prePostMul v a = inner (preMul v a) v

transpose :: M2 -> M2
transpose ( x, y
          , z, w) = ( x, z
                    , y, w)

scaleM :: S -> M2 -> M2
scaleM x ( a, b
         , c, d) = ( x * a, x * b
                   , x * c, x * d)

scaleV :: S -> V2 -> V2
scaleV x (a, b) = (x*a, x*b)

det :: M2 -> S
det ( a, b
    , c, d) = a*d - b*c

-- Wrapper type to enable a QuickCheck generator for non-zero values
newtype NonZero a = NonZero {unNonZero :: a}
  deriving (Eq, Show, Num)

-- a matrix m is postive definite if for all nonzero x . x^Tmx > 0
prop_positive_definite :: M2 -> NonZero V2 -> Bool
prop_positive_definite m = \(NonZero v) -> prePostMul v m > 0

-- | Example vector and matrix
a :: M2
a = ( 3, 2
    , 2, 6)

b :: V2
b = (2, 8)

c :: S
c = 0

-- quadratic form
f :: V2 -> S
f x = 0.5 * prePostMul x a - inner b x + c

-- gradient of quadradic form
f' :: V2 -> V2
f' x = 0.5 * postMul (transpose a) x + 0.5 * postMul a x - b

-- error, how far from goal are we
-- e_i = x_i - x
-- residual, how far from the correct value of b
-- r_i = b - Ax_i = -Ae_i
-- "the error transformed by A into the same space as b"


-- steepest descent takes a step
-- x_1 = x_0 + alpha*r_0
-- how big should alpha be? p. 6
sdStep :: V2 -> V2
sdStep x = let r     = b - postMul a x            -- r  = b - Ax
               alpha = inner r r / prePostMul r a -- a  = (r^T r)/(r^T A r)
               x'    = x + scaleV alpha r         -- x' = x + a r
           in x'


-- Jacobi iterations, p 11

-- a matrix is diagonal if all but the diagonal is 0
prop_diagonal :: M2 -> Bool
prop_diagonal ( _, b
              , c, _) = b == 0 && c == 0 -- possibly with some epsilon

-- invert a diagonal matrix
invertDiag :: M2 -> M2
invertDiag ( a, _
           , _, d) = ( recip a, 0
                     , 0      , recip d)

-- split matrix into two parts: D whose diagonal elemts are equal to
-- A's and off diagonal elements are 0 and E whose diagonal
-- elements are 0 and off diagonal elements equal to A's
jacobiSplit :: M2 -> (M2, M2)
jacobiSplit ( a, b
            , c, d) = ( ( a, 0
                        , 0, d)
                      , ( 0, b
                        , c, 0))

jacobiStep :: V2 -> V2
jacobiStep x =
  let (d, e) = jacobiSplit a
      f  = (negate (invertDiag d)) * e -- can precompute this
      z  = postMul (invertDiag d) b
      x' = postMul f x + z
  in x'


-----

-- | v1^T A v2 = 0 <=> v1, v2 are A orthogonal
prop_A_orthogonal :: M2 -> V2 -> V2 -> Bool
prop_A_orthogonal a v1 v2 = inner (preMul v1 a) v2 == 0
