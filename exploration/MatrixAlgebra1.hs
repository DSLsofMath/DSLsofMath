{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Working with 2x2 matrices

module MatrixAlgebra1 where

import Prelude hiding (id)
-- import Test.QuickCheck

-- | S for Scalar
type S = Double

-- | for simplicity only 2 by 2 matrices for now
type M2 s = ( s, s
            , s, s)

-- | vectors of dimension 2
type V2 s = (s, s)

instance Num a => Num (a, a) where
  fromInteger x   = (fromInteger x, fromInteger x)
  (a, b) + (x, y) = (a + x, b + y)
  (a, b) * (x, y) = (a * x, b * y)
  abs (a, b)      = (abs a, abs b)
  signum          = error "signum: undefined for pairs"
  negate (a, b)   = (negate a, negate b)

instance Fractional a => Fractional (a, a) where
  fromRational a = (fromRational a, fromRational a)
  recip (a, b) = (recip a, recip b)

instance Num a => Num (a, a, a, a)  where
  fromInteger x = ( fromInteger x, 0
                  , 0            , fromInteger x)
  (a, b, c, d) + (x, y, z, w) = ( a+x, b+y
                                , c+z, d+w)
  (a, b,
   c, d) * (x, y,
            z, w) = ( a*x+b*z, a*y+b*w
                    , c*x+d*z, c*y+d*w) -- matrix multiplication
  abs (a, b, c, d) = (abs a, abs b, abs c, abs d) -- TODO: matrix norm?
  signum = error "signum: undefined for 2x2 matrices"
  negate (a, b, c, d) = ( negate a, negate b
                        , negate c, negate d)

instance Fractional a => Fractional (a, a, a, a) where
  fromRational a = ( fromRational a, 0
                   , 0             , fromRational a)
  recip (a, b, c, d) = (recip a, recip b,  -- TODO: matrix inverse?
                        recip c, recip d)


inner :: Num a => V2 a -> V2 a -> a
inner (a, b) (c, d) = a*c + b*d

-- | Pre-multiply with vector
preMul :: Num a => V2 a -> M2 a -> V2 a
preMul (a, b) ( x, y
              , z, w) = ( a * x + b * z
                        , a * y + b * w)

-- | Post-multiply with vector
postMul :: Num a => M2 a -> V2 a -> V2 a
postMul ( x, y
        , z, w) (a, b) = ( x * a + y * b
                         , z * a + w * b)

-- | v^T A v = ppm v A
--
prePostMul :: Num a => V2 a -> M2 a -> a
prePostMul v a = inner (preMul v a) v

transpose :: M2 a -> M2 a
transpose ( x, y
          , z, w) = ( x, z
                    , y, w)

scaleM :: Num a => a -> M2 a -> M2 a
scaleM x ( a, b
         , c, d) = ( x * a, x * b
                   , x * c, x * d)

scaleV :: Num a => a -> V2 a -> V2 a
scaleV x (a, b) = (x*a, x*b)

det :: Num a => M2 a -> a
det ( a, b
    , c, d) = a*d - b*c


-- | invert a diagonal matrix
invertDiag :: Fractional a => M2 a -> M2 a
invertDiag ( a, _
           , _, d) = ( recip a, 0
                     , 0      , recip d)

-- * Properties of matrix operations

-- | Wrapper type to enable a QuickCheck generator for non-zero values
newtype NonZero a = NonZero {unNonZero :: a}
  deriving (Eq, Show, Num)

-- | a matrix m is postive definite if for all nonzero x . x^Tmx > 0
prop_positive_definite :: (Num a, Ord a) => M2 a -> NonZero (V2 a) -> Bool
prop_positive_definite m = \(NonZero v) -> prePostMul v m > 0


-- | a matrix is diagonal if all but the diagonal is 0
prop_diagonal :: (Num a, Eq a) => M2 a -> Bool
prop_diagonal ( _, b
              , c, _) = b == 0 && c == 0 -- possibly with some epsilon

-- | Identity matrix is left unit of matrix multiplication
prop_id_left_unit :: M2 Integer -> Bool
prop_id_left_unit = \m -> 1 * m == m

-- | Identity matrix is right unit of matrix multiplication
prop_id_right_unit :: M2 Integer -> Bool
prop_id_right_unit = \m -> m * 1 == m

prop_mmult_assoc :: M2 Integer -> M2 Integer -> M2 Integer -> Bool
prop_mmult_assoc = \m1 m2 m3 -> m1 * (m2 * m3) == (m1 * m2) * m3
