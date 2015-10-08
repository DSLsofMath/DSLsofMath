{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Working with 2x2 matrices

module MatrixAlgebra1 where


-- | S for Scalar
type S = Double

-- | for simplicity only 2 by 2 matrices for now
data M2 s = Q s s
              s s
  deriving (Functor,Eq,Ord,Show)

-- | vectors of dimension 2
data V2 s = V s s
  deriving (Functor,Eq,Ord,Show)

instance Num a => Num (V2 a) where
  fromInteger x     = V (fromInteger x) (fromInteger x)
  (V a b) + (V x y) = V (a + x) (b + y)
  (V a b) * (V x y) = V (a * x) (b * y)
  abs (V a b)       = V (abs a) (abs b)
  signum            = error "signum: undefined for pairs"
  negate (V a b)    = V (negate a) (negate b)

instance Fractional a => Fractional (V2 a) where
  fromRational a = V (fromRational a) (fromRational a)
  recip (V a b)  = V (recip a) (recip b)

instance Num a => Num (M2 a)  where
  fromInteger x = Q (fromInteger x) 0 0 (fromInteger x)
  (Q a b c d) + (Q x y z w) = Q (a+x) (b+y) (c+z) (d+w)

  -- matrix multiplication
  (Q a b c d) * (Q x y z w) = Q (a*x+b*z) (a*y+b*w)
                                (c*x+d*z) (c*y+d*w)

  -- TODO: matrix norm? -- No, abs :: Num a => a -> a
  abs = fmap abs

  signum = error "signum: undefined for 2x2 matrices"

  negate = fmap negate

instance Fractional a => Fractional (M2 a) where
  fromRational a = Q (fromRational a) 0
                     0                (fromRational a)

  -- TODO: matrix inverse?
  recip = fmap recip

-- TODO: implement 2x2 (block) matrix inverse

inner :: Num a => V2 a -> V2 a -> a
inner (V a b) (V c d) = a*c + b*d

-- | Pre-multiply with vector
preMul :: Num a => V2 a -> M2 a -> V2 a
preMul (V a b) (Q x y
                  z w) = V (a * x + b * z)
                           (a * y + b * w)

prop_identity_preMul :: V2 Integer -> Bool
prop_identity_preMul = \v -> v `preMul` 1 == v

-- | Post-multiply with vector
postMul :: Num a => M2 a -> V2 a -> V2 a
postMul (Q x y
           z w) (V a b) = V (x * a + y * b)
                            (z * a + w * b)

prop_identity_postMul :: V2 Integer -> Bool
prop_identity_postMul = \v -> 1 `postMul` v == v

-- | v^T A v = ppm v A
--
prePostMul :: Num a => V2 a -> M2 a -> a
prePostMul v a = inner (preMul v a) v

transpose :: M2 a -> M2 a
transpose (Q x y
             z w) = Q x z
                      y w

scaleM :: Num a => a -> M2 a -> M2 a
scaleM x (Q a b
            c d) = Q (x * a) (x * b)
                     (x * c) (x * d)

scaleV :: Num a => a -> V2 a -> V2 a
scaleV x (V a b) = V (x*a) (x*b)

det :: Num a => M2 a -> a
det (Q a b
       c d) = a*d - b*c


-- | invert a diagonal matrix
invertDiag :: Fractional a => M2 a -> M2 a
invertDiag (Q a _
              _ d) = Q (recip a) 0
                       0         (recip d)

-- * Properties of matrix operations

-- | Wrapper type to enable a QuickCheck generator for non-zero values
newtype NonZero a = NonZero {unNonZero :: a}
  deriving (Eq, Show, Num)

-- | a matrix m is postive definite if for all nonzero x . x^Tmx > 0
prop_positive_definite :: (Num a, Ord a) => M2 a -> NonZero (V2 a) -> Bool
prop_positive_definite m = \(NonZero v) -> prePostMul v m > 0


-- | a matrix is diagonal if all but the diagonal is 0
prop_diagonal :: (Num a, Eq a) => M2 a -> Bool
prop_diagonal (Q _ b
                 c _) = b == 0 && c == 0 -- possibly with some epsilon

-- | Identity matrix is left unit of matrix multiplication
prop_id_left_unit :: M2 Integer -> Bool
prop_id_left_unit = \m -> 1 * m == m

-- | Identity matrix is right unit of matrix multiplication
prop_id_right_unit :: M2 Integer -> Bool
prop_id_right_unit = \m -> m * 1 == m

prop_mmult_assoc :: M2 Integer -> M2 Integer -> M2 Integer -> Bool
prop_mmult_assoc = \m1 m2 m3 -> m1 * (m2 * m3) == (m1 * m2) * m3

prop_madd_assoc :: M2 Integer -> M2 Integer -> M2 Integer -> Bool
prop_madd_assoc = \m1 m2 m3 -> m1 + (m2 + m3) == (m1 + m2) + m3

prop_madd_comm :: M2 Integer -> M2 Integer -> Bool
prop_madd_comm = \m1 m2 -> m1 + m2 == m2 + m1
