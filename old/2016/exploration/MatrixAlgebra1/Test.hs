{-# LANGUAGE FlexibleContexts #-}
module MatrixAlgebra1.Test where

import MatrixAlgebra1
import MatrixAlgebra1.CG hiding (us)

import Control.Applicative
import Test.QuickCheck hiding (NonZero)


-- I think this instance should be pushed into QuickCheck instead of
-- the version with Ord as a requirement.
instance (Eq a, Num a, Arbitrary a) => Arbitrary (NonZero a) where
  arbitrary = fmap NonZero $ arbitrary `suchThat` (/= 0)

  shrink (NonZero x) = [ NonZero x' | x' <- shrink x, x' /= 0 ]

instance Arbitrary a => Arbitrary (M2 a) where
  arbitrary = Q <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Q a b c d) = [Q a' b' c' d' | (a',b',c',d') <- shrink (a,b,c,d)]

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V <$> arbitrary <*> arbitrary
  shrink (V v1 v2) = [V v1' v2' | (v1',v2') <- shrink (v1,v2)]

test x = quickCheck (prop_positive_definite x)

q :: M2 S
q = Q 1 1
      2 1

-- coordinate axes
us :: [V2 S]
us@[u1, u2] = [(V 1 0),(V 0 1)]

-- compute some A orthogonal vectors
[d1,d2] = gsConjugation us

main  = do --quickCheck (prop_positive_definite _)
           quickCheck (prop_linearly_independent_R2 (u1,u2))
           quickCheck (prop_linearly_independent_R2 (d1,d2))
           quickCheck prop_id_left_unit
           quickCheck prop_id_right_unit
           quickCheck prop_mmult_assoc
           quickCheck prop_madd_assoc
           quickCheck prop_madd_comm
           quickCheck prop_identity_preMul
           quickCheck prop_identity_postMul

type M4 = M2 (M2 S)

m4 :: M4
m4 = Q q q q q

n4 :: M4
n4 = 1
