module Test where

import MatrixAlgebra1
import CG1 hiding (us)


import Test.QuickCheck hiding (NonZero)

-- I think this instance should be pushed into QuickCheck instead of
-- the version with Ord as a requirement.
instance (Eq a, Num a, Arbitrary a) => Arbitrary (NonZero a) where
  arbitrary = fmap NonZero $ arbitrary `suchThat` (/= 0)

  shrink (NonZero x) = [ NonZero x' | x' <- shrink x, x' /= 0 ]

test x = quickCheck (prop_positive_definite x)

q :: M2
q = ( 1, 1
    , 2, 1)

-- coordinate axes
us :: [V2]
us@[u1, u2] = [(1,0),(0,1)]

-- compute some A orthogonal vectors
[d1,d2] = gsConjugation us

tests = do --quickCheck (prop_positive_definite _)
           quickCheck (prop_linearly_independent_R2 (u1,u2))
           quickCheck (prop_linearly_independent_R2 (d1,d2))

type M4 = (M2,M2,M2,M2)

m4 :: M4
m4 = (q,q,
      q,q)

n4 :: M4
n4 = 1
