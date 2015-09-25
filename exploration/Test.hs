module Test where
import CG1
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
