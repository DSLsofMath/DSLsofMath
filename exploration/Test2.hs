module Test2 where

import MatrixAlgebra2

test :: Num a => a
test = 3

q :: Num a => Mat a
q = Q 1 2 3 4

q2 :: Num a => Mat a
q2 = Q q q q q

a :: Num a => Mat a
a = Q 0 1 1 0
-- a*a==1
-- ? a' = Q 0 a' a' 0


rotate :: Num a => Mat a -> Mat a
rotate q = a*q*a

-- Hypothesis:
prop_rotate :: (Eq a, Num a) => Mat a -> Bool
prop_rotate m = rotate (rotate m) == m
