{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Exploring optimisation as presented in "An introduciton to the
-- conjugate gradient method without the agonizing pain"

module CG1 where

import MatrixAlgebra1

-- * Example vector, matrix and corresponding quadratic form from paper.

-- |
-- > ( 3, 2
-- > , 2, 6)
a :: M2 S
a = ( 3, 2
    , 2, 6)

-- |
-- > (2, 8)
b :: V2 S
b = (2, 8)

-- | > 0
c :: S
c = 0

-- | quadratic form
f :: V2 S -> S
f x = 0.5 * prePostMul x a - inner b x + c

-- | gradient of quadradic form
f' :: V2 S -> V2 S
f' x = 0.5 * postMul (transpose a) x + 0.5 * postMul a x - b

-- error, how far from goal are we
-- e_i = x_i - x
-- residual, how far from the correct value of b
-- r_i = b - Ax_i = -Ae_i
-- "the error transformed by A into the same space as b"


-- * Steepest descent

-- steepest descent takes a step
-- x_1 = x_0 + alpha*r_0
-- how big should alpha be? p. 6
sdStep :: V2 S -> V2 S
sdStep x = let r     = b - postMul a x            -- r  = b - Ax
               alpha = inner r r / prePostMul r a -- a  = (r^T r)/(r^T A r)
               x'    = x + scaleV alpha r         -- x' = x + a r
           in x'

-- * Jacobi iterations, p. 11

-- | split matrix into two parts: D whose diagonal elemts are equal to
-- A's and off diagonal elements are 0 and E whose diagonal
-- elements are 0 and off diagonal elements equal to A's
jacobiSplit :: M2 S -> (M2 S, M2 S)
jacobiSplit ( a, b
            , c, d) = ( ( a, 0
                        , 0, d)
                      , ( 0, b
                        , c, 0))

-- | Jacobi steps, Jacobi method doesn't always terminate
jacobiStep :: V2 S -> V2 S
jacobiStep x =
  let (d, e) = jacobiSplit a
      f  = (negate (invertDiag d)) * e -- can precompute this
      z  = postMul (invertDiag d) b
      x' = postMul f x + z
  in x'


-- * Conjugate directions

-- | v1^T A v2 = 0 <=> v1, v2 are A orthogonal
prop_A_orthogonal :: M2 S -> V2 S -> V2 S -> Bool
prop_A_orthogonal a v1 v2 = inner (preMul v1 a) v2 == 0

cdStep :: V2 S -- ^ d_n, search direction
       -> V2 S -- ^ x_n
       -> V2 S -- ^ x_{n+1}
cdStep d x =
  let r     = b - postMul a x            -- r  = b - Ax
      alpha = inner d r / prePostMul d a -- a  = (d^T r)/(d^T A d)
      x'    = x + scaleV alpha r         -- x' = x + a r
  in x'


-- ** Gram-Schmidt conjugation
-- Find A orthogonal search directions

-- | start with n linearly independent vectors us = [u_0, ..., u_{n-1}], here the coordinate axes
us :: [V2 S]
us = [(1,0), (0,1)]

-- prop_linearly_independent :: [V2] -> [NonZero S] -> Bool -- also want same length
-- prop_linearly_independent vs = \as -> sum (zipWith scaleV as vs) /= 0

-- | Vectors are linearly independent if
-- a_1 v_1 + ... + a_n v_n = 0 only when a_i = 0, for all i
prop_linearly_independent_R2 :: (V2 S, V2 S) -> (NonZero S, NonZero S) -> Bool
prop_linearly_independent_R2 (v1, v2)
  = \(NonZero s1, NonZero s2) -> (scaleV s1 v1 + scaleV s2 v2) /= 0

-- | Gram-Schmidt conjugation, p. 25
gsConjugation us
  = let u i = us !! i
        d i = u i + sum (flip map [0 .. i-1] (\k -> scaleV (beta i k) (d k)))
        beta i j = - (inner (preMul (u i) a) (d j) / (prePostMul (d j) a))
    in map d [0 .. (length us - 1)]


-- prop_gsConjugation_ok us@(u1,u2) =
--   prop_linearly_independent_R2 us ==>
--   prop_linearly_independent_R2 (gsConjugation us)
