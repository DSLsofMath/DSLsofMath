
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chebyshev where

import Prelude hiding (Num(..),Fractional(..))
import Prelude (abs)
import Algebra.Classes
import Data.Map as Map

type Nat = Int
newtype T a = T (Map Nat a) deriving Show
deriving instance  Additive a => (Additive (T a))
deriving instance  Group a => (Group (T a))
deriving instance  Additive a => (AbelianAdditive (T a))
deriving instance Ring a => (Module a (T a))

-- Chebyshev vector basis.
t i = T (singleton i one)

-- Product of Chebyshev polynomials
instance Field a => Multiplicative  (T a) where
  one = t 0
  T a * T b = add [(x*y/2)*^ (t (i+j) + t (abs (i-j))) | (i,x) <- toList a, (j,y) <- toList b]


(*<) :: Module a (f a) => a -> f a -> f a
(*<) = (*^)

-- derivative of Chebyshev polynomial T(k), copied from
-- https://scicomp.stackexchange.com/questions/28309/derivatives-of-a-chebychev-polynomial
q :: forall a. Field a => Nat -> T a
q 0 = zero
q 1 = t 0
q k = 2 *< t (k-1) + 2 *< (t 1 * q (k-1)) - q (k-2)

test :: T Double
test = q 0

-- >>> q 3
-- T (fromList [(0,3.0),(2,6.0)])

