{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
-- | Power series, code from Power Series, Power Serious (M. Douglas McIlroy)

module PowerSeries where

import Control.Applicative (liftA2)

import GHC.Exts

default (Integer, Rational, Double)

data PS a = Cons a (PS a)

showPS :: Show a => PS a -> String
showPS fs = go 20 fs
  where go 0 _ = "..."
        go n (Cons f fs) = show f ++ ", " ++ go (pred n) fs

zipWithPS f (Cons a as) (Cons b bs) = Cons (f a b) (zipWithPS f as bs)

-- to use list syntax for power series
instance Num a => IsList (PS a) where
  type Item (PS a) = a
  fromList = foldr Cons 0
  toList (Cons f fs) = f : toList fs

instance Functor PS where
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative PS where
  pure a = let p = Cons a p in p
  fs <*> as = zipWithPS ($) fs as

instance Num a => Num (PS a) where
  fromInteger n = Cons (fromInteger n) (pure 0)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  (+) = liftA2 (+)
  (Cons f fs) * (Cons g gs) = Cons (f * g) (f .* gs + fs * (Cons g gs))

infixl 7 .*
-- | multiply with scalar
(.*) :: Num a => a -> PS a -> PS a
a .* ps = (*) <$> pure a <*> ps

instance (Eq a, Fractional a) => Fractional (PS a) where
  fromRational r = Cons (fromRational r) (pure 0)
  (Cons f fs) / (Cons g gs)
    | f == 0
    , g == 0
    = fs / gs
    | otherwise
    = let q = f / g in Cons q ((fs - q .* gs) / (Cons g gs))

-- | Composition
compose (Cons f fs) (Cons g gs)
  | g == 0 = Cons f (gs * (compose fs (Cons g gs)))
  | otherwise = error "compose: first term not 0"

-- | Reversion,
-- Given F find R such that F(R(x)) = x
revert (Cons f fs)
  | f == 0    = let rs = Cons 0 (1 / (compose fs rs)) in rs
  | otherwise = error "revert: first term not 0"


deriv (Cons _ fs) = (*) <$> [1..] <*> fs

integral fs = Cons 0 ((/) <$> fs <*> [1..])

-- prop_int_deriv_id = \fs -> deriv (integral fs) == fs -- no equality on streams, bisimilarity

expx = 1 + integral expx

sinx = integral cosx
cosx = 1 - integral sinx


sqrtx (Cons 0 (Cons 0 fs)) = Cons 0 (sqrtx fs)
sqrtx (Cons 1 fs) = let qs = 1 + integral ((deriv (Cons 1 fs)) / (2 .* qs)) in qs
sqrtx _ = error "sqrtx: first terms wrong"
