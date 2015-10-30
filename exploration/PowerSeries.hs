{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Power series, code from Power Series, Power Serious (M. Douglas McIlroy)

module PowerSeries where

import Control.Applicative (liftA2)

import GHC.Exts

default (Integer, Rational, Double)

-- | where an empty power series is the series 0, 0, ...
newtype PS a = PS {unPS :: [a]}
  deriving (Functor)

uncons :: Num a => PS a -> (a, PS a)
uncons (PS (a:as)) = (a, PS as)
uncons (PS _) = (0, PS [])

cons a (PS as) = PS (a:as)

pattern (:.) f fs <- (uncons -> (f, fs))
  where (:.) f fs = cons f fs

infixr 5 :.

x :: Num a => PS a
x = PS [0,1]
ps0 :: Num a => PS a
ps0 = PS []

instance (Num a, Show a) => Show (PS a) where
  show as = "[" ++ go as ++ "]"
    where
      go (PS (a:as)) = show a ++ ", " ++ go (PS as)
      go (PS _)      = "0, 0, ..."

-- | Only works with finite power series
instance (Num a, Eq a) => Eq (PS a) where
  (PS []) == (PS []) = True
  (uncons -> (f, fs)) == (uncons -> (g, gs)) =
    f == g && fs == gs

-- to use list syntax for power series
instance IsList (PS a) where
  type Item (PS a) = a
  fromList = PS
  toList (PS l) = l

-- | A "zippy" applicative
instance Applicative PS where
  pure a = PS (repeat a)
  fs <*> as = PS (zipWith ($) (unPS fs) (unPS as))

instance Num a => Num (PS a) where
  fromInteger n = cons (fromInteger n) ps0
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  (+) = liftA2 (+)
  (uncons -> (f, fs)) * (uncons -> (g, gs)) =
    cons (f * g) (f .* gs + fs * (cons g gs))

infixl 7 .*
-- | multiply with scalar
(.*) :: Num a => a -> PS a -> PS a
a .* ps = (*) <$> pure a <*> ps

instance (Eq a, Fractional a) => Fractional (PS a) where
  fromRational r = cons (fromRational r) ps0
  (uncons -> (f, fs)) / (uncons -> (g, gs))
    | f == 0
    , g == 0
    = fs / gs
    | otherwise
    = let q = f / g in cons q ((fs - q .* gs) / (cons g gs))


-- | Composition
compose :: (Eq a, Num a) => PS a -> PS a -> PS a
compose (uncons -> (f, fs)) (uncons -> (g, gs))
  | g == 0 = cons f (gs * (compose fs (cons g gs)))
  | otherwise = error "compose: first term not 0"

-- | Reversion,
-- Given F find R such that F(R(x)) = x
revert :: (Eq a, Fractional a) => PS a -> PS a
revert (uncons -> (f, fs))
  | f == 0    = let rs = 0 :. (1 / (compose fs rs)) in rs
  | otherwise = error "revert: first term not 0"


deriv :: (Enum t, Num t) => PS t -> PS t
deriv (uncons -> (_, fs)) = (*) <$> [1..] <*> fs

integral :: (Enum t, Fractional t) => PS t -> PS t
integral fs = cons 0 ((/) <$> fs <*> [1..])

-- prop_int_deriv_id = \fs -> deriv (integral fs) == fs -- no equality on streams

expx :: PS Rational
expx = 1 + integral expx

sinx, cosx :: PS Rational
sinx = integral cosx
cosx = 1 - integral sinx

sqrt' :: (Enum a, Eq a, Fractional a) => PS a -> PS a
sqrt' (0 :. 0 :. fs) = 0 :. sqrt' fs
sqrt' (1 :. fs) = let qs = 1 + integral ((deriv (1 :. fs)) / (2 .* qs)) in qs
sqrt' _ = error "sqrt': first terms wrong"


-- | enumeration of binary trees
ts = 1 :. (ts ^ 2)

-- | pascals triangle
pascal :: PS (PS Rational)
pascal = 1 / [1, -[1,1]]
