{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Power series, code from Power Series, Power Serious (M. Douglas McIlroy)

module PowerSeries where

import Control.Applicative (liftA2)

import GHC.Exts -- IsList

import Data.Ratio

default (Integer, Rational, Double)

-- | where an empty power series is the series 0, 0, ...
newtype PS a = PS {unPS :: [a]}
  deriving (Functor)

-- to use list syntax for power series
instance IsList (PS a) where
  type Item (PS a) = a
  fromList = PS
  toList (PS l) = l

instance (Num a, Show a) => Show (PS a) where
  show = showPS show

showPS :: (t -> String) -> PS t -> String
showPS show as = "[" ++ go as ++ "]"
    where
      go (PS (a:as)) = show a ++ ", " ++ go (PS as)
      go (PS _)      = "0, 0, ..."


-- | Head and tail of power series
uncons :: Num a => PS a -> (a, PS a)
uncons (PS (a:as)) = (a, PS as)
uncons (PS _) = (0, PS [])

cons a (PS as) = PS (a:as)

-- | the zero power series
pattern Zero <- PS [] -- destruction
  where Zero = PS []  -- construction

-- | Construct/Destruct power series from head and tail
pattern (:.) f fs <- (uncons -> (f, fs)) -- destruction
  where (:.) f fs = cons f fs            -- construction

infixr 5 :.

x :: Num a => PS a
x = [0,1]

-- | Only works with finite power series
instance (Num a, Eq a) => Eq (PS a) where
  Zero       == Zero       = True
  (f :. fs) == (g :. gs) = f == g && fs == gs

-- | A "zippy" applicative
instance Applicative PS where
  pure a = PS (repeat a)
  fs <*> as = PS (zipWith ($) (unPS fs) (unPS as))

instance Num a => Num (PS a) where
  fromInteger n = [fromInteger n]
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum

  Zero + Zero           = Zero
  (f :. fs) + (g :. gs) = (f + g) :. (fs + gs)

  Zero * Zero           = Zero
  (f :. fs) * (g :. gs) =
     (f * g) :. (f .* gs + fs * (g :. gs))

infixl 7 .*
-- | multiply with scalar
(.*) :: Num a => a -> PS a -> PS a
a .* ps = (*) <$> pure a <*> ps

instance (Eq a, Fractional a) => Fractional (PS a) where
  fromRational r = [fromRational r]
  (f :. fs) / (g :. gs)
    | g == 0 -- if g == 0 then division can only succeed if f == 0 as
    , f == 0 -- well
    = fs / gs
    | otherwise
    = let q = f / g in q :. ((fs - q .* gs) / (g :. gs))


-- | Composition
compose :: (Eq a, Num a) => PS a -> PS a -> PS a
compose (f :. fs) (g :. gs)
  | g == 0    = f :. (gs * (compose fs (g :. gs)))
  | otherwise = error "compose: first term not 0"

-- | Reversion,
-- Given F find R such that F(R(x)) = x
revert :: (Eq a, Fractional a) => PS a -> PS a
revert (f :. fs)
  | f == 0    = let rs = 0 :. (1 / (compose fs rs)) in rs
  | otherwise = error "revert: first term not 0"


deriv :: (Enum t, Num t) => PS t -> PS t
deriv (_ :. fs) = (*) <$> [1..] <*> fs

integral :: (Enum t, Fractional t) => PS t -> PS t
integral fs = 0 :. ((/) <$> fs <*> [1..])

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

testPascal = showPS (showPS (showRat)) $ takePS 5 (fmap (takePS 5) pascal)

-- let q = 1; fs=0; g=1; gs= cons (-1) 0 in cons q ((fs - q .* gs) / (cons g gs))

takePS :: Int -> PS t -> PS t
takePS n (PS xs) = PS (take n xs)

showRat :: Rational -> String
showRat r | denominator r == 1   = show (numerator r)
          | otherwise            = show (numerator r) ++"%"++ show (denominator r)

showPSR :: PS Rational -> String
showPSR = showPS showRat
