{-# LANGUAGE ConstraintKinds #-}
module DSLsofMath.Algebra where

import qualified Data.Ratio

import qualified Prelude
import Prelude (Int,Integer,Float,Double, Foldable (..), (==), Monoid(..), Ord(..) ,Real(..), Enum(..), snd, Rational, Functor(..), Eq(..), Bool(..), Semigroup(..))

class Additive a where
  zero :: a
  (+) :: a -> a -> a

class Multiplicative a where
  one :: a
  (*) :: a -> a -> a

type Ring a = (Group a, Multiplicative a)

instance Group Integer where
  negate = Prelude.negate

instance Group Int where
  negate = Prelude.negate

instance Additive Integer where
  (+) = (Prelude.+)
  zero = 0

instance Additive Int where
  (+) = (Prelude.+)
  zero = 0

instance Multiplicative Integer where
  (*) = (Prelude.*)
  one = 1

instance Multiplicative Int where
  (*) = (Prelude.*)
  one = 1

class Additive a => Group a where
  negate :: a -> a

type Field a = (Ring a, Division a)


times :: Additive a => Integer -> a -> a
times n0 = if n0 < 0 then Prelude.error "Algebra.Classes.times: negative number of times" else go n0
  where go 0 _ = zero
        go n x = if r == 0 then y + y else x + y + y
          where (m,r) = n `Prelude.divMod` 2
                y = go m x

mult :: Group a => Integer -> a -> a
mult n x = if n < 0 then negate (times (negate n) x) else times n x

fromRational :: (Field a) => Data.Ratio.Ratio Integer -> a
fromRational x  =  fromInteger (Data.Ratio.numerator x) / fromInteger (Data.Ratio.denominator x)

fromInteger :: Ring a => Integer -> a
fromInteger n = mult n one

class Multiplicative a => Division a where
  recip :: a -> a

(/) :: Division a => a -> a -> a
x / y           =  x * recip y

(-) :: Group a => a -> a -> a
x - y           =  x - negate y
