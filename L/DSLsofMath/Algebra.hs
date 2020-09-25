{-# LANGUAGE ConstraintKinds #-}
module DSLsofMath.Algebra where

import qualified Data.Ratio

import qualified Prelude
import Prelude (Int,Integer,Float,Double, Foldable (..), (==), Monoid(..), Ord(..) ,Real(..), Enum(..), snd, Rational, Functor(..), Eq(..), Bool(..), Semigroup(..), const, (.))

class Additive a where
  zero :: a
  (+) :: a -> a -> a

class Multiplicative a where
  one :: a
  (*) :: a -> a -> a

type Ring a = (AddGroup a, Multiplicative a)

instance AddGroup Integer where
  negate = Prelude.negate

instance AddGroup Int where
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

class Additive a => AddGroup a where
  negate :: a -> a

type Field a = (Ring a, MulGroup a)


times :: Additive a => Integer -> a -> a
times n0 = if n0 < 0 then Prelude.error "Algebra.Classes.times: negative number of times" else go n0
  where go 0 _ = zero
        go n x = if r == 0 then y + y else x + y + y
          where (m,r) = n `Prelude.divMod` 2
                y = go m x

mult :: AddGroup a => Integer -> a -> a
mult n x = if n < 0 then negate (times (negate n) x) else times n x

fromRational :: (Field a) => Data.Ratio.Ratio Integer -> a
fromRational x  =  fromInteger (Data.Ratio.numerator x) / fromInteger (Data.Ratio.denominator x)

fromInteger :: Ring a => Integer -> a
fromInteger n = mult n one

class Multiplicative a => MulGroup a where
  {-# MINIMAL (recip | (/)) #-}
  recip :: a -> a
  recip x         =  one / x

  (/) :: a -> a -> a
  x / y           =  x * recip y

(-) :: AddGroup a => a -> a -> a
x - y           =  x - negate y


instance Additive a => Additive (x -> a) where
   f + g        =  \x -> f x + g x
   zero = const zero

instance Multiplicative a => Multiplicative (x -> a) where
   f * g        =  \x -> f x * g x
   one = const one

instance AddGroup a => AddGroup (x -> a) where
   negate f     =  negate . f



instance Additive Double where
  (+) = (Prelude.+)
  zero = 0

instance AddGroup Double where
  negate = Prelude.negate

instance MulGroup Double where
  (/) = (Prelude./)
  recip = Prelude.recip

instance Multiplicative Double where
  (*) = (Prelude.*)
  one = 1

ifThenElse :: Bool -> p -> p -> p
ifThenElse c a b = if c then a else b

(^+) :: Multiplicative a => a -> Int -> a

x0 ^+ n0 = if n0 < 0 then Prelude.error "Algebra.Classes.^: negative exponent" else go x0 n0
  where go _ 0 = one
        go x n = if r == 0 then y * y else x * y * y
          where (m,r) = n `Prelude.divMod` 2
                y = go x m

(^) :: MulGroup a => a -> Int -> a
a ^ b | b < 0 = recip (a ^+ (negate b))
      | True = (a ^+ b)

