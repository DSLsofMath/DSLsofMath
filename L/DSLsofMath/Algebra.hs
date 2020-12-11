{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module DSLsofMath.Algebra where

import qualified Data.Ratio

import qualified Prelude
import Prelude (Int,Integer,Float,Double, Foldable (..), (==), Monoid(..), Ord(..) ,Real(..), Enum(..), snd, Rational, Functor(..), Eq(..), Bool(..), Semigroup(..), const, (.))
import Data.Complex

-------------------------------
-- Classes
infixl 6 -
infixl 6 +

infixl 7 *
infixl 7 /

class Additive a where
  zero :: a
  (+) :: a -> a -> a

sum :: (Foldable t, Additive a) => t a -> a
sum = foldr (+) zero

times :: Additive a => Integer -> a -> a
times n0 = if n0 < 0  then Prelude.error "Algebra.Classes.times: negative number of times"
                      else go n0
  where go 0 _ = zero
        go 1 x = x
        go n x = if r == 0 then twoy else x + twoy
          where (m,r) = n `Prelude.divMod` 2
                y = go m x
                twoy = y+y

(-) :: AddGroup a => a -> a -> a
x - y           =  x + negate y

class Additive a => AddGroup a where
  negate :: a -> a

mult :: AddGroup a => Integer -> a -> a
mult n x = if n < 0 then negate (times (negate n) x) else times n x

class Multiplicative a where
  one :: a
  (*) :: a -> a -> a

(^+) :: Multiplicative a => a -> Int -> a

x0 ^+ n0 = if n0 < 0  then Prelude.error "Algebra.Classes.^: negative exponent"
                      else go n0 x0
  where go 0 _ = one
        go 1 x = x
        go n x = if r == 0 then y2 else x * y2
          where  (m,r) = n `Prelude.divMod` 2
                 y = go m x
                 y2 = y * y

type Ring a = (AddGroup a, Multiplicative a)

fromInteger :: Ring a => Integer -> a
fromInteger n = mult n one

class Multiplicative a => MulGroup a where
  {-# MINIMAL (recip | (/)) #-}
  recip :: a -> a
  recip x         =  one / x

  (/) :: a -> a -> a
  x / y           =  x * recip y

(^) :: MulGroup a => a -> Int -> a
a ^ b | b < 0 = recip (a ^+ (negate b))
      | True = (a ^+ b)


type Field a = (Ring a, MulGroup a)

fromRational :: (Field a) => Data.Ratio.Ratio Integer -> a
fromRational x  =  fromInteger (Data.Ratio.numerator x) / fromInteger (Data.Ratio.denominator x)

class Field a => Algebraic a where
  sqrt :: a -> a

class Field a => Transcendental a where -- normally it should be "Algebraic" instead of "Field" but we're lazy like that. (Also Transcendental is a terrible name; taken from the "numeric prelude")
  pi :: a
  exp :: a -> a
  sin :: a -> a
  cos :: a -> a

---------------------------------
-- Instances

instance AddGroup Int where
  negate = Prelude.negate


instance Additive Int where
  (+) = (Prelude.+)
  zero = 0
instance Multiplicative Int where
  (*) = (Prelude.*)
  one = 1



instance Additive Integer where
  (+) = (Prelude.+)
  zero = 0
instance AddGroup Integer where
  negate = Prelude.negate

instance Multiplicative Integer where
  (*) = (Prelude.*)
  one = 1


instance Additive Rational where
  (+) = (Prelude.+)
  zero = 0
instance AddGroup Rational where
  negate = Prelude.negate
instance MulGroup Rational where
  (/) = (Prelude./)
  recip = Prelude.recip
instance Multiplicative Rational where
  (*) = (Prelude.*)
  one = 1

instance Additive a => Additive (x -> a) where
   f + g        =  \x -> f x + g x
   zero = const zero

instance Multiplicative a => Multiplicative (x -> a) where
   f * g        =  \x -> f x * g x
   one = const one

instance AddGroup a => AddGroup (x -> a) where
   negate f     =  negate . f

instance MulGroup a => MulGroup (x -> a) where
   recip f     =  recip . f

instance Algebraic a => Algebraic (x -> a) where
   sqrt f     =  sqrt . f

instance Transcendental a => Transcendental (x -> a) where
   pi = const pi
   sin f =  sin . f
   cos f =  cos . f
   exp f =  exp . f

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

instance Algebraic Double where
   sqrt = Prelude.sqrt

instance Transcendental Double where
   pi = Prelude.pi
   sin =  Prelude.sin
   cos =  Prelude.cos
   exp =  Prelude.exp


instance Additive a => Additive (Complex a) where
  (x :+ y) + (x' :+ y') = (x + x') :+ (y+y') 
  zero = zero :+ zero

instance AddGroup a => AddGroup (Complex a) where
  negate (a :+ b) = negate a :+ negate b

instance Ring a => Multiplicative (Complex a) where
  (a :+ b) * (a' :+ b') = (a * a' - b * b') :+ (a * b' + b * a')
  one = one :+ zero

instance Field a => MulGroup (Complex a) where
  recip (a :+ b) = (a / m)  :+ (negate b / m) where m = a*a + b*b


instance (Algebraic a, Prelude.RealFloat a) =>  Algebraic (Complex a) where
   sqrt = Prelude.sqrt

instance (Transcendental a, Prelude.RealFloat a) =>  Transcendental (Complex a) where
   pi = Prelude.pi
   sin =  Prelude.sin
   cos =  Prelude.cos
   exp =  Prelude.exp

---------------------------------
-- For RebindableSyntax

ifThenElse :: Bool -> p -> p -> p
ifThenElse c a b = if c then a else b

-------------------------------
-- Typesetting aliases.

neg :: AddGroup a => a -> a
neg = negate   -- |neg| is used to typeset unary minus as a shorter dash, closer to its argument

frac :: MulGroup a => a -> a -> a
frac = (/)     -- |frac| is used to typeset a fraction (more compactly than |x / y|)
