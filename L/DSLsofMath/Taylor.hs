{-# LANGUAGE FlexibleContexts #-}

module Taylor where

import Data.Complex

-- also this is a vector space
data Taylor a = Integ a (Taylor a)

instance Show a => Show (Taylor a) where 
  show = show . take 10 . toList

toList (Integ x xs) = x:toList xs

zero = Integ 0 zero

instance Num a => Num (Taylor a) where
  (a `Integ` as) + (b `Integ` bs)  =  (a + b)  `Integ`  (as + bs)
  (a `Integ` as) * (b `Integ` bs) = (a*b) `Integ`  (as * (b `Integ` bs) + (a `Integ` as) * bs)
  fromInteger x = integ (fromInteger x) 0
  negate (a `Integ` as) = negate a `Integ` negate as

constant :: Num a => a -> Taylor a
constant x = integ x 0

integ :: a -> Taylor a -> Taylor a
integ = Integ

sinx :: Taylor Integer
sinx = integ 0 cosx

cosx :: Taylor Integer
cosx = integ 1 (-sinx)

expx :: Taylor (Complex Double)
expx = integ 1 expx

-- expkx :: Taylor Double -> Taylor Double
-- expkx k = integ 1 (k*expx)

varx :: Taylor (Complex Double)
varx = integ 0 1

(*^) :: Num t => t -> Taylor t -> Taylor t
a *^ (Integ k ks) = Integ (a * k) (a *^ ks)

evalPart :: Fractional p => [p] -> p -> p -> p
evalPart [] _ _  = 0
evalPart (k:ks) n y = k + evalPart (((y/n) *) <$> ks) (n+1) y

eval :: Fractional p => Taylor p -> Int -> p -> p
eval t n x = evalPart (take n $ toList t) 1 x


-- f . g
-- integ ((f . g) 0) (D (f . g))
-- integ (f (g 0)) (D f . g * g')

compose :: (Eq a, Num a) => Taylor a -> Taylor a -> Taylor a
compose (Integ f0 f') g@(Integ g0 g') = Integ fg0 ((f' `compose` g) * g')
  where
    -- fg0 is the evaluation of f at point g0. (An infinite sum in the general case, but...)
    fg0 = if g0 == 0 then f0 else error "fg0: computation diverges"

-- >>> varx
-- [0,1,0,0,0,0,0,0,0,0]

-- >>> 3 * varx
-- [0.0,3.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]

-- >>> expx `compose` ((constant (0 :+ 1) * varx))
-- [1.0 :+ 0.0,
--  0.0 :+ 1.0,
--  (-1.0) :+ 0.0,
--  0.0 :+ (-1.0),
--  1.0 :+ 0.0,
--  0.0 :+ 1.0,
--  (-1.0) :+ 0.0,
--  0.0 :+ (-1.0),
--  1.0 :+ 0.0,
--  0.0 :+ 1.0]

