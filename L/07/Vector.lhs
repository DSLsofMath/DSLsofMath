This code is not formally part of the DSLsofMath course.

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}

> module Vector where
> import GHC.Exts

> is a b = if a == b then 1 else 0

> type S            =  Double
> newtype Vector g  =  V (g -> S)
> toF (V v)         =  v

> class (Bounded a, Enum a, Eq a) => Finite a
> instance (Bounded a, Enum a, Eq a) => Finite a


> class Func f where
>   type Pred f :: * -> Constraint
>   func :: (Pred f a, Pred f b) => (a -> b) -> f a -> f b

> instance Func Vector where
>   type Pred Vector = Finite
>   func f (V v) = V (funcV f v)

> funcV :: (Finite g, Eq g', Num s) => (g -> g') -> (g -> s) -> (g' -> s)
> funcV f v g' = sum [v g | g <- [minBound .. maxBound], g' == f g]

> class Func f => Mon f where
>   eta   ::  Pred f a => a -> f a
>   bind  ::  (Pred f a, Pred f b) => f a -> (a -> f b) -> f b

> class Mon' f where
>   type Pred' f :: * -> Constraint
>   eta'   ::  Pred' f a => a -> f a
>   bind'  ::  (Pred' f a, Pred' f b) => f a -> (a -> f b) -> f b


> instance Mon Vector where
>   eta a         =  V (\ a' -> if a == a' then 1 else 0)
>   bind (V v) f  =  V (bindV v (toF . f))

> bindV :: (Finite g, Num s) => (g -> s) -> (g -> (g' -> s)) -> (g' -> s)
> bindV v f g' = sum [f g g' * v g | g <- [minBound .. maxBound]]

> instance Mon' Vector where
>   type Pred' Vector = Finite
>   eta' a         =  V (\ a' -> if a == a' then 1 else 0)
>   bind' (V v) f  =  V (\ g' -> sum [toF (f g) g' * v g | g <- [minBound .. maxBound]])

 instance Mon' f => Func f where
   type Pred f = Pred' f
   func = fmapMon'

 fmapMon' f m = m `bind'` (eta' . f)

---------------

> instance Num (Vector g) where
>   (+) = addV
>   fromInteger = fromIntegerV
>   -- ...

> addV :: Vector g -> Vector g -> Vector g
> addV (V v) (V w) = V (\g -> v g + w g)

> fromIntegerV :: Integer -> Vector g
> fromIntegerV s = V (const (fromInteger s))

> newtype Id a = Id {unId :: a}
>   deriving (Eq, Show)

> instance Functor Id where
>   fmap f (Id a) = Id (f a)
> class All a
> instance All a

> instance Func Id where
>   type Pred Id = All
>   func = fmap

instance Functor f => Func f where func = funcFromFunctor

> funcFromFunctor :: Functor f => (Pred f a, Pred f b) => (a -> b) -> f a -> f b
> funcFromFunctor = fmap

> testId = fmap (1+) (Id 3)

> type TwoDim   = Vector Bool
> type ThreeDim = Vector Ordering

> test2D1 :: TwoDim
> test2D1 = eta False

> test2D2 :: TwoDim
> test2D2 = eta True

> test3D1 :: ThreeDim
> test3D1 = eta LT + eta EQ

> test3D2 :: ThreeDim
> test3D2 = eta EQ + eta GT

> testM :: Bool -> ThreeDim
> testM False  =  test3D1
> testM True   =  test3D2

> testV1 :: ThreeDim
> testV1 = bind (test2D1 + test2D2) testM

----------------

> instance Finite g => Show (Vector g) where
>   show = showVector

> showVector :: Finite g => Vector g -> String
> showVector (V v) = show (map v [minBound .. maxBound])
