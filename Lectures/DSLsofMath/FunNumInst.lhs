> module DSLsofMath.FunNumInst where

> instance Num a => Num (x -> a) where
>   f + g        =  \x -> f x + g x
>   f - g        =  \x -> f x - g x
>   f * g        =  \x -> f x * g x
>   negate f     =  negate . f
>   abs f        =  abs . f
>   signum f     =  signum . f
>   fromInteger  =  const . fromInteger

> instance Fractional a => Fractional (x -> a) where
>   recip  f         =  recip . f
>   fromRational     =  const . fromRational

> instance Floating a => Floating (x -> a) where
>   pi       =  const pi
>   exp f    =  exp . f
>   f ** g   =  \ x -> (f x)**(g x)
>   -- and so on
