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
>   f ** g   =  \ x -> (f x)**(g x)
>   exp f    =  exp . f
>   log f    =  log . f

>   sin f    =  sin . f
>   cos f    =  cos . f
>   tan f    =  tan . f
>   sinh f   =  sinh . f
>   cosh f   =  cosh . f
>   tanh f   =  tanh . f

>   asin f   =  asin . f
>   acos f   =  acos . f
>   atan f   =  atan . f
>   asinh f  =  asinh . f
>   acosh f  =  acosh . f
>   atanh f  =  atanh . f
