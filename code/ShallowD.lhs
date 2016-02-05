> {-# LANGUAGE FlexibleInstances #-}
> module ShallowD where
> import FunNumInst

Note: completing the instance declarations left as exercise

> type FD a = (a -> a, a -> a)
> 
> instance Num a => Num (FD a) where
>   (f, f') + (g, g') = (f + g, f' + g')
>   (f, f') * (g, g') = (f * g, f' * g + f * g')
>   fromInteger n     = (fromInteger n, const 0)

> instance Fractional a => Fractional (FD a) where

> instance Floating a => Floating (FD a) where
>   exp (f, f')       =  (exp f, (exp f) * f')
>
>
> test x = sin (1 + exp x)
>
> type FD' a = (a, a) -- instead of (f, f') we have (f x, f' x)
>
> 


