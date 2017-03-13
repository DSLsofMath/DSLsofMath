> {-# LANGUAGE FlexibleInstances #-}
> module DSLsofMath.ShallowD where
> import DSLsofMath.FunNumInst

Note: completing the instance declarations left as exercise

> type FD a = (a -> a, a -> a)

> instance Num a => Num (FD a) where
>   (f, f') + (g, g') = (f + g, f' + g')
>   (f, f') * (g, g') = (f * g, f' * g + f * g')
>   fromInteger n     = (fromInteger n, const 0)

> instance Fractional a => Fractional (FD a) where

> instance Floating a => Floating (FD a) where
>   exp (f, f')       =  (exp f, (exp f) * f')
