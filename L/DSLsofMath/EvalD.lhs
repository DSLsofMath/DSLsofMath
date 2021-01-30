> {-# LANGUAGE FlexibleContexts #-}
> module DSLsofMath.EvalD (module DSLsofMath.FunExp, module DSLsofMath.EvalD) where
> import DSLsofMath.FunExp
> import DSLsofMath.Derive

> type FD a = (a -> a, a -> a)

Specification:

> evalD ::  FunExp  ->  FD Double
> evalD e = (eval e, eval' e)

Implementation

> evalD2 (Exp e)  =  let (f, f') = evalD2 e
>                    in  (exp . f, (exp . f) * f')

> evalD2 (e1 :*: e2)  =  let (f, f') = evalD2 e1
>                            (g, g') = evalD2 e2
>                        in  (f * g, f' * g + f * g')
