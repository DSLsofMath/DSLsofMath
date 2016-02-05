> module EvalD where
> import Expr
>
> evalD :: Expression -> (Double -> Double, Double -> Double)

> evalD (Exp e)  =  let (f, f') = evalD e
>                   in  (exp . f, (exp . f) * f')

> evalD (e1 :*: e2)  =  let (f, f') = evalD e1
>                           (g, g') = evalD e2
>                       in  (f * g, f' * g + f * g')

