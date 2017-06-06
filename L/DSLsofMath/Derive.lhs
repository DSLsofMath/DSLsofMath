> module DSLsofMath.Derive where
> import DSLsofMath.FunNumInst
> import DSLsofMath.FunExp

> derive  ::  FunExp        ->  FunExp
> derive      (Const alpha)  =  Const 0
> derive      Id             =  Const 1
> derive      (e1 :+: e2)    =  derive e1 :+: derive e2
> derive      (e1 :*: e2)    =  (derive e1 :*: e2) :+: (e1 :*: derive e2)
> derive      (Exp e)        =  Exp e :*: derive e

> expr1' :: FunExp
> expr1' = derive expr1

What should the result be?

> eval' :: FunExp -> Double -> Double
> eval' =  eval . derive


< t'    = eval expr 2 == ...
