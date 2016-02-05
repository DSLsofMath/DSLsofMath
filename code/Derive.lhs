> module Derive where
> import FunNumInst
> import Expr
>
> derive  ::  Expression    ->  Expression
> derive      (Const alpha)  =  Const 0
> derive      Id             =  Const 1
> derive      (e1 :+: e2)    =  derive e1 :+: derive e2
> derive      (e1 :*: e2)    =  (derive e1 :*: e2) :+: (e1 :*: derive e2)
> derive      (Exp e)        =  Exp e :*: derive e

> expr' = derive expr

What should the result be?

> eval'         ::  Expression -> Double -> Double
> eval'          =  eval . derive


< t'    = eval expr 2 == ...
