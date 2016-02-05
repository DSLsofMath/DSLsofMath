> module Expr where
> import FunNumInst

> data Expression  =  Const Double
>                  |  Id
>                  |  Expression :+: Expression
>                  |  Expression :*: Expression
>                  |  Exp Expression
>                  -- and so on
>                  deriving Show

Example:

> expr = (Const 2) :*: (Exp (Exp Id))

What is the function corresponding to this expression?

> f x  = 2 * exp (exp x)

> eval  ::  Expression      ->  Double -> Double
> eval      (Const alpha)    =  const alpha
> eval      Id               =  id
> eval      (e1 :+: e2)      =  eval e1 + eval e2
> eval      (e1 :*: e2)      =  eval e1 * eval e2 
> eval      (Exp e1)         =  exp (eval e1) -- exp . (eval e1) !

Test:

> t = eval expr 2 == f 2
