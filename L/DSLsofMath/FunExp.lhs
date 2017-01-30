> module DSLsofMath.FunExp where
> import DSLsofMath.FunNumInst

> data FunExp      =  Const Double
>                  |  Id
>                  |  FunExp     :+: FunExp
>                  |  FunExp     :*: FunExp
>                  |  Exp FunExp
>                  -- and so on
>                  deriving Show

Example:

> expr1 :: FunExp
> expr1 = (Const 2) :*: (Exp (Exp Id))

What is the function corresponding to this expression?

> f x  = 2 * exp (exp x)

> eval  ::  FunExp          ->  Double -> Double
> eval      (Const alpha)    =  const alpha
> eval      Id               =  id
> eval      (e1 :+: e2)      =  eval e1 + eval e2
> eval      (e1 :*: e2)      =  eval e1 * eval e2
> eval      (Exp e1)         =  exp (eval e1)      -- = exp . (eval e1) !

Test:

> test1 :: Bool
> test1 = eval expr1 2 == f 2
