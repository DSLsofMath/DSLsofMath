> module DSLsofMath.Simplify where
> import DSLsofMath.FunExp
> import DSLsofMath.Derive

> test1' = simplify expr1'

There are many possible implementations of |simplify| and it is a good
exercise to try to implement your own version. Spoiler below.



































This is one possibility. Note the structure with "semantic functions"
to help keeping track of what is "already simplified". (Helps to avoid
infinite recursion.)

> type SimpleExp = FunExp

> simplify  ::  FunExp -> SimpleExp
> simplify (x :+: y) = simpAdd (simplify x) (simplify y)
> simplify (x :*: y) = simpMul (simplify x) (simplify y)
> simplify (Exp x)   = simpExp (simplify x)
> simplify (Sin x)   = simpSin (simplify x)
> simplify (Cos x)   = simpCos (simplify x)
> simplify Id        = Id
> simplify (Const c) = Const c

> simpAdd :: SimpleExp -> SimpleExp -> SimpleExp
> simpAdd (Const a)  (Const b)  = Const (a+b)
> simpAdd (Const 0)  x          = x
> simpAdd x          (Const 0)  = x
> simpAdd (x:+:y)    z          = simpAdd x (y:+:z)
> simpAdd (Const a)  (Const b:+:x) = simpAdd (Const (a+b)) x
> simpAdd x          y  = case scaledEq x y of
>   Left (a,b,x) -> simpMul (Const (a+b)) x
>   Right (x,y)  -> x :+: y

More work is needed to handle constants well.

> simpMul :: SimpleExp -> SimpleExp -> SimpleExp
> simpMul (Const a)  (Const b)  = Const (a*b)
> simpMul (Const 0)  x          = Const 0
> simpMul x          (Const 0)  = Const 0
> simpMul (Const 1)  x          = x
> simpMul x          (Const 1)  = x
> simpMul (x:*:y)    z          = simpMul x (y:*:z)
> simpMul x          (Const c)  = simpMul (Const c) x
> simpMul (Const a)  (Const b :*: x) = simpMul (Const (a*b)) x
> simpMul x          (Const c :*: y) = simpMul (Const c) (x :*: y)
> simpMul x          y          = x :*: y

> simpExp :: SimpleExp -> SimpleExp
> simpExp (Const c)  = Const (exp c)
> simpExp x          = Exp x

> simpSin :: SimpleExp -> SimpleExp
> simpSin (Const c)  = Const (sin c)
> simpSin x          = Exp x

> simpCos :: SimpleExp -> SimpleExp
> simpCos (Const c)  = Const (cos c)
> simpCos x          = Exp x

> scaledEq x y                         | x==y = Left (1,1,x)
> scaledEq            x  (Const b:*:y) | x==y = Left (1,b,x)
> scaledEq (Const a:*:x)            y  | x==y = Left (a,1,x)
> scaledEq (Const a:*:x) (Const b:*:y) | x==y = Left (a,b,x)
> scaledEq x y = Right (x,y)

Some test cases:

> -- x3 = Id:*:(Id:*:Id)
> x3 = (Id:*:Id):*:Id
> x3' = derive x3
> x3'' = derive x3'
> x3''' = derive x3''

> t1 = simplify x3'
> t2 = simplify x3''
> t3 = simplify x3'''
