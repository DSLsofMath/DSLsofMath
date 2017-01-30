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
> simplify Id        = Id
> simplify (Const c) = Const c

> simpAdd :: SimpleExp -> SimpleExp -> SimpleExp
> simpAdd (Const a)  (Const b)  = Const (a+b)
> simpAdd (Const 0)  x          = x
> simpAdd x          (Const 0)  = x
> simpAdd x          y          = x :+: y

> simpMul :: SimpleExp -> SimpleExp -> SimpleExp
> simpMul (Const a)  (Const b)  = Const (a*b)
> simpMul (Const 0)  x          = Const 0
> simpMul x          (Const 0)  = Const 0
> simpMul (Const 1)  x          = x
> simpMul x          (Const 1)  = x
> simpMul x          y          = x :*: y

> simpExp :: SimpleExp -> SimpleExp
> simpExp (Const c)  = Const (exp c)
> simpExp x          = Exp x
