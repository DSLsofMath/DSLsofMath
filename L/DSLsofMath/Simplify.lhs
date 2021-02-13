\begin{code}
module DSLsofMath.Simplify where
import DSLsofMath.FunExp
\end{code}

Example:

\begin{code}
expr1 :: FunExp
expr1 = (Const 2) :*: (Exp (Exp X))
expr1' :: FunExp
expr1' = derive expr1
\end{code}

Test:

\begin{code}
test1' = simplify expr1'
\end{code}

There are many possible implementations of |simplify| and it is a good
exercise to try to implement your own version. Spoiler below.



































This is one possibility. Note the structure with "semantic functions"
to help keeping track of what is "already simplified". (Helps to avoid
infinite recursion.)

\begin{code}
type SimpleExp = FunExp

simplify  ::  FunExp -> SimpleExp
simplify (x :+: y)   = simpAdd (simplify x) (simplify y)
simplify (x :*: y)   = simpMul (simplify x) (simplify y)
simplify (Exp x)     = simpExp (simplify x)
simplify (Sin x)     = simpSin (simplify x)
simplify (Cos x)     = simpCos (simplify x)
simplify (Negate x)  = simpNeg (simplify x)
simplify X           = X
simplify (Const c)   = Const c

simpAdd :: SimpleExp -> SimpleExp -> SimpleExp
simpAdd (Const a)  (Const b)  = Const (a+b)
simpAdd (Const 0)  x          = x
simpAdd x          (Const 0)  = x
simpAdd (x:+:y)    z          = simpAdd x (y:+:z)
simpAdd (Const a)  (Const b:+:x) = simpAdd (Const (a+b)) x
simpAdd x          y  = case scaledEq x y of
  Left (a,b,x) -> simpMul (Const (a+b)) x
  Right (x,y)  -> x :+: y
\end{code}

More work is needed to handle constants well.

\begin{code}
simpMul :: SimpleExp -> SimpleExp -> SimpleExp
simpMul (Const a)  (Const b)  = Const (a*b)
simpMul (Const 0)  _x         = Const 0
simpMul _x         (Const 0)  = Const 0
simpMul (Const 1)  x          = x
simpMul x          (Const 1)  = x
simpMul (x:*:y)    z          = simpMul x (y:*:z)
simpMul x          (Const c)  = simpMul (Const c) x
simpMul (Const a)  (Const b :*: x) = simpMul (Const (a*b)) x
simpMul x          (Const c :*: y) = simpMul (Const c) (x :*: y)
simpMul x          y          = x :*: y

simpNeg :: SimpleExp -> SimpleExp
simpNeg (Const c)   = Const (negate c)
simpNeg (Negate x)  = x
simpNeg x           = Negate x

simpExp :: SimpleExp -> SimpleExp
simpExp (Const c)  = Const (exp c)
simpExp x          = Exp x

simpSin :: SimpleExp -> SimpleExp
simpSin (Const c)  = Const (sin c)
simpSin x          = Sin x

simpCos :: SimpleExp -> SimpleExp
simpCos (Const c)  = Const (cos c)
simpCos x          = Cos x

scaledEq x y                         | x==y = Left (1,1,x)
scaledEq            x  (Const b:*:y) | x==y = Left (1,b,x)
scaledEq (Const a:*:x)            y  | x==y = Left (a,1,x)
scaledEq (Const a:*:x) (Const b:*:y) | x==y = Left (a,b,x)
scaledEq x y = Right (x,y)
\end{code}

Some test cases:

\begin{code}
-- x3 = X:*:(X:*:X)
x3 = (X:*:X):*:X
x3' = derive x3
x3'' = derive x3'
x3''' = derive x3''

t1 = simplify x3'
t2 = simplify x3''
t3 = simplify x3'''
\end{code}

\begin{code}
size :: FunExp -> Int
size (f:+:g) = 1 + size f + size g
size (f:*:g) = 1 + size f + size g
size (Const _) = 1 -- or 2
size X         = 1
size (Negate f)= 1 + size f
size (Sin f)   = 1 + size f
size (Cos f)   = 1 + size f
size (Exp f)   = 1 + size f
\end{code}
