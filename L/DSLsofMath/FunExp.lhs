\begin{code}
module DSLsofMath.FunExp where
import DSLsofMath.FunNumInst

type REAL = Double

data FunExp  =  Const REAL         -- |Rational| does not work with |Floating|
             |  Id
             |  FunExp :+: FunExp
             |  FunExp :*: FunExp
             |  FunExp :/: FunExp
             |  Negate FunExp
             |  Exp FunExp
             |  Sin FunExp
             |  Cos FunExp
                -- and so on
  deriving (Eq, Show)
\end{code}

Example:

\begin{code}
expr1 :: FunExp
expr1 = (Const 2) :*: (Exp (Exp Id))
\end{code}

What is the function corresponding to this expression?

\begin{code}
f x  = 2 * exp (exp x)

type Func = REAL -> REAL
eval  ::  FunExp          ->  Func
eval      (Const alpha)    =  const alpha
eval      Id               =  id
eval      (e1 :+: e2)      =  eval e1 + eval e2
eval      (e1 :*: e2)      =  eval e1 * eval e2
eval      (Negate e)       =  negate (eval e)
eval      (Exp e)          =  exp (eval e)      -- = exp . (eval e) !
eval      (Sin e)          =  sin (eval e)
eval      (Cos e)          =  cos (eval e)
\end{code}

Test:

\begin{code}
test1 :: Bool
test1 = eval expr1 2 == f 2
\end{code}
