\begin{code}
module DSLsofMath.FunExp where
import DSLsofMath.Algebra
import Prelude (Eq, Ord, Show, Double, id, const, (.), toRational)

type REAL = Double

data FunExp  =  Const REAL
             |  X
             |  FunExp :+: FunExp
             |  FunExp :*: FunExp
             |  Recip FunExp
             |  Negate FunExp
             |  Exp FunExp
             |  Sin FunExp
             |  Cos FunExp
                -- and so on
  deriving (Eq, Ord, Show)
\end{code}

\begin{code}
eval :: Transcendental a => FunExp -> a -> a
eval (Const alpha)  =  const (fromRational (toRational alpha))
eval X              =  id
eval (e1 :+: e2)    =  eval e1 + eval e2
eval (e1 :*: e2)    =  eval e1 * eval e2
eval (Recip e)      =  recip (eval e)
eval (Negate e)     =  negate (eval e)
eval (Exp e)        =  exp (eval e)      -- = exp . (eval e) !
eval (Sin e)        =  sin (eval e)
eval (Cos e)        =  cos (eval e)

derive  ::  FunExp        ->  FunExp
derive      (Const _)      =  Const 0
derive      X              =  Const 1
derive      (e1 :+: e2)    =  derive e1 :+: derive e2
derive      (e1 :*: e2)    =  (derive e1 :*: e2) :+: (e1 :*: derive e2)
derive      (Recip e)      =  let re = Recip e in Negate (re:*:re) :*: derive e
derive      (Negate e)     =  Negate (derive e)
derive      (Exp e)        =  Exp e :*: derive e
derive      (Sin e)        =  Cos e :*: derive e
derive      (Cos e)        =  Negate (Sin e) :*: derive e

eval' :: Transcendental a => FunExp -> a -> a
eval' =  eval . derive
\end{code}
