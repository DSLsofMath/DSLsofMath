\begin{code}
module DSLsofMath.Derive where
import DSLsofMath.FunExp hiding (derive)

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
\end{code}
