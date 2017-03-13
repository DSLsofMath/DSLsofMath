This is an example of an equality proof from week 3 "embedded in Haskell".

\begin{code}
module DSLsofMath.PoorMansProofs where
import DSLsofMath.EvalD
import DSLsofMath.Derive

type EqChain a = [a]

eqChain :: FunExp -> EqChain (FD Double)
eqChain e =
  [
     evalD (Exp e)

  ,  {- specification of |evalD| -}

     (eval (Exp e), eval' (Exp e))

  ,  {- def. |eval'|, function composition -}

     (eval (Exp e), eval (derive (Exp e)))

  ,  {- def. |derive| for |Exp| -}

     (eval (Exp e), eval (Exp e :*: derive e))

  ,  {- def. |eval| for |:*:| -}

     (eval (Exp e), eval (Exp e) * eval (derive e))

  ,  {- def. |eval| for |Exp| -}

     (exp (eval e), exp (eval e) * eval (derive e))

  ,  {- def. |eval'| -}

     (exp (eval e), exp (eval e) * eval' e)

  ,  {- introduce names for subexpressions -}

     let  f   = eval e
          f'  = eval' e
     in (exp f, exp f * f')

  ,  {- def. |evalD| -}

     let (f, f') = evalD e
     in (exp f, exp f * f')
  ]

applyFD ::  a ->  FD a     ->  (a, a)
applyFD     c     (f, f')  =   (f c, f' c)

test :: Double -> FunExp -> [(Double, Double)]
test c e = map (applyFD c) (eqChain e)

check c e = allEq (test c e)

allEq :: Eq a => EqChain a -> Bool
allEq []      = True
allEq (e:es)  = all (e==) es

main = check 0 (Const 1) && check 1 (Id :+: Exp Id)
\end{code}
