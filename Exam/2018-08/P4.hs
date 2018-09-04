import FunExp
-- 4a
d :: Func -> Func
d = undefined
eqChain4a e =
  [
    eval (derive (Exp e))
  , -- =  spec.
    d (eval (Exp e))
  , -- =  def. eval
    d (exp (eval e))
  , -- =  def. lifted exp
    d (exp (eval e))
  , -- =  def. lifted exp
    d (exp . eval e)
  , -- =  d chain rule
    (d exp . eval e) * d (eval e)
  , -- =  d for exp, specification
    (exp . eval e) * eval (derive e)
  , -- =  lifted exp
    exp (eval e) * eval (derive e)
  , -- =  eval for Exp
    eval (Exp e) * eval (derive e)
  , -- =  eval for :*:
    eval (Exp e :*: derive e)
  ]

-- 4b
eqChain4b e1 e2 =
  [
    eval (derive (e1 :*: e2))
  , -- = spec.
    d (eval (e1 :*: e2))
  , -- = def. eval for :*:
    d (eval e1 * eval e2)
  , -- = derivative of product
    eval e1 * d (eval e2) + d (eval e1) * eval e2
  , -- = spec. bacwards
    eval e1 * eval (derive e2) + eval (derive e1) * eval e2
  , -- = eval for :*:, twice
    eval (e1 :*: derive e2) + eval (derive e1 :*: e2)
  , -- = eval for :+:
    eval ((e1 :*: derive e2) :+: (derive e1 :*: e2))
  ]

-- 4c
derive  ::  FunExp        ->  FunExp
derive      (Const alpha)  =  Const 0
derive      Id             =  Const 1
derive      (e1 :+: e2)    =  derive e1 :+: derive e2
derive      (e1 :*: e2)    =  (derive e1 :*: e2) :+: (e1 :*: derive e2)
derive      (Exp e)        =  Exp e :*: derive e
