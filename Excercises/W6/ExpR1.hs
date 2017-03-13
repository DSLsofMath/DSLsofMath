data ExpR = ExpR :+: ExpR
          | Negate ExpR
          | Zero
          | ExpR :*: ExpR
          | One
          | Var String

evalRI :: Num a => (String -> a) -> ExpR -> a
evalRI env = ev where
  ev (e1 :+: e2)  =  ev e1 + ev e2
  ev (Negate e)   =  negate (ev e)
  ev Zero         =  0
  ev (e1 :*: e2)  =  ev e1 * ev e2
  ev One          =  1
  ev (Var v)      =  env v

class Ring r where
  add   ::  r -> r -> r
  neg   ::  r -> r
  zero  ::  r
  mul   ::  r -> r -> r
  one   ::  r

foldRingExp :: Ring r => (String -> r) -> ExpR -> r
foldRingExp var = fold where
    fold (e1 :+: e2)  = add (fold e1) (fold e2)
    fold (Negate e)   = neg (fold e)
    fold  Zero        = zero
    fold (e1 :*: e2)  = mul (fold e1) (fold e2)
    fold  One         = one
    fold (Var v)      = var v
