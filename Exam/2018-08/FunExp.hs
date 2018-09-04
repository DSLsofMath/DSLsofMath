module FunExp where
type REAL = Double
data FunExp  =  Const Double | Id | Exp FunExp
             |  FunExp  :+:  FunExp |  FunExp  :*:  FunExp
             deriving Show
type Func = REAL -> REAL

eval  ::  FunExp         ->  Func
eval      (Const alpha)  =   const alpha
eval      Id             =   id
eval      (e1 :+: e2)    =   eval e1  +  eval e2    -- note the use of ``lifted |+|'',
eval      (e1 :*: e2)    =   eval e1  *  eval e2    -- ``lifted |*|'',
eval      (Exp e1)       =   exp (eval e1)          -- and ``lifted |exp|''.

instance Num a => Num (x -> a) where
  f + g        =  \x -> f x + g x               -- lifted |+|
  f * g        =  \x -> f x * g x               -- lifted |*|
instance Fractional a => Fractional (x -> a)
instance Floating a => Floating (x -> a) where
  exp f    =  exp . f                           -- lifted |exp|
