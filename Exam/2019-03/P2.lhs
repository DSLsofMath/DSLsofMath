P2: Calculational proof of syntactic differentiation.

\begin{code}
{-# LANGUAGE GADTs #-}
eval  ::  FunExp           ->  Func
eval      (e1  :+:  e2)    =   eval e1  +  eval e2    -- note the use of ``lifted |+|'',
eval      (e1  :*:  e2)    =   eval e1  *  eval e2    -- note the use of ``lifted |*|'',
eval      (e1  :.:  e2)    =   eval e1  .  eval e2
eval      Exp              =   exp

-- two extra cases not asked for in the exam:
eval Id        = id
eval (Const c) = const c
\end{code}

Spec: Forall e : FunExp.  eval (derive e) == D (eval e)

**2a**

  eval (derive (f:*:g))
= -- Spec.
  D (eval (f:*:g))
= -- def. of eval for (:*:)
  D (eval f  *  eval g)
= -- product law for D
  D (eval f) * eval g  +  eval f * D (eval g)
= -- Spec. twice "backwards"
  eval (derive f) * eval g  +  eval f * eval (derive g)
= -- eval of :*: "backwards"
  eval (derive f :*: g)  +  eval (f :*: derive g)
= -- eval of :+: "backwards"
  eval ((derive f :*: g) :+: (f :*: derive g))

Thus we can satisfy this by defining:
\begin{code}
derive (f:*:g) = (derive f :*: g) :+: (f :*: derive g)
\end{code}

  eval (derive (f:.:g))
= -- Spec.
  D (eval (f:.:g))
= -- def. of eval for (:.:)
  D (eval f . eval g)
= -- Chain rule for D
  D (eval f . eval g)
= -- Chain rule for D
  (D (eval f) . eval g) * D (eval g)
= -- Spec. "backwards" twice
  (eval (derive f) . eval g) * eval (derive g)
= -- Def. of eval for (:.:) "backwards"
  eval (derive f :.: g) * eval (derive g)
= -- Def. of eval for (:*:) "backwards"
  eval ((derive f :.: g) :*: derive g)

Thus we can satisfy this by defining
\begin{code}
derive (f:.:g) = (derive f :.: g) :*: derive g
\end{code}

We fill in the remaining cases without calculation:
\begin{code}
derive (f:+:g) = derive f :+: derive g
derive Exp = Exp
derive Id  = Const 1
derive (Const c) = Const 0
\end{code}


-- ----------------------------------------------------------------
Helper code from the exam question:

\begin{code}
type REAL = Double
data FunExp where
    (:+:)  ::  FunExp -> FunExp -> FunExp
    (:*:)  ::  FunExp -> FunExp -> FunExp
    (:.:)  ::  FunExp -> FunExp -> FunExp
    Exp    ::  FunExp
    Id     ::  FunExp
    Const  ::  REAL -> FunExp
  deriving Show


type Func = REAL -> REAL
instance Num a => Num (x -> a) where (+) = addF; (*) = mulF; -- ...

addF, mulF :: Num a => (x->a) -> (x->a) -> (x->a)
addF = liftOp (+)   -- a point-free definition of ``lifted |+|''
mulF = liftOp (*)   -- ... and lifted |*|

liftOp :: (a -> b -> c) -> (x -> a) -> (x -> b) -> (x -> c)
liftOp op f g = \x -> op (f x) (g x)

instance Floating a   => Floating (x -> a)
instance Fractional a => Fractional (x -> a)
\end{code}
