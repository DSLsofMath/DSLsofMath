-- See also exam 2017-08-22 (Semiring)
--   ../../../Exam/2017-08/P1.lhs

\begin{code}
{-# LANGUAGE GADTs #-}
\end{code}


1.3a: A datatype |SR v| for semiring expressions

\begin{code}
data SR v where
  Add    ::  SR v -> SR v -> SR v
  Mul    ::  SR v -> SR v -> SR v
  Zero   ::  SR v
  One    ::  SR v
  Var    ::  v -> SR v
 deriving (Eq, Show)
\end{code}

1.3b: Example expressions from the laws:

\begin{code}
assocAdd a b c     =  [ Add (Add a b) c, Add a (Add b c) ]
unitAdd  a         =  [ Add Zero a, Add a Zero, a ]

assocMul a b c     =  [ Mul (Mul a b) c , Mul a (Mul b c) ]
unitMul  a         =  [ Mul One a, Mul a One, a ]

distMulAddL a b c  =  [ Mul a (Add b c) , Add (Mul a b) (Mul a c) ]
distMulAddR a b c  =  [ Mul (Add a b) c , Add (Mul a c) (Mul b c) ]
zeroMul  a         =  [ Mul Zero a , Mul a Zero, Zero ]
\end{code}

1.3c:

Warm-up: a specific evaluator for |Integer|.

\begin{code}
evalI :: (v -> Integer) -> SR v -> Integer
evalI f (Add  a b)  =  evalI f a  +  evalI f b
evalI f (Mul  a b)  =  evalI f a  *  evalI f b
evalI f (Zero)      =  0
evalI f (One)       =  1
evalI f (Var v)     =  f v
\end{code}

A general evaluator: here we take semiring operations as parameters.

\begin{code}
eval ::  (r -> r -> r) -> (r -> r -> r) -> r -> r ->
         (v -> r) ->
         SR v -> r
eval add mul zero one var = e where
  e (Add  a b)  =  add (e a) (e b)
  e (Mul  a b)  =  mul (e a) (e b)
  e (Zero)      =  zero
  e (One)       =  one
  e (Var v)     =  var v
\end{code}

Example: natural numbers modulo some |n > 0|:

\begin{code}
evalMod :: Int -> (v -> Int) -> SR v -> Int
evalMod n va = eval add mul zero one var
  where  add a b  =  mod (a+b) n
         mul a b  =  mod (a*b) n
         zero     =  0
         one      =  mod 1 n
         var v    =  mod (va v) n
\end{code}

In a later Chapter we will introduce type classes to collect all
semiring operations in one instance. For completeness we include that
also here to give the final type class generic evaluator:

\begin{code}
class SemiRing r where
  add    ::  r -> r -> r
  mul    ::  r -> r -> r
  zero   ::  r
  one    ::  r

evalSR :: SemiRing r => (v->r) -> SR v -> r
evalSR var = e where
  e (Add  a b)  =  add (e a) (e b)
  e (Mul  a b)  =  mul (e a) (e b)
  e (Zero)      =  zero
  e (One)       =  one
  e (Var v)     =  var v
\end{code}
