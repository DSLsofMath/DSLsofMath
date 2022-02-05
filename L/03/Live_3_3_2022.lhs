\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Live_3_3 where
import Prelude hiding (  (+), (-), (*), (/), negate, recip, (^),
                         pi, sin, cos, exp, fromInteger, fromRational)
import qualified Prelude
type REAL = Double
\end{code}

Domain-Specific Languages of Mathematics course
Week & chapter 3: Types in mathematics
Lecture 3.3 (cont. from L3.1 and L3.2)

1. Reminder about the DSL definition.
2. Define a "syntactic derivative" function
    deriv :: FunExp -> FunExp
3. Define type classes Additive, Multiplicative, and some instances
   (REAL, FunExp, functions).  

----------------
1. Reminder about the DSL definition
  Each DSL needs
  + type of syntax trees:     Syn
  + type of semantic values:         Sem
  + a function        eval :: Syn -> Sem

FunExp is a syntax (DSL) for "1-argument function expressions".
\begin{code}
data FunExp = C REAL
            | X
            | Add FunExp FunExp
            | Mul FunExp FunExp
  deriving Show

e1, e2, e3, e4 :: FunExp
e1 = Add X X       -- 
e2 = Mul X X
e3 = Add e1 e2     -- \x ->  (x+x) + x^2
e4 = Mul e3 e3

type FunSem = REAL -> REAL    -- type REAL = Double
eval :: FunExp -> FunSem
eval (C c)        = const c     -- :: REAL -> REAL
eval X            = id
eval (Add e1 e2)  = addFun (eval e1) (eval e2)
eval (Mul e1 e2)  = mulFun (eval e1) (eval e2)

addFun :: FunSem -> FunSem -> FunSem
addFun f g = \x -> f x  Prelude.+  g x

mulFun :: FunSem -> FunSem -> FunSem
mulFun f g = \x -> f x  Prelude.*  g x
\end{code}

----------------
2. Define a "syntactic derivative" function
    deriv :: FunExp -> FunExp

Specification:   D (eval e) = eval (deriv e)
   (where D is not implementable in Haskell)
\begin{code}
deriv :: FunExp -> FunExp
deriv (C c)        = C 0
deriv X            = C 1
deriv (Add e1 e2)  = addD (deriv e1) (deriv e2)
deriv (Mul e1 e2)  = mulD e1 e2 (deriv e1) (deriv e2)

addD :: FunExp -> FunExp -> FunExp
addD = Add

mulD :: FunExp -> FunExp -> FunExp -> FunExp -> FunExp
-- mulD e1 e2 e1' e2' = Add (Mul e1' e2) (Mul e1 e2')
mulD e1 e2 e1' e2' = (e1'*e2) + (e1*e2')
\end{code}

----------------
3. Define type classes Additive, Multiplicative, and some instances
   (Double, FunExp, functions).  

Additive + REAL instance 
\begin{code}
class Additive a where (+) :: a -> a -> a; zero :: a
-- now we have an empty set of instances of the class Additive
instance Additive REAL where (+) = (Prelude.+); zero = 0
-- now we have one instance: REAL
\end{code}

Multiplicative + REAL instance 
\begin{code}
class Multiplicative a where (*) :: a -> a -> a; one :: a
instance Multiplicative REAL where (*) = (Prelude.*); one = 1

two :: (Additive a, Multiplicative a) => a
two = one + one
\end{code}

FunExp instances for Additive and Multiplicative
\begin{code}
instance Additive       FunExp where (+) = Add; zero = C 0
instance Multiplicative FunExp where (*) = Mul; one  = C 1
\end{code}
Now both Additive and Multiplicative have two instances: REAL and FunExp.

Make an instance for functions.
\begin{code}
instance Additive b =>       Additive       (a->b) where (+) = addF; zero = zeroF
instance Multiplicative b => Multiplicative (a->b) where (*) = mulF; one = oneF

zeroF :: Additive b => a -> b
zeroF = const zero 
  
oneF :: Multiplicative b => a -> b
oneF = const one

addF :: Additive b => (a -> b) -> (a -> b) -> (a -> b)
addF f g = \x -> f x  +  g x

mulF :: Multiplicative b => (a -> b) -> (a -> b) -> (a -> b)
mulF f g = \x -> f x  *  g x

f1, f2, f3, f4 :: REAL -> REAL
[f1, f2, f3, f4] = map eval [e1, e2, e3, e4]

s1 "x" = 2
s1 "y" = 3
s1 _   = 0
s2 "x" = 1
s2 "y" = 2
s2 _   = 1
\end{code}
Which types are now in the set of instances for Additive and Multiplicative?

  { REAL, FunExp,
    REAL -> REAL, FunExp -> FunExp,
    String -> REAL, String -> FunExp,
    REAL -> REAL -> REAL, String -> REAL -> FunExp    
  ...
  }

Note that while there are many types in there, all the operations
still have "homogenous types": a -> a -> a for some a in the class.
An "inhomogenous type" for (+) would be something like
  (+) :: REAL -> Integer -> Double
or
  (*) :: (Bool -> REAL) -> (Bool -> String -> REAL) -> (Bool -> REAL)
but that is not supported by these classes.







































----------------------------------------------------------------

Some unicode examples in emacs:
* describe-char with cursor on a character gives information about it
* C-x 8 * x gives ×
* also C-x 8 RET MULTIPLICATION SIGN  -- or any other unicode code point name
* C-x 8 RET 3b4 gives δ
* C-x 8 RET 3b5 gives ε

----------------
