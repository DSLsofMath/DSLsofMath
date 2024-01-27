\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Live_3_2 where
import Prelude hiding (  (+), (-), (*), (/), negate, recip, (^),
                         pi, sin, cos, exp, fromInteger, fromRational)
import qualified Prelude
type REAL = Double
\end{code}

Domain-Specific Languages of Mathematics course, Chalmers and UGOT
Week & chapter 3: Types in mathematics
Lecture 3.2

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
         -- | Sin FunExp | Exp FunExp | Div FunExp FunExp | ...
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

Specification: Forall e:FunExp.  D (eval e) = eval (deriv e)
   (where D is not implementable in Haskell)
\begin{code}
deriv :: FunExp -> FunExp
deriv = error "TODO"
\end{code}

----------------
3.1 What is a type class?

  "a collection of types with some common properties / methods"

----


3.2 Define type classes Additive, Multiplicative, and some instances
   (Double, FunExp, functions).

Additive + REAL instance
\begin{code}
\end{code}

Multiplicative + REAL instance
\begin{code}
\end{code}
infixl 6 +
infixl 7 *

FunExp instances for Additive and Multiplicative
\begin{code}
\end{code}

Final step: make instances for functions.
\begin{code}
\end{code}























----------------------------------------------------------------

Some unicode examples in emacs:
* describe-char with cursor on a character gives information about it
* C-x 8 * x gives ×
* also C-x 8 RET MULTIPLICATION SIGN  -- or any other unicode code point name
* C-x 8 RET 3b4 gives δ
* C-x 8 RET 3b5 gives ε

----------------
