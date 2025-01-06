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

-- smart constructor
pow :: Int -> FunExp
pow n | n < 0 = error "pow: neg. not supported"
pow 0 = C 1
pow n = Mul X (pow (n Prelude.- 1))

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

-- addFun :: FunSem -> FunSem -> FunSem
addFun f g = \x -> f x  +  g x

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
deriv (C c) = C 0
deriv X     = C 1   ---
deriv (Add e1 e2) = deriv e1  +  deriv e2
deriv (Mul e1 e2) = deriv e1 * e2   +   e1 * deriv e2
\end{code}
  -- (f*g)' = f'g + fg' = f'*g + f*g' = ...
-- Equational reasoning

  eval (deriv X)
= -- Def. av deriv
  eval (C 1)
= -- Def. av eval
  const 1
= -- Enligt def. av D
  D (\x->x)
= -- Def. av id
  D id
= -- Def av eval
  D (eval X)



=

----------------
3.1 What is a type class?

  "a collection of types with some common properties / methods"

----


3.2 Define type classes Additive, Multiplicative, and some instances
   (Double, FunExp, functions).

Additive + REAL instance

\begin{code}
class    Additive a    where zero :: a; (+) :: a -> a -> a
instance Additive REAL where zero = 0;  (+) = (Prelude.+)   -- nu finns REAL i "mängden" Additive
\end{code}

Multiplicative + REAL instance
\begin{code}
class    Multiplicative a  where one :: a; (*) :: a -> a -> a
instance Multiplicative REAL where one = 1; (*) = (Prelude.*)

infixl 6 +
infixl 7 *

\end{code}

FunExp instances for Additive and Multiplicative
\begin{code}
instance Additive FunExp where zero = zeroFE; (+) = addFE

zeroFE :: FunExp
zeroFE = C 0

addFE :: FunExp -> FunExp -> FunExp
addFE = Add

instance Multiplicative FunExp where one = C 1; (*) = Mul
\end{code}

Final step: make instances for functions.
\begin{code}
instance Additive b => Additive (a->b) where zero = zeroF; (+) = addF

zeroF :: Additive b => a -> b
zeroF = const zero

addF :: Additive b => (a->b)->(a->b)->(a->b)
addF = addFun


instance Multiplicative b => Multiplicative (a->b) where one = oneF; (*) = mulF

oneF :: Multiplicative b => a -> b
oneF = const one

mulF :: Multiplicative b => (a->b)->(a->b)->(a->b)
mulF = lift2 (*)

lift2 :: (a->b->c) -> (t->a) -> (t->b) -> (t->c)
lift2 op f g = \x -> op (f x) (g x)
\end{code}























----------------------------------------------------------------

Some unicode examples in emacs:
* describe-char with cursor on a character gives information about it
* C-x 8 * x gives ×
* also C-x 8 RET MULTIPLICATION SIGN  -- or any other unicode code point name
* C-x 8 RET 3b4 gives δ
* C-x 8 RET 3b5 gives ε

----------------
