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
            | Sin
            | Cos
            | Exp
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
eval (Add e1 e2)  = eval e1   +   eval e2
eval (Mul e1 e2)  = eval e1   *   eval e2
\end{code}

----------------
2. Define a "syntactic derivative" function
    deriv :: FunExp -> FunExp

Specification: Forall e:FunExp.  D (eval e) = eval (deriv e)
   (where D is not implementable in Haskell)
\begin{code}
deriv :: FunExp -> FunExp
deriv (C c)      = C zero
deriv X          = C one
deriv (Add f g)  = Add (deriv f) (deriv g)
deriv (Mul f g)  = deriv f  * g    +   f * deriv g
deriv Sin = Cos
deriv Exp = Exp
deriv Cos = C (-1) * Sin

instance Additive FunExp       where zero = C zero; (+) = Add
instance Multiplicative FunExp where one  = C one ; (*) = Mul

\end{code}
--  Forall f.   D (eval f) = eval (deriv f)
--              D . eval = eval . f

-- (f*g)' = 
  Add (Mul (C 1.0) X) (Mul X (C 1.0))
  Add (Add (Mul (C 0.0) X)
           (Mul (C 1.0) (C 1.0)))
      (Add (Mul (C 1.0) (C 1.0))
           (Mul X (C 0.0)))



----------------
-- Rent polymorfa funktioner
\begin{code}
first :: (a,b) -> a
first (x,y) = x
\end{code}

3.1 What is a type class?

  "a collection of types with some common properties / methods"

----



3.2 Define type classes Additive, Multiplicative, and some instances
   (Double, FunExp, functions).

Additive + REAL instance
\begin{code}
class Additive a where   zero :: a;  (+) :: a -> a -> a
-- so far the collection is empty!
instance Additive Double where zero = 0.0; (+) = addReal
addReal :: Double -> Double -> Double
addReal = (Prelude.+)
-- now it has one single type as member

instance Additive a => Additive (k -> a) where zero = zeroFun; (+) = addFun
-- now we have many types as members

zeroFun :: Additive a => k -> a
zeroFun = const zero



addFun :: Additive a => (k->a) -> (k->a) -> (k->a)
addFun f g = \x -> f x  +  g x
\end{code}
Additive types include Double, Bool -> Double, String -> Double, Double -> Double,
  (Double->Double) -> Double,
   Int -> Bool -> Double,
   Char -> String -> Int -> Double


Multiplicative + REAL instance
\begin{code}
--    class Additive a where   zero :: a;  (+) :: a -> a -> a
class Multiplicative a where   one  :: a;  (*) :: a -> a -> a
instance Multiplicative Double where one = 1; (*) = (Prelude.*)
instance Multiplicative a => Multiplicative (k -> a) where one = oneF; (*) = mulF

oneF :: Multiplicative a =>     k -> a
oneF = const one

mulF :: Multiplicative a =>     (k -> a) -> (k->a) -> (k->a)
mulF f g = \x -> f x  *  g x

infixl 6 +
infixl 7 *

trigett :: Double -> Double
trigett = s*s + c*c
  where s = Prelude.sin
        c = Prelude.cos
\end{code}
























----------------------------------------------------------------

Some unicode examples in emacs:
* describe-char with cursor on a character gives information about it
* C-x 8 * x gives ×
* also C-x 8 RET MULTIPLICATION SIGN  -- or any other unicode code point name
* C-x 8 RET 3b4 gives δ
* C-x 8 RET 3b5 gives ε

----------------
