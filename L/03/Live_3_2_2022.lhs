Week & chapter 3: Types in mathematics
Lecture 3.2

\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Live_3_2 where
\end{code}

An example class |C|:

  "a collection of types with some common properties / methods"

First the collection is empty!
Each instance declaration adds a new type to the collection.

\begin{code}
class C a where         foo :: a -> a
instance C Int where    foo i = -i   -- now at least we have 'Elem Int C'
instance C Bool where   foo b = not b  -- now we have C ~= {Int, Bool}
instance C String where foo s = reverse s -- now three types are in C

testC1 = foo (3::Int)
testC2 = foo False
testC3 = foo "Patrik"
\end{code}



Added two types to the class Additive: Double, FunExp
\begin{code}
class Additive a where add :: a -> a -> a; zero :: a
  -- forall x, y. add x zero == x == add zero x
  -- (for what definition of (==)?)
instance Additive Double where add = (+); zero = 0
instance Additive FunExp where add = addFE; zero = zeroFE

addFE :: FunExp -> FunExp -> FunExp
addFE = Add
zeroFE :: FunExp
zeroFE = C 0
\end{code}

TODO: Add instances for Double and for functions
\begin{code}
\end{code}

Explain the set of instances of Additive
  = TODO

\begin{code}
type REAL = Double

-- Syntax datatype for 1-argument function expressions
-- FunExp is a syntax (DSL) for 1-argument function expressions
data FunExp = Sq
            | Tw
            | C REAL
            | Add FunExp FunExp
            | Mul FunExp FunExp
            | X
  deriving Show

type FunSem = REAL -> REAL
eval :: FunExp -> FunSem
eval (C c)        = const c     -- :: REAL -> REAL
eval X            = id
eval (Add e1 e2)  = oplus (eval e1) (eval e2)
eval (Mul e1 e2)  = omul  (eval e1) (eval e2)

oplus :: FunSem -> FunSem -> FunSem
oplus f g = \x -> f x + g x

omul :: FunSem -> FunSem -> FunSem
omul f g = \x -> f x * g x
\end{code}


































----------------------------------------------------------------

Some unicode examples in emacs:
* describe-char with cursor on a character gives information about it
* C-x 8 * x gives ×
* also C-x 8 RET MULTIPLICATION SIGN  -- or any other unicode code point name
* C-x 8 RET 3b4 gives δ
* C-x 8 RET 3b5 gives ε

----------------
