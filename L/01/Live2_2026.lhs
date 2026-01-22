This file is from the "live coding" session at the end of L1.2
(study week 1, lecture 2, of the DSLsofMath course, 2026).

It is all from Chapter 1 in the book
  Domain-Specific Languages of Mathematics
  https://www.collegepublications.co.uk/computing/?00024
More specifically it covers
  Section 1.4 (A DSL of complex numbers)
  Section 1.5 (A syntax for (complex) arithmetical expressions)
and some of
  Section 1.2 (Types in Haskell: |type|, |newtype|, and |data|)

{- iimage-mode for inlining the quotes -}

+ Pragmatics:
\begin{code}
{-# LANGUAGE GADTs #-}
module Live2_2026 where
type REAL = Double
\end{code}

A datatype for "semantic complex numbers" as defined on page A.2 in
the book [Adams and Essex, Calculus, 2010].

----------------
Quote 1:
file://Adams_Complex_01.png

data ImagUnit = ?
i :: ImagUnit
i = ?
\begin{code}
\end{code}

----------------
Quote 2:
file://Adams_Complex_02.png
data CA = TODO  -- first  form
        | TODO  -- second form

c1, c2 :: CA
\begin{code}
data CA -- TODO
c1 = error "3 + 4i"
c2 = error "3 + i4"
\end{code}

toC :: REAL -> C

\begin{code}
showCA :: CA -> String
showCA = error "showCA: TODO"

instance Show CA where  show = showCA
\end{code}

----------------
Quote 3:
file://Adams_Complex_03.png

Code up some example expressions
\begin{code}
\end{code}

----------------
Quote 4:
file://Adams_Complex_04.png

\begin{code}
data CC where
  PlusI :: REAL -> REAL -> CC   -- a real part and an imaginary part make up a complex number
equalCC :: CC -> CC -> Bool
equalCC = error "equalCC: TODO"

instance Eq CC where (==) = equalCC

\end{code}
----------------
Quote 5: 
file://Adams_Complex_05.png

----------------
Quote 6:
file://Adams_Complex_06.png
We define the functions |re| and |im| using pattern-matching.
\begin{code}
re, im :: CC -> REAL
re z = error "re: TODO"
im z = error "im: TODO"

\end{code}

An example of the use of pattern-matching:
----------------
Quote 7:
file://Adams_Complex_07.png
\begin{code}

add :: CC -> CC -> CC
add = error "add: TODO"

mul :: CC -> CC -> CC
mul = error "mul: TODO"
\end{code}

Derive the definition of mul using laws: distributivity and i² = -1.



\begin{code}
toCC :: REAL -> CC
toCC a = PlusI a 0


showCC :: CC -> String
showCC (PlusI x y) = show x ++ "+" ++ show y ++ "i"

instance Show CC where
  show = showCC
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
So far we have defined "semantic complex numbers" (in Cartesian form).
Now we turn to "syntactic complex number expressions" to define a DSL.
Three parts:
+ an abstract syntax type CE,
+ a  semantic type CC,
+ an evaluator eval :: CE -> CC

Part 1: an abstract syntax type of complex number expressions
\begin{code}
data CE where
  FromREAL :: REAL -> CE
  I        :: CE               -- I^2=-1
  Add, Mul :: CE -> CE -> CE
  deriving Show

\end{code}

"wishful thinking" -- call eval recursively on subtrees
\begin{code}
eval :: CE -> CC
eval I            = PlusI 0 1
eval (FromREAL a) = PlusI a 0
eval (Add e1 e2)  = add (eval e1) (eval e2)
eval (Mul e1 e2)  = mul (eval e1) (eval e2)
\end{code}
