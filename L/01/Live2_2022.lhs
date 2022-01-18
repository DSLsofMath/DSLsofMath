This file is from the "live coding" session at the end of L1.2
(study week 1, lecture 2, of the DSLsofMath course, 2022).

{- iimage-mode for inlining the quotes -}

\begin{code}
{-# LANGUAGE GADTs #-}
module Live2 where
type REAL = Double
\end{code}

A datatype for "semantic complex numbers" as defined on page A.2 in
the book [Adams and Essex, Calculus, 2010].


Adams_Complex_01.png

\begin{code}
data ImagUnit = IU deriving Show
i :: ImagUnit
i = IU
\end{code}
Adams_Complex_02.png
\begin{code}
data CB where
  Plus1 :: REAL -> REAL -> ImagUnit -> CB
  Plus2 :: REAL -> ImagUnit -> REAL -> CB
 deriving (Show)

showCB :: CB -> String
showCB = error "showCB: TODO"
\end{code}
Adams_Complex_03.png
\begin{code}
e1, e2, e3, e4 :: CB
e1 = Plus1 3      2      IU
e2 = Plus1 (7/2)  (2/3)  IU
e3 = Plus2 0      IU     pi
e4 = Plus1 (-3)   0      IU
\end{code}
Adams_Complex_04.png
\begin{code}
data CC where
  PlusI :: REAL -> REAL -> CC
 deriving (Show)
-- PlusI a b == Plus1 a b IU == Plus2 a IU b
\end{code}
Adams_Complex_05.png

Adams_Complex_06.png
We define the functions |re| and |im| using pattern-matching.
\begin{code}
re, im :: CC -> REAL
re z@(PlusI x y) = x
im z@(PlusI x y) = y
\end{code}
An example of its use:
Adams_Complex_07.png
\begin{code}
addCC :: CC -> CC -> CC
addCC = error "TODO: define addCC"
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
So far we have defined "semantic complex numbers" (in cartesian form).
Now we turn to "syntactic complex number expressions" to define a DSL.
Three parts:
+ an abstract syntax type CE,
+ a  semantic type CC,
+ an evaluator eval :: CE -> CC

\begin{code}
data CE where
  Add  :: CE -> CE -> CE
  Mul  :: CE -> CE -> CE
  RCon :: REAL -> CE
  I    :: CE

-- "wishful thinking"
eval :: CE -> CC
eval (Add x y) = error "TODO eval Add"
eval (Mul x y) = error "TODO eval Mul"
eval I         = error "TODO eval I"
eval (RCon r)  = error "TODO eval RCon"
-- semantic "constructors"
-- addCC  :: CC -> CC -> CC  -- defined above
mulCC   :: CC -> CC -> CC
rconCC  :: REAL -> CC
iCC     :: CC
mulCC = undefined

rconCC r = PlusI r 0

iCC = PlusI 0 1 -- real part = 0, imaginary part = 1
\end{code}
exercise: implement mulCC

Add    :: CE -> CE -> CE   -- syntactic constructor
addCC  :: CC -> CC -> CC   -- semantic "smart constructor"

\begin{code}

\end{code}
