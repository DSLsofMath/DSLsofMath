This file is from the "live coding" session at the end of L1.2
(study week 1, lecture 2, of the DSLsofMath course, 2023).

It is all from Chapter 1 in the book
  Domain-Specific Languages of Mathematics
  https://www.collegepublications.co.uk/computing/?00024
More specifically it covers
  Section 1.4 (A DSL of complex numbers)
  Section 1.5 (A syntax for (complex) arithmetical expressions)
and some of
  Section 1.2 (Types in Haskell: |type|, |newtype|, and |data|)

{- iimage-mode for inlining the quotes -}

\begin{code}
{-# LANGUAGE GADTs #-}
module Live2_2023 where
type REAL = Double
\end{code}

A datatype for "semantic complex numbers" as defined on page A.2 in
the book [Adams and Essex, Calculus, 2010].

Quote 1:
file://Adams_Complex_01.png

\begin{code}
data ImagUnit = IU
i :: ImagUnit
i = IU
\end{code}
Quote 2:
file://Adams_Complex_02.png
\begin{code}
c1 :: CB
c1 = PlusI1 3 4 i   -- 3 + 4i
c2 = PlusI2 3 i 4   -- 3 + i4
        
data CB where
  PlusI1 :: REAL -> REAL -> ImagUnit -> CB  -- första formen
  PlusI2 :: REAL -> ImagUnit -> REAL -> CB  -- andra formen
--  MinusI :: REAL -> REAL -> ImagUnit -> CB  -- 7/2 - 2/3 i

showCB :: CB -> String
showCB (PlusI1 a b IU) = show a ++ "+" ++ show b ++ "i"
showCB (PlusI2 a IU b) = show a ++ "+" ++ "i"++ show b

instance Show CB where show = showCB
\end{code}
Quote 3:
file://Adams_Complex_03.png
\begin{code}
e1, e2, e3, e4 :: CB
e1 = PlusI1   3      2      IU
e2 = PlusI1  (7/2)  (-2/3)  IU -- 7/2 + (-2/3) i
e3 = PlusI2   0      IU     pi
e4 = PlusI1  (-3)    0      IU 
\end{code}
Quote 4:
file://Adams_Complex_04.png
\begin{code}
data CC where
  PlusI :: REAL -> REAL -> CC  -- Combining PlusI1 and PlusI2
 deriving Show

equalityCheck :: CC -> CC -> Bool
equalityCheck (PlusI a b) (PlusI x y) = error "TODO"

instance Eq CC where (==) = equalityCheck
\end{code}
Quote 5: 
file://Adams_Complex_05.png

Quote 6:
file://Adams_Complex_06.png
We define the functions |re| and |im| using pattern-matching.
\begin{code}
re, im :: CC -> REAL
re z@(PlusI x y) = x
im z@(PlusI x y) = y
\end{code}
An example of the use of pattern-matching:
Quote 7:
file://Adams_Complex_07.png
\begin{code}
      -- w     z     result
addCC :: CC -> CC -> CC
addCC = error "TODO"
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
 deriving Show

-- "wishful thinking" -- call eval recursively on subtrees
eval :: CE -> CC   -- x, y :: CE
eval (I)          = error "TODO"
eval (RCon r)     = error "TODO"
eval (Add e1 e2)  = error "TODO"
eval (Mul e1 e2)  = error "TODO"

-- semantic "constructors"
-- addCC  :: CC -> CC -> CC  -- defined above
mulCC   :: CC -> CC -> CC
mulCC (PlusI a b) (PlusI x y) = PlusI real imag
  where real = a*x-b*y
        imag = a*y+b*x

rconCC  :: REAL -> CC
rconCC = error "TODO: rconCC"

iCC     :: CC
iCC = error "TODO: iCC"
\end{code}


exercise: implement mulCC

Add    :: CE -> CE -> CE   -- syntactic constructor
addCC  :: CC -> CC -> CC   -- semantic "smart constructor"

