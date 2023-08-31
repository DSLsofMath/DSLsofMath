This file shows some "live coding" from Chapter 1 of
  Domain-Specific Languages of Mathematics
  https://www.collegepublications.co.uk/computing/?00024
  (iimage-mode for inlining the quotes)

\begin{code}
{-# LANGUAGE GADTs #-}
module Live2_2023 where
type REAL = Double
\end{code}

A datatype for "semantic complex numbers" as defined on page A.2 in
the book [Adams and Essex, Calculus, 2010].

Quote 1:
file://Adams_Complex_01.png

(not yet mentioned complex numbers - strictly speaking)
ImagUnit =~ {IU}
\begin{code}
data ImagUnit = IU
i :: ImagUnit
i = IU
\end{code}
Quote 2:
file://Adams_Complex_02.png
Note that (bi + a) is not of the right form!
\begin{code}
c1 :: CB
c1 = PlusI1 3 4 i   -- 3 + 4i
c2 = PlusI2 3 i 4   -- 3 + i4
        
data CB where
  PlusI1 :: REAL -> REAL -> ImagUnit -> CB  -- first form
  PlusI2 :: REAL -> ImagUnit -> REAL -> CB  -- second form

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
equalityCheck (PlusI a b) (PlusI x y) = (a==x) && (b==y)

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
addCC w@(PlusI a b) z@(PlusI x y) = PlusI (a+x) (b+y)
\end{code}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
So far we have defined "semantic complex numbers" (in cartesian form).
Now we turn to "syntactic complex number expressions" to define a DSL.
Three parts:
+ an abstract syntax type CE,  "Complex Expression"
+ a  semantic type CC,
+ an evaluator eval :: CE -> CC
\begin{code}
data CE where
  Add  :: CE -> CE -> CE
  Mul  :: CE -> CE -> CE
  RCon :: REAL -> CE   -- embedding real constants
  I    :: CE              
 deriving Show

ce1, ce2, ce3, ce4 :: CE
ce1 = Add I I
ce2 = Mul I I   -- I^2 = -1
ce3 = Add ce1 ce2
ce4 = Add (RCon 2) I
ce5 = Mul ce2 ce2
ce6 = Mul ce5 ce5

-- "wishful thinking" -- call eval recursively on subtrees
eval :: CE -> CC   -- x, y :: CE
eval (I)          = PlusI 0 1
eval (RCon r)     = PlusI r 0
eval (Add e1 e2)  = addCC (eval e1) (eval e2)
eval (Mul e1 e2)  = mulCC (eval e1) (eval e2)

-- semantic "constructors"
-- addCC  :: CC -> CC -> CC  -- defined above
mulCC :: CC -> CC -> CC
mulCC (PlusI a b) (PlusI x y) = PlusI real imag
  where  real = a*x - b*y
         imag = a*y + b*x
\end{code}

  (a+bi)*(x+yi)
= -- distributivity
  ax + ayi + bix + biyi
= -- simplify (collect terms in a "polynomial in i")
  ax + (ay+bx)*i + by*i^2
= -- i^2 = -1 + simplify
  (ax - by) + (ay + bx)*i





exercise: implement mulCC

Add    :: CE -> CE -> CE   -- syntactic constructor
addCC  :: CC -> CC -> CC   -- semantic "smart constructor"

01234567890123456789012345678901234567890123456789012345678901234567890123456789
(The numbers are just here to help choose the font size when presenting.)
