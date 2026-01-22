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

\begin{code}
data ImagUnit = IU
i' :: ImagUnit
i' = IU
\end{code}

----------------
Quote 2:
file://Adams_Complex_02.png
\begin{code}
data CA = PlusI1 REAL REAL ImagUnit
        | PlusI2 REAL ImagUnit REAL
-- c1, c2 :: CA
-- c1 = PlusI1 3 4 i
-- c2 = PlusI2 3 i 4
\end{code}

toC :: REAL -> C

\begin{code}
showIU :: ImagUnit -> String
showIU IU = "i"

showCA :: CA -> String
showCA (PlusI1 x y u) = show x ++ "+" ++ show y ++ showIU u
showCA (PlusI2 x u y) = show x ++ "+" ++ showIU u ++ show y

instance Show CA where  show = showCA
\end{code}

----------------
Quote 3:
file://Adams_Complex_03.png

Code up some example expressions
\begin{code}
r2c :: REAL -> CA
r2c r = PlusI1 r 0 i'

e1, e2, e3, e4 :: CA
e1 = PlusI1 3 2 i'
e2 = PlusI1 (7/2) (-2/3) i'
e3 = PlusI2 0 i' pi
e4 = r2c (-3)
\end{code}

----------------
Quote 4:
file://Adams_Complex_04.png

\begin{code}
data CC where
  PlusI :: REAL -> REAL -> CC   -- a real part and an imaginary part make up a complex number
equalCC :: CC -> CC -> Bool
equalCC (PlusI x1 y1) (PlusI x2 y2) = (x1 == x2) && (y1 == y2)

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
re z@(PlusI x _y) = x
im z@(PlusI _x y) = y

c1, c2, c3, c4 :: CC
c1 = PlusI 3 2
c2 = PlusI (7/2) (-2/3)
c3 = PlusI 0 pi
c4 = PlusI (-3) 0
\end{code}

An example of the use of pattern-matching:
----------------
Quote 7:
file://Adams_Complex_07.png
\begin{code}
add, add' :: CC -> CC -> CC
add w z = PlusI foo bar
  where
    a = re w; b = im w;
    x = re z; y = im z;
    foo = a + x
    bar = b + y

add' (PlusI a b) (PlusI x y) = PlusI (a+x) (b+y)

mul :: CC -> CC -> CC
mul (PlusI a b) (PlusI x y) = PlusI (a*x - b*y) (x*b + a*y)

i :: CC
i = PlusI 0 1

fromReal :: REAL -> CC
fromReal r = PlusI r 0

testMul1 = mul i i == fromReal (-1)
\end{code}

Derive the definition of mul using laws: distributivity and i² = -1.

\begin{code}

showCC :: CC -> String
showCC (PlusI x y) = show x ++ "+" ++ show y ++ "i"

instance Show CC where show = showCC
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
-- not included: re and im  :: Complex -> REAL  --
-- could be included in a DSL for REAL number expressions

"wishful thinking" -- call eval recursively on subtrees
eval :: Syn -> Sem
\begin{code}
eval :: CE -> CC
eval (FromREAL r) = fromReal r
eval I            = i
eval (Add e1 e2)  = add (eval e1) (eval e2)
eval (Mul e1 e2)  = mul (eval e1) (eval e2)

ce1 :: CE
ce1 = Add I I
ce2 :: CE
ce2 = Mul ce1 ce1
ce3 = Add I (FromREAL 1)
ce4 = Mul ce3 ce3

test1 = eval ce1 == PlusI 0 2
test2 = eval ce2 == PlusI (-4) 0
test4 = eval ce4 == PlusI 0 2
\end{code}
"the world of syntax"
  FromREAL :: REAL -> CE
  I        :: CE
  Add      :: CE -> CE -> CE
  Mul      :: CE -> CE -> CE

eval

"the world of semantics"
  fromREAL :: REAL -> CC
  i        :: CC
  add      :: CC -> CC -> CC
  mul      :: CC -> CC -> CC
