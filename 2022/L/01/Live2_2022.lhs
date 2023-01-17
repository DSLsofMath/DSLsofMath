This file is from the "live coding" session at the end of L1.2
(study week 1, lecture 2, of the DSLsofMath course, 2022).

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
module Live2 where
type REAL = Double
\end{code}

A datatype for "semantic complex numbers" as defined on page A.2 in
the book [Adams and Essex, Calculus, 2010].

Quote 1:
file://Adams_Complex_01.png

\begin{code}
data ImagUnit = IU deriving Show
i :: ImagUnit
i = IU
\end{code}
Quote 2:
file://Adams_Complex_02.png
\begin{code}
data CA = Plu1 REAL REAL ImagUnit
        | Plu2 REAL ImagUnit REAL
data CB where
  Plus1 :: REAL -> REAL -> ImagUnit -> CB
  Plus2 :: REAL -> ImagUnit -> REAL -> CB
 deriving (Show)

showCB :: CB -> String
showCB (Plus1 x y IU) = show x ++ "+" ++ show y ++ "i"
showCB (Plus2 x IU y) = show x ++ "+" ++ "i" ++ show y
\end{code}
Quote 3:
file://Adams_Complex_03.png
\begin{code}
e1, e2, e3, e4 :: CB
e1 = Plus1 3       2      IU
e2 = Plus1 (7/2)  (-2/3)  IU  -- 7/2 + (-2/3) i
e3 = Plus2 0       IU     pi
e4 = Plus1 (-3)    0      IU
\end{code}
Quote 4:
file://Adams_Complex_04.png
\begin{code}
data CC where
  PlusI :: REAL -> REAL -> CC  -- real part + imagimary part
 deriving (Show, Eq)
-- PlusI a b == Plus1 a b IU == Plus2 a IU b

equalityCheck :: CC -> CC -> Bool
equalityCheck (PlusI a b) (PlusI x y) = a==x && b==y
\end{code}
Quote 5: 
file://Adams_Complex_05.png

Quote 6:
file://Adams_Complex_06.png
We define the functions |re| and |im| using pattern-matching.
\begin{code}
re, im :: CC -> REAL
re z@(PlusI x y) = x  -- left-hand side binds z, x, and y (RHS only uses one)
im z@(PlusI _ y) = y
\end{code}
An example of the use of pattern-matching:
Quote 7:
file://Adams_Complex_07.png
\begin{code}
      -- w     z     result
addCC :: CC -> CC -> CC
addCC w@(PlusI a b) z@(PlusI x y) = PlusI (a+x) (b+y)

addCC' :: CC -> CC -> CC
addCC' w z = PlusI (a+x) (b+y)
  where  PlusI a b = w
         PlusI x y = z

addCC'' :: CC -> CC -> CC
addCC'' w z = PlusI (a+x) (b+y)
  where  a = re w; b = im w
         x = re z; y = im z
\end{code}
-- if we used CB, then the im + re version would be better

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
So far we have defined "semantic complex numbers" (in cartesian form).
Now we turn to "syntactic complex number expressions" to define a DSL.
Three parts:
+ an abstract syntax type CE,
+ a  semantic type CC,
+ an evaluator eval :: CE -> CC

data CE = Add CE CE
        | Mul CE CE
        | RCon REAL
        | I
\begin{code}
data CE where
  Add  :: CE -> CE -> CE
  Mul  :: CE -> CE -> CE
  RCon :: REAL -> CE   -- "embed the real number as a complex number"
  I    :: CE

-- "wishful thinking" -- call eval recursively on subtrees
eval :: CE -> CC   -- x, y :: CE
eval (Add x y) = addCC (eval x) (eval y)
eval (Mul x y) = mulCC (eval x) (eval y)
eval I         = iCC
eval (RCon r)  = rconCC r   -- not eval r because r is not of type CE
-- semantic "constructors"
-- addCC  :: CC -> CC -> CC  -- defined above
mulCC   :: CC -> CC -> CC
mulCC (PlusI a b) (PlusI x y) = PlusI real imag
  where real = a*x - b*y
        imag = a*y + b*x 

rconCC  :: REAL -> CC
rconCC r = PlusI r 0

iCC     :: CC
iCC = PlusI 0 1 -- real part = 0, imaginary part = 1

e5 :: CE
e5 = Add (RCon 1) I  -- equal to sqrt (2*i)
\end{code}
-- e5^2 = 2*i



exercise: implement mulCC

Add    :: CE -> CE -> CE   -- syntactic constructor
addCC  :: CC -> CC -> CC   -- semantic "smart constructor"

----------------------------------------------------------------
lists of bool
length 0: []
length 1: [f], [t]
length 2: [f,f], [f,t], [t,f], [t,t]
length 3: [f,f,f], [f,f,t], ...
...
length n: ... (2^n lists)
...

Types in Haskell
"data" for syntax trees
\begin{code}
data Boo = Tru | Fals
data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Add parameters
data Foo a = Hi a | Ho a a
  -- a is a type parameter to the datatype Foo a
foo :: Foo a -> Foo a
foo (Hi x)    = Ho x x
foo (Ho x y)  = Hi y

data Option a = None | Some a

div :: REAL -> REAL -> Option REAL
div x 0 = None
div x y = Some (x/y)

data Eit a b = Lef a | Righ b

div' :: REAL -> REAL -> Eit String REAL
div' x 0 = Lef "div by zero"
div' x y = Righ (x/y)

\end{code}
