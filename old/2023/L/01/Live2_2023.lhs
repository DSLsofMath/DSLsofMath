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
data CA = PlusI1 REAL REAL ImagUnit  -- första formen
        | PlusI2 REAL ImagUnit REAL  -- andra formen
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
e1, e2, e3, e4 :: CC
e1 = PlusI 3 2  
-- e2 = MinusI (7/2) (2/3) i   -- egentligen utanför "syntaxen"
e2 = PlusI (7/2) (-2/3)    -- egentligen utanför "syntaxen"
e3 = PlusI 0  pi
e4 = PlusI (-3) 0 
\end{code}
Quote 4:
file://Adams_Complex_04.png
\begin{code}
data CC where
  PlusI :: REAL -> REAL -> CC  -- första och andra formen
 deriving Show

equalityCheck :: CC -> CC -> Bool
equalityCheck (PlusI a b) (PlusI x y) =  a==x  &&  b==y

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
+ an abstract syntax type CE,
+ a  semantic type CC,
+ an evaluator eval :: CE -> CC
\begin{code}
data CE where
  I   :: CE              
  ToC :: REAL -> CE
  Add :: CE -> CE -> CE
  Mul :: CE -> CE -> CE
 deriving Show
e6 :: CE
e6 = Add I (Mul (ToC 2.3) (ToC 3.7))

-- "wishful thinking" -- call eval recursively on subtrees
eval :: CE -> CC   -- x, y :: CE
eval (I)          = PlusI 0 1
eval (ToC r)      = PlusI r 0
eval (Add e1 e2)  = addCC (eval e1) (eval e2)
eval (Mul e1 e2)  = mulCC (eval e1) (eval e2)

-- semantic "constructors"
-- addCC  :: CC -> CC -> CC  -- defined above
mulCC   :: CC -> CC -> CC
mulCC (PlusI a b) (PlusI x y) = PlusI real imag
  where real = a*x-b*y
        imag = a*y+b*x

e7 :: CE
e7 = Add I (ToC 1)

e8 :: CE
e8 = Mul e7 e7

rconCC  :: REAL -> CC
rconCC = error "TODO: rconCC"

iCC     :: CC
iCC = error "TODO: iCC"

e5 :: CE
e5 = error "TODO: e5" -- 1+i
\end{code}


exercise: implement mulCC

Add    :: CE -> CE -> CE   -- syntactic constructor
addCC  :: CC -> CC -> CC   -- semantic "smart constructor"

----------------------------------------------------------------

Types in Haskell
"data" for syntax trees
\begin{code}
-- data Bool = False | True
data Veckodag = Mån | Tis | Ons | Tor | Fre | Lör | Sön

-- Parameterised types: optional values, sum types
data Kanske a = Inget | Bara a deriving Show
-- Används ofta när det kan bli fel => Inget svar

säkerDivision :: REAL -> REAL -> Kanske REAL
säkerDivision x 0 = Inget
säkerDivision x y = Bara (x/y)

-- data Either a b = Left a | Right b
data Summa a b = Vänster a | Höger b

s1 :: Summa String b
s1 = Vänster "hej"

s2 :: Summa a Bool
s2 = Höger False

ss :: [Summa String Bool]
ss = [s1, s2]
\end{code}
Veckodag :: Type
Kanske :: Type -> Type


data Maybe a = Nothing | Just a
