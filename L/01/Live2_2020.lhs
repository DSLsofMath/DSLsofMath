This file is from the "live coding" session at the end of L1.2
(study week 1, lecture 2, of the DSLsofMath course, 2020).

\begin{code}
{-# LANGUAGE GADTs #-}
module Live2 where
type REAL = Double
\end{code}

A datatype for "semantic complex numbers" as defined on page A.2 in
the book [Adams and Essex, Calculus, 2010].

\begin{spec}
data ImagUnit = I
data CA = CA1 REAL REAL ImagUnit
        | CA2 REAL ImagUnit REAL

c1 = CA1 3 2 I
c2 = CA2 7 I 2

instance Show ImagUnit where  show = showImagUnit
instance Show CA       where  show = showCA

showImagUnit :: ImagUnit -> String
showImagUnit I = "i"

showCA :: CA -> String
showCA (CA1 x y e) = show x ++ "+" ++ show y ++ showImagUnit e
showCA (CA2 x e y) = show x ++ "+" ++ showImagUnit e ++ show y
\end{spec}
\begin{code}
-- CB är typens namn
-- C är konstruerarens namn - en funktion av typen  REAL -> REAL -> CB
data CB = C REAL REAL  -- Re Im
  deriving Show

z, w :: CB
z = C 2 3
w = C 2 7

i = C 0 1

instance Num CB where -- ...
  (+) = plusCB
  (*) = timesCB

plusCB  :: CB -> CB -> CB
plusCB (C a b) (C x y) = C något någotannat
  where  något       = a + x  -- (+) :: REAL -> REAL -> REAL
         någotannat  = b + y

timesCB :: CB -> CB -> CB
timesCB (C a b) (C x y) = C (a*x-b*y) (a*y+b*x)

data CE = Plus CE CE | Times CE CE | I | ToComplex REAL
  deriving Show

e1, e2, e3 :: CE
e1 = Times I I
e2 = Plus e1 (ToComplex 1)
e3 = Times e1 e1
\end{code}


Translation from syntax to semantics:

\begin{code}
eval :: CE -> CB
eval I             = i
eval (ToComplex r) = toComplex r
eval (Plus e1 e2)  = eval e1  +  eval e2
eval (Times e1 e2) = eval e1  *  eval e2
--eval (Plus e1 e2)  = plusCB  (eval e1) (eval e2)
--eval (Times e1 e2) = timesCB (eval e1) (eval e2)

toComplex :: REAL -> CB
toComplex r = C r 0
\end{code}



Semantic version of the constructors:

Plus    :: CE -> CE -> CE
plusCB  :: CB -> CB -> CB


Showing values: another semantics of complex number expressions (as
Strings).

\begin{code}
-- instance Show CC where
--   show = showCC

-- showCC :: CC -> String
\end{code}

----------------

Examples of simpler datatypes:

Vi såg exempel på |type| och |data| - här ett enkelt exempel till
\begin{code}
-- data Nat = Zero | Succ Nat
data Nat where   -- Nat is the name of the type
  Zero :: Nat
  Succ :: Nat -> Nat
  Var  :: String -> Nat
  deriving Show

type NatSem = Tab -> Integer

evalNat :: Nat -> NatSem
evalNat Zero     = semZero
evalNat (Succ n) = semSucc (evalNat n)
evalNat (Var v)  = semVar v

semZero :: NatSem -- Tab -> Integer
semZero tab = 0

semSucc :: NatSem -> NatSem  -- (Tab->Integer) -> (Tab->Integer)
semSucc f = \tab -> 1 + (f tab)
--semSucc f tab = 1 + (f tab)

semVar :: String -> NatSem -- String -> (Tab -> Integer)
semVar x = \tab -> slåupp x tab

type Tab = String -> Integer
slåupp :: String -> Tab -> Integer
slåupp x tab = tab x

hej :: Nat
hej = Succ (Var "x")
\end{code}

\begin{code}
data ZZ where
  Pos :: Nat -> ZZ
  Neg :: Nat -> ZZ   -- adding the minus sign in front
\end{code}

What about Pos Zero and Neg Zero? Two representation for the "same" value.

Possible alternative to avoid that:
\begin{code}
data ZZ' where
  Pos' :: Nat -> ZZ'
  Neg' :: Nat -> ZZ'   -- Neg n means (-(n+1))  -- Neg Zero means -1
\end{code}

\begin{code}
data Rat where
  Q :: ZZ -> ZZ -> Rat
\end{code}

Note that Q 6 3 == Q 4 2 == Q 2 1 ... -- many representations for each value

Also: Q 1 0 is a Rat but should not be a rational!

Suggestion: add a type PosNat of just _positive_ naturals and use that as the second argument to Q.
