This file is from the "live coding" session at the end of L1.2
(study week 1, lecture 2, of the DSLsofMath course, 2022).

\begin{code}
{-# LANGUAGE GADTs #-}
module Live2 where
type REAL = Double
\end{code}

A datatype for "semantic complex numbers" as defined on page A.2 in
the book [Adams and Essex, Calculus, 2010].

\begin{code}
data ImagUnit where
  II :: ImagUnit
 deriving Show
-- data ImagUnit = I

--i :: ImagUnit
--i = II

data CB where
  Plus1 :: REAL -> REAL -> ImagUnit -> CB
  Plus2 :: REAL -> ImagUnit -> REAL -> CB
 deriving (Show)

-- e1 = Plus1 3 2 I     -- 3+2i
-- e3 = Plus2 0 I pi

showCB :: CB -> String
showCB = error "showCB: TODO"

addCB :: CB -> CB -> CB
addCB (Plus1 a b j) (Plus1 a' b' j') = error "TODO"
addCB (Plus1 a b j) (Plus2 a' j' b') = error "TODO"
addCB (Plus2 a j b) (Plus1 a' b' j') = error "TODO"
addCB (Plus2 a j b) (Plus2 a' j' b') = error "TODO"

e1 = PlusI 3 2
e3 = PlusI 0 (-1)
addCC :: CC -> CC -> CC
addCC (PlusI re im) (PlusI re' im') = PlusI realpart imagpart
  where  realpart = re + re'
         imagpart = im + im'
data CC where
  PlusI :: REAL -> REAL -> CC
 deriving (Show)
-- PlusI a b == Plus1 a b I

--instance Num CC where
--   (+) = addCC
--   (*) = mulCB


mulCB :: CB -> CB -> CB
mulCB = error "TODO mul"
\end{code}

Translation from syntax to semantics:

\begin{code}
data CE where
  Add  :: CE -> CE -> CE
  Mul  :: CE -> CE -> CE
  RCon :: REAL -> CE
  I    :: CE

-- "wishful thinking"
eval :: CE -> CC  -- x :: CE, I need a value of type CC
eval (Add x y) = addCC (eval x) (eval y)
eval I         = iCC
eval (RCon r)  = rconCC r
-- semantic "constructors"
-- addCC  :: CC -> CC -> CC
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
