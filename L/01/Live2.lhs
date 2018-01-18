This file is the end result of the "live coding" session at the end of L1.2
(study week 1, lecture 2 within the week, of the DSLsofMath course, 2018).

This is an example of a "literate Haskell file" where the default is comment
text and code blocks are enclosed in "\begin{code}" and "\end{code}."

The following "spec" block is, from the point of view of the Haskell
interpreter, just part of the comment. The code in this block was the first
version of the datatype for "semantic complex numbers" as defined on page A.2 in
the book [Adams and Essex, Calculus, 2010]. It was later commented out in favour
of a new definition.

\begin{spec}
data ImagUnit = I
  deriving Show

i :: ImagUnit
i = I

data CA  =  CA1 REAL REAL ImagUnit
         |  CA2 REAL ImagUnit REAL
  deriving Show

cA1 = CA1 3 2 i
cA2 = CA1 (7/2) (-2/3) i
\end{spec}

Here is the next version of the "semantic complex numbers".

\begin{code}
module Live2 where

type REAL = Double

data CB  =  CB REAL REAL
  deriving (Eq) -- Show

cB1 = CB 3 2
cB2 = CB (7/2) (-2/3)

plusCB :: CB -> CB -> CB
    --  (a+bi) +  (c+di)   =    (a+c)+(b+d)i
plusCB (CB a b) (CB c d)   = CB (a+c) (b+d)

i :: CB
i = CB 0 1
\end{code}

If we want to show the values in a more readable way, we can provide our own
"show" function as follows:

\begin{code}
showCB :: CB -> String
showCB (CB a 0) = show a
showCB (CB 0 b) = show b ++ "i"
showCB (CB a b) = show a ++ "+" ++ show b ++ "i"

instance Show CB where
  show = showCB
\end{code}

Now we continue by typing in the computation from the blackbord:

\begin{code}
multCB :: CB -> CB -> CB
    --  (a+bi) * (c+di)    =    (ac-bd)+(ad+bc)i
multCB (CB a b) (CB c d)   = CB (a*c-b*d) (a*d+b*c)

test1, test2 :: CB
test1 = multCB i i
test2 = test1 * test1
  where (*) = multCB
\end{code}

The next step after defining these operations is to make the notation a bit
smoother. We will talk much more about type classes in later weeks, so check
elsewhere for a proper explanation. For now we can just treat it as a way of
overloading the math operators to enable simple notatoin for complex numbers.

\begin{code}
instance Num CB where
  (+) = plusCB
  (*) = multCB
  fromInteger n = CB (fromInteger n) 0
\end{code}

We also did a few examples with integer exponents: i^4, etc. and noted (as an
aside) that there are three "power" operators in Haskell, with different types:
(^), used here, (^^), and (**).

----

Semantics and Syntax trees

This datatype was built as a small extension of the corresponding type from
L1.1, where it was used for simple arithmetic (without the imaginary unit).

\begin{code}
data CE a  =  Add  (CE a) (CE a)  -- syntax: Konstr. Typ Typ
           |  Mul  (CE a) (CE a)
           |  Con  a
           |  I
  deriving (Eq, Show)

ce1, ce2 :: CE Integer
ce1 = Add (Mul (Con 2) (Con 3)) (Con 5)
ce2 = Mul ce1 ce1
\end{code}

From syntax tree to semantic value:
(Exercise: implement a variant with this type: |evalCE2 :: CE CB -> CB|.)

\begin{code}
evalCE :: CE REAL -> CB
evalCE (Add x y) = (evalCE x) + (evalCE y) -- x :: CE REAL, evalCE x :: CB
evalCE (Mul x y) = (evalCE x) * (evalCE y)
evalCE (Con c)  =  CB c 0
evalCE (I)      =  i -- CB 0 1

test :: [CB]
test = take 8 (map evalCE (iterate (Mul I) (Con 1)))
\end{code}
