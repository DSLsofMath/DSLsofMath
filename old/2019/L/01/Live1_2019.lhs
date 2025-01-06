Some live coding from the second half of lecture 1 (week 1).
(DSLsofMath course, 2019).

This file is an example of a "literate Haskell file" where the
default (like this intro text) is comment and code blocks are
enclosed in "\begin{code}" and "\end{code}."

Starting with a simple function
\begin{code}
f x = x*x
\end{code}

It can be given different types:

\begin{code}
f1 :: Integer -> Integer
f1 = f
f2 :: Float -> Float
f2 = f
f3 :: Num a =>    a -> a
f3 = f
\end{code}

Num has the operations (+), (*), (-), fromInteger (ask ghci
with ":i" to get the full list) but not (/).

Fractional has the operations from Num and also division (/).

\begin{code}
g :: Fractional a => a -> a
g x = x / x
\end{code}

Float and Double have
* finite precision (most real numbers are missing)
* but also extra values: NaN, Infinity, -Infinity, and a few more

\begin{code}
inf :: Double
inf = 1/0
\end{code}

Associative(+) = forall x,y,z. (x+y)+z == x+(y+z)
Note: Float and Double are _not_ associative (but almost)

\begin{code}
lhs x y z =  (x+y)+z
rhs x y z =  x+(y+z)

checkAssoc x y z = (l, r, l-r, l == r)
  where
    l = lhs x y z
    r = rhs x y z

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]
\end{code}

Sv: Fakultet, En: factorial

Some type synonyms:
\begin{code}
type Tal = Integer
type Ålder = Integer
type Skostorlek = Integer
\end{code}

Creating new types:
\begin{code}
newtype Age      -- name of the new type
      = Age2     -- function (constructor) to build Age values
        Integer  -- inner type
\end{code}

Age2 :: Integer -> Age

\begin{code}
e :: Tal
e = 5

å :: Age
å = Age2 5
\end{code}

----------------

data nedan definierar den nya typen U och tre nya konstruerare: Plus, Gånger, och Tal

\begin{code}
data U  =  Plus    U  U  -- konstruerare typerna på argumenten
        |  Gånger  U  U
        |  Tal Integer
  deriving (Eq, Show)
\end{code}

|U| is a type of syntax trees.

\begin{code}
u1 = Plus (Gånger (Tal 2) (Tal 3)) (Tal 5)  -- 11
u2 = u1 `Gånger` u1                         -- 121

eval :: U -> Integer
eval (Tal n)      = n
eval (Plus x y)   = (eval x) + (eval y)
eval (Gånger x y) = (eval x) * (eval y)
\end{code}


----------------

Definierar den nya typen |E a| (för varje |a|)
och tre nya konstruerare: |Plus|, |Gånger|, och |Tal|

\begin{code}
data E a  =  Add  (E a) (E a)  -- konstruerare typerna på argumenten
          |  Mul  (E a) (E a)
          |  Con  a
  deriving (Eq, Show)
\end{code}

In this case the derived show has type |Show a =>  E a -> String|.

\begin{code}
e1 :: E String
e1 = Add (Mul (Con "2") (Con "3")) (Con "5.0")

e2 :: Num a => E a
e2 = Add (Mul (Con 2) (Con 3)) (Con 5)

g1, g2 :: Tal -> Tal
g1 x = 1+x
g2 x = 2*x

e3 :: E (Tal->Tal)
e3 = Add (Con g1) (Con g2)
\end{code}
