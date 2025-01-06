Some live coding from the second half of lecture 1 (week 1).
(DSLsofMath course, 2020).

This file is an example of a "literate Haskell file" where the
default (like this intro text) is comment and code blocks are
enclosed in "\begin{code}" and "\end{code}."

\begin{code}
import Data.Ratio

r :: Rational
r = 3%2

f x = x^2
\end{code}

The function |f| can be given different types:

\begin{code}
f1 :: Integer -> Integer
f1 = f
f2 :: Float -> Float
f2 = f
f3 :: Rational -> Rational
f3 = f
-- and the most general:
f :: Num a =>    a -> a
\end{code}

The type class Num has the operations (+), (*), (-), fromInteger (ask ghci
with ":i" to get the full list) but not the operation (/).

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

nan :: Double
nan = 0/0
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

nonAssoc :: (Double, Double, Double, Bool)
nonAssoc = checkAssoc (1/3) 1 1

isAssoc :: (Rational, Rational, Rational, Bool)
isAssoc = checkAssoc (1/3) 1 1
\end{code}

----------------

data nedan definierar den nya typen E och tre nya konstruerare: Add, Con, och Var.

\begin{code}
data E  =  Add E  E  -- konstruerare följt av typerna på argumenten
        |  Con Integer
        |  Var String
  deriving (Eq, Show)
\end{code}

|E| is a type of syntax trees.

\begin{code}
e1, e2 :: E
e1 = Add (Con 2) (Var "x")
e2 = Add e1 e1
\end{code}
With a "symbol table" we can evaluate an E:
\begin{code}
type Table = String -> Integer
eval :: E -> Table -> Integer
eval (Con c)    t  =  c
eval (Add x y)  t  =  (eval x t) + (eval y t)
eval (Var v)    t  =  t v

testTab :: Table
testTab "x" = 3
testTab _   = 0

test :: Integer
test = eval e2 testTab
\end{code}
