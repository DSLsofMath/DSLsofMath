Some live coding from the second half of lecture 1 (week 1).
(DSLsofMath course, 2022).

This file is an example of a "literate Haskell file" where the
default (like this intro text) is comment and code blocks are
enclosed in "\begin{code}" and "\end{code}."

\begin{code}
module Live1_2022 where
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

The type class Num has the operations (+), (*), (-), fromInteger (ask
 ghci with ":i" to get the full list) but not the operation (/).

Fractional has the operations from Num and also division (/).

\begin{code}
g :: Fractional a => a -> a
g x = x / x    -- 1 om x/=0, annars?
\end{code}

Float and Double have
+ finite precision (most real numbers are missing)
+ but also extra values: NaN, Infinity, -Infinity, and a few more

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

The data declaration below defines the new type E and three new
constructor functions: Add, Mul, och Con.

\begin{code}
data E  =  Add E  E  -- constructor function name followed by types of arg.s
        |  Mul E  E
        |  Con Integer
  deriving (Eq, Show)
\end{code}

E is a type of abstract syntax trees.

\begin{code}
e1, e2 :: E
e1 = Add (Con 1) (Mul (Con 2) (Con 3))    -- 1+(2*3)
e2 = Mul (Add (Con 1) (Con 2)) (Con 3)    -- (1+2)*3
\end{code}

We can evaluate (translate) an E to an integer:

-- DSL for simple arithemtic expressions
eval  ::  Syntax  ->  Semantics
eval  ::  E       ->  Integer
-- the semantic domain is integers
\begin{code}
eval :: E  ->  Integer
eval (Add x y)  =  error "eval: Add TODO" 
eval (Mul x y)  =  error "eval: Mul TODO" 
eval (Con c)    =  error "eval: Con TODO" 
\end{code}

What about variables?
Start with one variable - call it X.

