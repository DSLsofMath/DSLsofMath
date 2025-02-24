Some live coding from the second half of lecture 1 (week 1).
(DSLsofMath course, 2023).

This file is an example of a "literate Haskell file" where the
default (like this intro text) is comment and code blocks are
enclosed in "\begin{code}" and "\end{code}."

\begin{code}
module Live1_2023 where
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

-- B = Bool = {F,T}
-- all values of type B->B
allBtoBs :: [Bool -> Bool]
allBtoBs = [id, not, const False, const True]

-- |f             |   f False     |  f True |
-- |--------------|---------------|---------|
-- |id            |   False       |  True   |
-- |not           |   True        |  False  |
-- |const False   |   False       |  False  |
-- |const True    |   True        |  True   |


-- id F = F; id T = T  ; id ~= (F,T)

b1 = allBtoBs !! 0  
b2 = allBtoBs !! 1
b3 = allBtoBs !! 2
b4 = allBtoBs !! 3

-- all values of type B->B->B
exercise :: [Bool -> Bool -> Bool]
exercise = [(&&), (||) ] -- ... should be quite a few more: 16 in total
\end{code}
-- How many are there?

  Bool -> A  ~=  (A, A)
  card A = number of values in A 
  card (Bool->A) = (card A)^2

  A = B->B

  card (B->(B->B)) = (card (B->B))^2 = ((card B)^2)^2 = (2^2)^2

  




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


div :: Int -> Int -> Maybe Int

\begin{code}
inf :: Double -- an approximation of the REAL number type
inf = 1/0

nan :: Double
nan = 0/0
\end{code}

Associative(+) = forall x,y,z. (x+y)+z == x+(y+z)
Note: Float and Double are _not_ associative (but almost)

\begin{code}
lhs, rhs :: Num a => a -> a -> a -> a
lhs x y z =  (x+y)+z
rhs x y z =  x+(y+z)

checkAssoc :: (Eq a, Num a) => a -> a -> a -> (a, a, a, Bool)
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

forall x. x*0 == 0

\begin{code}
data E = Add E E
       | Mul E E
       | Con Integer
       | X                   -- Add (Add (Mul X X) X) (Con 1) ~= x² + x + 1
  deriving Show
\end{code}

E is a type of abstract syntax trees.
   can represent simple functions
   (not only 1-argument expressions)

semantics = meaning

"meaning-assigning function"

translator from an abstract (un-interpreted) syntax to some meaningful value type (the semantic type)

\begin{code}
a1, a2 :: E
a1 = Add (Con 1) (Mul (Con 2) (Con 3))    -- 1+(2*3)
a2 = Mul (Add (Con 1) (Con 2)) (Con 3)    -- (1+2)*3
a3 = Mul a2 a2
a4 = Mul a3 a3

a5 = Mul X X
\end{code}

We can evaluate (translate) an E to an integer:

-- DSL for simple arithemtic expressions
eval  ::  Syntax  ->  Semantics
eval  ::  E       ->  Integer
-- the semantic domain is integers
\begin{code}
type AbsSyn = E
type Z = Integer
type Sem = Z -> Z   -- from the value of X to the value of the full expression
eval :: AbsSyn  ->        Sem
eval (Add e1 e2) = addE (eval e1) (eval e2)
eval (Mul e1 e2) = mulE (eval e1) (eval e2)
eval (Con c)     = conE c
eval X           = xE

-- Add := addE; Mul := mulE; Con := conE

addE :: Sem -> Sem -> Sem
addE s1 s2 vX = s1 vX    +    s2 vX
  --   s1, s2 :: Sem = Z -> Z
  -- bar :: Z
mulE :: Sem -> Sem -> Sem
mulE s1 s2 vX = s1 vX    *    s2 vX

conE :: Integer -> Sem -- Z -> Z
conE c vX = c

xE :: Sem
xE vX = vX
\end{code}
