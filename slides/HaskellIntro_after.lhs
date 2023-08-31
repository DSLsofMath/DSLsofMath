Literate programming - anything I write outside the code blocks is a
comment.

\begin{code}
{-# LANGUAGE GADTs #-}
module HaskellIntro where
import Prelude
  hiding (id, const, (.),
          fst, snd, swap,
          maybe, Maybe(Nothing,Just),
          either, Either(Left,Right))
\end{code}
comment again down here

Some test functions
Pure functional programming langauges
  based on functions
  :: means "has type"
\begin{code}
f1 ::           Int -> Bool    -- consumes an Int
f2 :: Double -> Int            -- produces an Int
f3 :: Double ->        Bool
f1 = even                -- built-in
f2 = round               -- built-in
f3 = \x -> f1 (f2 x)
sq = \x -> x^2
g = \x -> 1 + 2*x + x^2
\end{code}
anonymous function expressions
lambda expression: \x -> e
"lambda" is "ASCII approximated" by \ (backslash)

pure means "no side-effects"
random :: Nat -> Nat
random(6) would give a random number between 0 and 5
random(6) == 2
random(6) == 5
 2/=5 => error!



Type-Driven Development
 -- use parametrically polymorphic types to guide the implementation

Functions:
id for identity
\begin{code}
id :: a -> a -- forall types a, take an input of that type and return
             -- an answer of that same type
id    x =  x

const :: a -> (b -> a)
const x = \y -> x

-- Function composition
(.) :: (b->c) -> (a->b) -> (a->c)
--     (    ) -> (    ) -> (    )
f . g = \x -> f (g x)
f3'   = f1 . f2
\end{code}
Laziness
Haskell is a "lazy" programming language.

(.) is a "higher-order" function
  (a function taking functions as arguments)
  "operator"


-- end of part 1










Part 2: *Pairs* (cartesian product)
Mathematical syntax: A⨯B = { (x,y) | x∈A, y∈B }
{1,2}⨯{a,b,c} = {(1,a),(2,a),
                 (1,b),(2,b),
                 (1,c),(2,c)}
Haskell syntax:
  The type (a,b) has values of
  the form (x,y) where x :: a and y :: b.
Pattern-matching
\begin{code}
fst :: (a,b) -> a
fst (x,y) = x
  
snd :: (a,b) -> b
snd (x,y) = y

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

assoc  :: (a,(b,c)) -> ((a,b),c)
assoc (x,(y,z)) = ((x,y),z)

-- "function to pair (of functions)"
f2p :: (a -> (b, c)) -> (a->b, a->c)
--     (           ) -> (    ,     )
f2p f = (fst . f, snd . f)

  -- f :: a -> (b, c)
  -- f x :: (b, c)           

myfun :: Int -> (Bool, Int)
myfun n = (even n, n+1)

test1 :: (Int->Bool, Int -> Int)
test1 = f2p myfun

test1f :: Int->Bool
test1f = fst test1
test1s :: Int -> Int
test1s = snd test1


p2f :: (a->b, a->c) -> (a -> (b,  c ) )
p2f (f, g) = \x -> (f x, g x)
\end{code}
Program calculation is easier with short notation.

   x*(a+b) = (x*a) + (x*b)

Some example uses
\begin{code}

\end{code}

\begin{code}
what :: (a->b, a->c)  ->  (a->b, a->c)
what = f2p . p2f
-- what == id

ex1 = ((+1), even)
ex2 = what ex1


bad x = bad x

\end{code}
some :: String -> String -- too specific type to help "correctness"
some 
"more general types => fewer programs => correct programs"

(a->b, a->c)  <->  (a -> (b,  c ) )

 B^A * C^A     =   (B*C)^A              -- cardinality

cardinality = the number of elements in the types / sets

End of 2nd part



Turing completeness

Haskell is a general purpose language (and Turing complete)

(pairs and records are "product types")
there are  card A * card B = card (A,B)

Third part: *Sum types (Maybe, Either)*

Maybe Weekday =~
  {Nothing, Just Mon, Just Tue, Just Wed, ..., Just Sun}

card Weekday = 7
card (Maybe Weekday) = 1+7 = 8

\begin{code}
data Maybe a = Nothing | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe def action Nothing   = def
maybe def action (Just x)  = action x

safeDiv :: Double -> Double -> Maybe Double
safeDiv x 0 = Nothing
safeDiv x y = Just (x / y)












-- Either Bool Weekday =~ {Left False, Left True,
--                         Right Mon, Right Tue, ... Right Sun}
-- card (Either Bool Weekday) = card Bool + card Weekday = 2 + 7 = 9

-- Basically "a + b" or "disjoint union" or "tagged union"
data Either a b = Left a | Right b
either :: (a->r) -> (b->r) -> (Either a b -> r)
either = error "TODO"

-- Example values
ei1 :: Either String b
ei1 = error "TODO" -- Left "hi"

ei2 :: Either a Bool
ei2 = error "TODO" -- Right False

l1 :: [Either String Bool]
l1 =  [ei1, ei2]

\end{code}
01234567890123456789012345678901234567890123456789012345678901234567890123456789
(The numbers are just here to help choose the font size when presenting.)
