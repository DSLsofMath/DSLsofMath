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

Some test functions

\begin{code}
f1 ::           Int -> Bool
f2 :: Double -> Int
f3 :: Double ->        Bool
f1 = even                -- built-in
f2 = round               -- built-in
f3 = error "TODO" -- combine f1 with f2
sq = error "TODO" -- squaring the argument 
\end{code}





Type-Driven Development
 -- use parametrically polymorphic types to guide the implementation

Functions:
\begin{code}
id :: a -> a
id = error "TODO"

const :: a -> (b -> a)
const = error "TODO"

-- Function composition
(.) :: (b->c) -> (a->b) -> (a->c)
(.) = error "TODO"

-- redo f3 using (.)
\end{code}

-- end of part 1










Part 2: *Pairs* (cartesian product)
Mathematical syntax: A⨯B = { (x,y) | x∈A, y∈B }
Haskell syntax:
  The type (a,b) has values of
  the form (x,y) where x :: a and y :: b.
\begin{code}
fst :: (a,b) -> a
fst = error "TODO"
snd :: (a,b) -> b
snd = error "TODO"

swap :: (a,b) -> (b,a)
swap = error "TODO"

assoc  :: (a,(b,c)) -> ((a,b),c)
assoc = error "TODO"

f2p :: (a -> (b, c)) -> (a->b, a->c)
f2p = error "TODO"

p2f :: (a->b, a->c) -> (a -> (b, c))
p2f = error "TODO"
\end{code}


Some example uses
\begin{code}

\end{code}

\begin{code}
what :: (a->b, a->c)  ->  (a->b, a->c)
what = f2p . p2f
-- what == id

ex1 = ((+1), even)
ex2 = what ex1
\end{code}

End of 2nd part








Third part: *Sum types (Maybe, Either)*

\begin{code}
data Maybe a -- TODO a type for "maybe an a"
maybe :: b -> (a -> b) -> Maybe a -> b
maybe = error "TODO"















data Either a b
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
