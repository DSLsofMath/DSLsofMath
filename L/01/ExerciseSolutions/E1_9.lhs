Exercise 1.9: The tupling transform and type driven development

When trying to implement the tupling transform function |f2p|

\begin{code}
f2p :: (a->(b,c)) -> (a->b, a->c)
\end{code}

it is useful to let the types guide the development. First off, we are defining a function of one argument, so we can start by writing

\begin{spec}
f2p a2bc = pairFun
  where
    a2bc :: a -> (b,c)
    pairFun :: (a->b, a->c)
\end{spec}

Here I have named the argument to mimic its type; the pronunciation of "a2bc" sounds like "a to b c".
The type of |pairFun| shows that we can build it from two components: we thus refine the definition to

\begin{spec}
f2p a2bc = (a2b, a2c)
  where
    a2b :: a -> b
    a2c :: a -> c
\end{spec}

When we now continue to define the first of these components, |a2b|, we have |a2bc| in our context in case we need it.
As |a2b| is of function type we again start by giving it an argument |x|:

\begin{spec}
    a2b x = body
      where
        body :: b
\end{spec}

Finally, when trying to define |body| we have both |a2bc| and |x| in scope, which is good, because they fit together: we can compute a pair by applying |a2bc| to |x|.
\begin{spec}
        pair = a2bc x
          where pair :: (b,c)
\end{spec}
When we have a pair we break take it apart to get the first component using |fst :: (b,c) -> b|:
\begin{spec}
        body = fst pair
\end{spec}
similarly, we can get at the second component using |snd :: (b,c) -> c|:
\begin{spec}
        body' = snd pair
\end{spec}

Finally, we assemble all we got into one complete definition:

\begin{code}
f2p a2bc = (a2b, a2c)
  where
    a2b x = fst (a2bc x)
    a2c x = snd (a2bc x)
\end{code}

We can shorten this a bit by noting that |a2b| (and |a2c|) can be defined using function composition:
\begin{code}
f2p a2bc = (a2b, a2c)
  where
    a2b = fst . a2bc
    a2c = snd . a2bc
\end{code}
And we can take this one step further by inlining their definitions to make this one-liner:
\begin{code}
f2p a2bc = (fst . a2bc, snd . a2bc)
-- or even
f2p fg = (fst.fg, snd.fg)
\end{code}

The other direction is simpler, from a pair of functions we build a
function to pairs, and the types guide the way:

\begin{code}
p2f :: (a->b , a->c)  -> (a -> (b    , c    ))
p2f    (a2b  , a2c )  =  \x -> (a2b x, a2c x)
-- or even
p2f (f,g) = \x -> (f x, g x)
\end{code}

As a bonus exercise we can prove that they compose to the identity:

\begin{code}
(===) :: a -> a -> Bool
(===) = undefined
claim  =  (p2f . f2p) === id
\end{code}

We do this by a chain of equational reasoning (type checked by
the Haskell compiler, but not checked for correctness by Haskell).

\begin{code}
type Equal a = [a]
poorMansProof :: (a -> (b, c)) -> a -> Equal (b, c)
poorMansProof fg x =
  [ (p2f . f2p) fg x                  -- Def. of |(.)|
  , p2f (f2p fg) x                    -- Def. of |f2p|
  , p2f (fst.fg, snd.fg) x            -- Def. of |p2f|
  , ((fst.fg) x, (snd.fg) x)          -- Def. of |(.)|, twice
  , (fst (fg x), snd (fg x))          -- Pair law
  , fg x                              -- Def. of |id|
  , id fg x
  ]
\end{code}
Thus, |(p2f . f2p)| is equal to |id :: (a -> (b, c)) -> (a -> (b, c))|.

\begin{code}
main :: IO ()
main = let xs = poorMansProof myfg 3
       in do print xs
             print (allEq xs)

myfg :: Int -> (Int, String)
myfg i = (1+i, show i)

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (x:xs) = all (x==) xs
\end{code}
