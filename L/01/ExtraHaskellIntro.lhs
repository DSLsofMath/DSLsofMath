\begin{code}
{-# LANGUAGE GADTs #-}
module Either where
import Prelude hiding (id, const, (.), fst, snd, swap, either, Either(Left,Right))
\end{code}

Type-Driven Development
 -- use parametrically polymorphic types to guide the implementation

Functions:
id :: forall a. a->a
id :: Int -> Int
id :: Bool -> Bool
id :: (Int->Bool) -> (Int->Bool)
\begin{code}
id :: a -> a   -- what is a?  a type variable (unconstrained)
id hi = hi
id' ::  a -> a
id' = \ x -> x     -- x :: a  , x/=a
  -- lambda expression

const :: a -> (b -> a)
const x y = x
  -- x :: a, y :: b
const' :: a -> (b -> a)   -- type variables
const' = \x -> \y -> x    -- value variables

-- Function composition
(.) :: (b->c) -> (a->b) -> (a->c)
(f . g) x = f (g x)

 -- referentially transparent
        -- rhs :: a -> c
        --   f :: b -> c
        --   g :: a -> b
        --   x :: a


-- test functions
f1 ::           Int -> Bool
f2 :: Double -> Int
f3 :: Double ->        Bool
f1 = even
f2 = round
f3 = f1 . f2
\end{code}















Pairs
data Pair a b = PairConstructor a b
         (a,b) :: *
         PairConstructor :: a -> b -> Pair a b
                            a -> b -> (a,b)

The type (a,b) has values of the
    form (x,y) where x :: a and y :: b.
\begin{code}
fst :: (a,b) -> a
fst    (x,_) =  x
snd :: (a,b) -> b
snd    (_,y) =  y

swap :: (a,b) -> (b,a)
swap    (x,y) =  (y,x)
  -- rhs :: (b,a)
  -- x :: a
  -- y :: b
swap' :: (a,b) -> (b,a)
swap' p = (snd p,fst p)
swap'' :: (a,b) -> (b,a)
swap'' = \p -> (snd p,fst p)
swap''' :: (a,b) -> (b,a)
swap''' = \(x,y) -> (y,x)

assoc  :: (a,(b,c)) -> ((a,b),c)
assoc     (x,(y,z)) =  ((x,y),z)

f2p :: (a -> (b, c)) -> (a->b, a->c)
--  :: (           ) -> (    ,     )
f2p f = (fst . f, snd . f)

p2f :: (a->b, a->c) -> (a -> (b, c))
--  :: (    ,     ) -> (           )
p2f (f, g) = \x -> (f x, g x)
  -- f :: a->b
  -- g :: a->c
  -- rhs :: a ->   (b, c)
f4 :: Double -> (Int, Bool)
f4 x = (round x, f3 x)

mypair :: (Double -> Int, Double -> Bool)
mypair = f2p f4
onefun   = fst mypair
otherfun = snd mypair
mynewfun :: Double -> (Int, Bool)
mynewfun = p2f mypair

what :: (a->b, a->c)  ->  (a->b, a->c)
what = f2p . p2f
-- what == id

ex1 = ((+1), even)
ex2 = what ex1
\end{code}


























Sum types (Either)

\begin{code}
data Either a b where
  Left  :: a -> Either a b
  Right :: b -> Either a b
 deriving Show
either :: (a->r) -> (b->r) -> (Either a b -> r)
either f _ (Left x)  = f x
either _ g (Right y) = g y
  -- f   :: a -> r
  -- e   :: Either a b
  -- Left x  :: Either a b
  --      x  ::        a
  -- g   :: b -> r
  -- Right y :: Either a b
  --       y ::          b
  -- rhs :: r


ei1 :: Either String b
ei1 = Left "hi"

ei2 :: Either a Bool
ei2 = Right False

l1 :: [Either String Bool]
l1 =  [ei1, ei2]


d       :: String -> String
d s = s++s

showB :: Bool   -> String
showB = show

te1 :: String
te1 = either d showB ei1
te2 :: String
te2 = either d showB ei2
\end{code}
