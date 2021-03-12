\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module P3 where
import Numeric.Natural
import Test.QuickCheck
import Data.Bits

-- a)
class Thing t where
  start  :: t
  grow   :: t -> t
  merge  :: t -> t -> t

-- Not requested, but useful for typechecking and testing
law1 x   = merge start x == start && merge x start == start
law2 x y = merge (grow x) (grow y) == grow (merge x y)

-- b)
data T v where
  Start  :: T v
  Grow   :: T v -> T v
  Merge  :: T v -> T v -> T v
  V      :: v -> T v
 deriving (Show)

instance Thing (T v) where start=Start; grow=Grow; merge=Merge

-- c)

type Nat = Natural
type Str = String
type R   = Double
type B   = Bool
newtype Pos = P Natural deriving (Eq, Num, Show, Bits)

-- several possible instances:
instance Thing Nat  where start = 0;  grow = (1+);   merge = min
instance Thing Str  where start = ""; grow = ('!':); merge = zipWith const
instance Thing ()   where start = (); grow = id;     merge = const
instance Thing Pos  where start = 1;  grow = (2*);   merge = mergePos
instance Thing R    where start = 0;  grow = recip;  merge = (*)
instance Thing B    where start = False;grow=id;     merge = (&&)
instance Thing Int  where start = 0;  grow = (^2);   merge = (*)
{- Not an instance:
instance Thing B    where start = False;grow=not;    merge = (&&) -- fails law2
instance Thing B    where start = False;grow=not;     merge = (||)-- fails law1&law2
instance Thing Int  where start = 0;  grow = negate; merge = (+)  -- fails law1
instance Thing Int  where start = maxBound;grow=(1+);merge = max  -- fails law2 start 0
-}

mergePos (P 1) y = P 1
mergePos x (P 1) = P 1
mergePos (P x) (P y) | even x && even y = 2*mergePos (P $ div x 2) (P $ div y 2)
                     | otherwise = P 1

test1 :: Thing t => t
test1 = merge (grow start) (grow (grow start))

-- d)
eval :: Thing t => (v->t) -> (T v -> t)
eval var = e where
  e Start        = start
  e (Grow a)     = grow (e a)
  e (Merge a b)  = merge (e a) (e b)
  e (V v)        = var v

-- e)
evalN :: (v->Nat) -> (T v -> Nat)
evalN = eval
evalS :: (v->String) -> (T v -> String)
evalS = eval

x, y, z, e1, e2, e3 :: T String
[x, y, z] = map V ["x","y", "z"]
e1 = Merge x y
e2 = Grow z
e3 = Merge e1 e2

varN "x" = 2
varN "y" = 3
varN "z" = 4

varS = id

tests = ( map (evalN varN) [e1,e2,e3]  ==  [2, 5, 2]
        , map (evalS varS) [e1,e2,e3]  ==  ["x","!z","x"]
        )

-- Not required:

main = do quickCheck (law1 :: Nat -> Bool); quickCheck (law2 :: Nat->Nat->Bool)
          quickCheck (law1 :: Str -> Bool); quickCheck (law2 :: Str->Str->Bool)
          quickCheck (law1 ());             quickCheck (law2 () ())
          quickCheck (law1 :: Pos -> Bool); quickCheck (law2 :: Pos->Pos->Bool)

instance Arbitrary Natural where
  arbitrary = fmap (fromInteger . abs) arbitrary

instance Arbitrary Pos where
  arbitrary = fmap (P . (1+)) arbitrary
\end{code}
