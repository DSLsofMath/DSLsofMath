> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> module P5 where

[20pts] Consider a non-deterministic system with a transition function
|f : G -> [G]| (for |G={0..5}|) represented in the following graph
[elided].  The transition matrix can be given the type |m :: G -> (G
-> Bool)| and the canonical vectors have type |e i :: G -> Bool| for
|i| in |G|.

    * (General questions.) What do the canonical vectors represent?
      What about non-canonical ones?
      What are the operations on |Bool| used in the matrix-vector
      multiplication?

    * (Specific questions.) Write the transition matrix |m| of the
      system.  Compute, using matrix-vector multiplication, the result
      of three steps of the system starting in state |2|.

a) Canonical vestors represent nodes or singleton subsets of |G|.
Non-canonical vectors represent sets of nodes (partial knowledge).

> instance Num Bool where
>   (+) = (||)
>   (*) = (&&)
>   fromInteger 0 = False
>   fromInteger 1 = True

b) The columns of the transition matrix show where you may get from a
certain node.

m = |001000|
    |100000|
    |000010|
    |011010|
    |000000|
    |010100|

let tr = transpose of a vector

v0 = (2==) = tr (001000)
v1 = mult m v0 = tr (100100)
v2 = mult m v1 = tr (010001)
v3 = mult m v2 = tr (000101)
-- for fun:
v4 = mult m v3 = tr (000001)
v5 = mult m v4 = tr (000000)
and in general m^5 = 0


----------------------------------------------------------------
-- Not part of the exam question, included for type checking.

> e i = (i==)

> lift2 :: (a->b->c) -> ((r->a)->(r->b)->(r->c))
> lift2 op f g = \x -> op (f x) (g x)
> lift1 :: (a->b) -> ((r->a)->(r->b))
> lift1 = (.)

> instance Num a => Num (x -> a) where
>   (+) = lift2 (+)
>   (*) = lift2 (*)
>   fromInteger = const . fromInteger
>   signum = lift1 signum
>   abs = lift1 abs

> f (G 0) = [G 1]
> f (G 1) = [G 3,G 5]
> f (G 2) = [G 0,G 3]
> f (G 3) = [G 5]
> f (G 4) = [G 2,G 3]
> f (G 5) = []

> class (Bounded a, Enum a) => Finite a
> instance (Bounded a, Enum a) => Finite a
> mulMatVec :: (Finite a) => Matrix a -> Vector a -> Vector a
> mulMatVec m v = \g -> sum (map ((`m` g) * v) [minBound..maxBound])

> m i j = elem j (f i)

> mult = mulMatVec
> v0 = (G 2 ==)
> v1 = mult m v0
> v2 = mult m v1
> v3 = mult m v2
> -- for fun:
> v4 = mult m v3
> v5 = mult m v4



> newtype G = G Int
>   deriving Eq

> instance Bounded G where
>   minBound = G 0
>   maxBound = G 5

> instance Enum G where
>   toEnum = G
>   fromEnum (G g) = g

> type Matrix g = g -> (g -> Bool)
> type Vector g = g -> Bool

> instance Finite g => Show (Vector g) where
>   show v = show $ map (showScalar . v) [minBound .. maxBound]

> showScalar False  = 0
> showScalar True   = 1


{-

the matrix elements are Booleans

canonical vectors = e i for i in {0..5}
  they represent the nodes

h (e i) represent the set of immediate successors

-}
