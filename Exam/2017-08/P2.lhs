Problem 2: Multiplication for matrices (from the matrix algebra DSL).

> {-# LANGUAGE FlexibleInstances #-}
> module P2 where
> -- The imports are not needed in the exam question
> import Prelude hiding (sum)
> import qualified P1
> import P1 (SemiRing,zero,one,add)
> import Test.QuickCheck

2a: Type the variables in the text.

Precise types (not available in Haskell):

A : M m n a
m : Nat
n : Nat
B : M n p a
p : Nat
i : Fin m
j : Fin p

where M m n a = Fin m -> Fin n -> a

----------------
2b: Type |mul| and |proj|

> proj :: Fin m -> Fin n -> M m n a -> a
> mul :: SemiRing a => M m n a -> M n p a -> M m p a

The function |proj| is parametrically polymorphic: it can project from
a matrix containing any type of elements. Thus is needs no class
constraint. For matrix multiplication to make sense you need addition
and multiplication on the element type. In standard Haskell you would
probably use the |Num| class and in a more mathematical setting you
may want to use a structure like |SemiRing| from P1.

----------------
2c: Implement |mul|.

Haskell version:

> type M m n a = [[a]]
> type V n a = [a]
> type Fin n = Nat
> type Nat = Int

> row :: Fin m -> M m n a -> V n a
> col :: Fin n -> M m n a -> V m a

> mul a b = matrix m p (\i j ->
>             sum (zipWith P1.mul (row i a) (col j b)))
>   where  m = rows a
>          p = cols b

> rows = length
> cols = length . head
> matrix :: Nat -> Nat -> (Fin m -> Fin n -> a) -> M m n a
> matrix m n f = [[f i j | j <- [0..n-1]] | i <- [0..m-1]]

----------------------------------------------------------------

Helpers and examples (not part of the exam question):

> row i m = m!!i
> col j m = map (!!j) m
> proj i j m = (m!!i)!!j
> sum :: SemiRing r => [r] -> r
> sum = foldr add zero

> m1 :: SemiRing r => M t t r
> m1 = [[one, zero],
>       [zero,one ]]
> m2 :: Num a => M m n a
> m2 = [[1,2,3],
>       [4,5,6]]
> m3 :: (Num a, SemiRing a) => M m n a
> m3 = mul m1 m2

> size = 3
> newtype M' a = M' [[a]]
>   deriving (Eq, Show)
> instance SemiRing a => SemiRing (M' a) where
>   mul (M' x) (M' y) = M' (mul x y)
>   add (M' x) (M' y) = M' (zipWith (zipWith add) x y)
>   zero = M' $ replicate size (replicate size zero)
>   one  = M' $ diagonal size one

> diagonal :: SemiRing r => Nat -> r -> M m m r
> diagonal m x
>   | m > 0     =  (x:replicate (m-1) zero) :
>                  map (zero:) (diagonal (m-1) x)
>   | otherwise =  []

> type MI = M' Integer
> type MB = M' Bool
> law1I i = P1.law1 (i :: MI); law1B b = P1.law1 (b :: MB)
> law2I i = P1.law2 (i :: MI); law2B b = P1.law2 (b :: MB)
> law3I i = P1.law3 (i :: MI); law3B b = P1.law3 (b :: MB)
> law4I i = P1.law4 (i :: MI); law4B b = P1.law4 (b :: MB)
> law5I i = P1.law5 (i :: MI); law5B b = P1.law5 (b :: MB)
> law6I i = P1.law6 (i :: MI); law6B b = P1.law6 (b :: MB)
> law7I i = P1.law7 (i :: MI); law7B b = P1.law7 (b :: MB)
> law8I i = P1.law8 (i :: MI); law8B b = P1.law8 (b :: MB)


> instance Arbitrary a => Arbitrary (M' a) where
>   arbitrary = fmap M' $ vectorOf size (vectorOf size arbitrary)

> main = do quickCheck law1I; quickCheck law1B
>           quickCheck law2I; quickCheck law2B
>           quickCheck law3I; quickCheck law3B
>           quickCheck law4I; quickCheck law4B
>           quickCheck law5I; quickCheck law5B
>           quickCheck law6I; quickCheck law6B
>           quickCheck law7I; quickCheck law7B
>           quickCheck law8I; quickCheck law8B
