-- Problem 2: Multiplication for matrices (from the matrix algebra DSL).

module P2 where

-- 2a: Type the variables in the text.
-- (This answer uses Agda syntax, but that is not required.)

postulate Nat : Set
postulate V : Nat -> Set -> Set
postulate Fin : Nat -> Set

Op : Set -> Set
Op a = a -> a -> a

postulate sum : {n : Nat} {a : Set} -> Op a -> V n a -> a
postulate zipWith : {n : Nat} {a : Set} -> Op a -> V n a -> V n a -> V n a

data M (m n : Nat) (a : Set) : Set where
  matrix : (Fin m -> Fin n -> a) -> M m n a

record Dummy (a : Set) : Set where
  field
    m : Nat
    n : Nat
    A : M m n a
    p : Nat
    B : M n p a
    i : Fin m
    j : Fin p

-- 2b: Type |mul| and |proj|

postulate
  proj : {m n : Nat} {a : Set} -> Fin m -> Fin n -> M m n a -> a

mul  : {m n p : Nat} {a : Set} -> Op a -> Op a ->
       M m n a -> M n p a -> M m p a

-- 2c: Implement |mul|.

postulate
  row : {m n : Nat} {a : Set} -> Fin m -> M m n a -> V n a
  col : {m n : Nat} {a : Set} -> Fin n -> M m n a -> V m a

mul addE mulE A B = matrix (\i j ->
                      sum addE (zipWith mulE (row i A) (col j B)))
