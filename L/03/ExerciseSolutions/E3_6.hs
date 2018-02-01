-- This example is using some type system features not present in normal Haskell,
-- so we have to enable some extensions to work with numbers on the type level
{-# LANGUAGE GADTs, KindSignatures, DataKinds, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, EmptyCase, FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
import Data.Type.Natural
import Data.List

-- This solution handles more than what the task is asking
-- for, since we actually implement row and col, manually,
-- and keep track of length for rows and columns, to
-- demonstrate how length constraint can be encoded
-- and enforced using the Haskell type system.
-- In an exam, this amount of detail is not required.

-- First we define a type encoding numbers up to some number n:
-- Intuitively, Fin n = {0, .., n-1}
data Fin (n :: Nat) where
  F0 :: Fin (S n)
  FS :: Fin n -> Fin (S n)

-- For debugging, we can define a conversion function to integers
finToInteger :: Fin n -> Integer
finToInteger F0 = 0
finToInteger (FS n) = finToInteger n + 1

-- And a show instance
instance Show (Fin n) where
  show i = show (finToInteger i)

-- An n x m matrix is can be modeled as a function returning an element for
-- an pair of numbers (i, j) where 0 <= i < n and 0 <= j < m.
-- We can express this using the Fin datatype above:
data Matrix a (n :: Nat) (m :: Nat) where
  Matrix :: (Fin n -> Fin m -> a) -> Matrix a n m

-- Similarly for a vector:
data Vector a (n :: Nat) where
  Vector :: (Fin n -> a) -> Vector a n

v0 :: Vector Integer Three
v0 = Vector f
  where f :: Fin Three -> Integer
        f F0 = 5
        f (FS F0) = 7
        f (FS (FS F0)) = 4

-- Extracting a matrix element is just function application
proj :: Fin n -> Fin m -> Matrix a n m -> a
proj i j (Matrix m) = m i j

-- We can get the ith row of a matrix as a vector:
row :: Fin n -> Matrix r n p -> Vector r p
row i (Matrix m) = Vector (m i)

-- Similarly for a column
col :: Fin p -> Matrix r n p -> Vector r n
col p (Matrix m) = Vector $ \(i :: Fin n) -> m i p

-- Projection function for vectors:
projVector :: Fin n -> Vector a n -> a
projVector i (Vector v) = v i

-- To define functions that manipulate all elements
-- of a vector, we define Bounded and Enum instances
-- for Fin.
-- Note that Fin Z is not an instance of Bounded, since
-- the type has no elements.
instance {-# OVERLAPPING #-} Bounded (Fin (S Z)) where
  minBound = F0
  maxBound = F0

instance {-# OVERLAPPING #-} Bounded (Fin n) => Bounded (Fin (S n)) where
  minBound = F0
  maxBound = FS (maxBound :: Fin n)

instance Enum (Fin Z) where
  toEnum x = error "No elements in Fin Z"
  fromEnum x = case x of {}

instance Enum (Fin n) => Enum (Fin (S n)) where
  toEnum 0 = F0
  toEnum i = FS (toEnum $ i - 1)
  fromEnum F0 = 0
  fromEnum (FS n) = fromEnum n + 1

-- This is a synonym for n being a valid type to use as a vector length
-- for the operations below:
type Length n = (Enum (Fin n), Bounded (Fin n))

-- We can also show a vector:
instance (Length n, Show a) => Show (Vector a n) where
  show v = "[" ++ intercalate ", " elts ++ "]"
    where elts = map (\i -> show (projVector i v)) [minBound .. maxBound]

-- Same for matrices:
instance (Length n, Length m, Show a) => Show (Matrix a n m) where
  show m = show $ Vector $ \i -> row i m

-- Two example matrices:
m0 :: Matrix Integer Two Three
m0 = Matrix f
  where f :: Fin Two -> Fin Three -> Integer
        f F0 F0 = 5
        f F0 (FS F0) = 7
        f F0 (FS (FS F0)) = 12
        f (FS F0) F0 = 2
        f (FS F0) (FS F0) = 4
        f (FS F0) (FS (FS F0)) = 3

m1 :: Matrix Integer Three One
m1 = Matrix f
  where f :: Fin Three -> Fin One -> Integer
        f F0 F0 = 8
        f (FS F0) F0 = 17
        f (FS (FS (F0))) F0 = 9

-- We can then define the sum of all vector elements using the Enum instance:
sumVector :: (Length n, Num a) => Vector a n -> a
sumVector v = sum . map (`projVector` v) $ [minBound .. maxBound]

-- If we have two vectors of equal length, we can combine each
-- pair of elements given a function of matching type:
zipVectorsWith :: (Length n, Num a)
               => (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n
zipVectorsWith f v w = Vector $ \i -> projVector i v `f` projVector i w

-- Now we can express matrix multiplication as follows:
mul :: forall r n m p. (Length n, Num r)
    => Matrix r m n -> Matrix r n p -> Matrix r m p
mul a b = Matrix f
  where f :: Fin m -> Fin p -> r
        f i j = sumVector (zipVectorsWith (*) (row i a) (col j b))
