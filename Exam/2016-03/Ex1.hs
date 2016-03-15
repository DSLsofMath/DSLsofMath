{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Ex1 where
import Test.QuickCheck

-- i.  Define a type class `Lattice` that corresponds to the lattice
-- structure.

class Lattice a where
  sup :: a -> a -> a
  inf :: a -> a -> a

-- ii. Define a datatype for the language of lattice expressions and
-- define a `Lattice` instance for it.

data L v   = V v
           | Sup (L v) (L v)
           | Inf (L v) (L v)
  deriving (Eq)

instance Lattice (L v) where
  sup = Sup
  inf = Inf

-- iii.  Find two other instances of the `Lattice` class.

instance Lattice Bool where
  sup = (||)
  inf = (&&)

instance Lattice Integer where
  sup = max
  inf = min

newtype Op a = Op a
  deriving (Eq, Show, Arbitrary)

instance Lattice a => Lattice (Op a) where
  sup (Op x) (Op y) = Op $ inf x y
  inf (Op x) (Op y) = Op $ sup x y

-- iv.  Define a general evaluator for `Lattice` expressions on the
-- basis of an assignment function.

eval :: Lattice a => (v -> a) -> L v -> a
eval f (V v)      = f v
eval f (Sup x y)  = sup (eval f x) (eval f y)
eval f (Inf x y)  = inf (eval f x) (eval f y)

-- v.  Specialise the evaluator to the two `Lattice` instances defined
-- at point iii.  Take three lattice expressions, give the appropriate
-- assignments and compute the results of evaluating, in each case,
-- the three expressions.

evalBool :: (v -> Bool) -> L v -> Bool
evalBool = eval

evalInteger :: (v -> Integer) -> L v -> Integer
evalInteger = eval

l1 x y z = (x ∧ y) ∨ z
l2 x y z = x ∨ (y ∧ z)
l3 x y z = l1 x y z ∧ l2 x y z

q1, q2, q3 :: L String
q1 = l1 (V "x") (V "F") (V "T")
q2 = l2 (V "x") (V "T") (V "x")
q3 = l3 (V "x") (V "T") (V "F")

assBool :: String -> Bool
assBool "x" = True
assBool "F" = False
assBool "T" = True

assInteger :: String -> Integer
assInteger "x" = 17
assInteger "F" = 38
assInteger "T" = 1738

{-
     Bool     Integer     Lattice
q1   True     1738        (x ∧ F) ∨ T
q2   True     17          x ∨ (F ∧ x)
q3   True     38          ((x ∧ T) ∨ F) ∧ (x ∨ (T ∧ F))

Bool:
  (x ∧ F) ∨ T = (T ∧ F) ∨ T = F ∨ T = T
  x ∨ (F ∧ x) = T ∨ (F ∧ T) = T ∨ F = T
  ((x ∧ T) ∨ F) ∧ (x ∨ (T ∧ F))
    = ((T ∧ T) ∨ F) ∧ (T ∨ (T ∧ F))
    = (T ∨ F) ∧ (T ∨ F) = T ∧ T = T
Integer:
  (x ∧ F) ∨ T = (17 ∧ 38) ∨ 1738 = 17 ∨ 1738 = 1738
  x ∨ (F ∧ x) = 17 ∨ (38 ∧ 17) = 17 ∨ 17 = 17
  ((x ∧ T) ∨ F) ∧ (x ∨ (T ∧ F))
    = ((17 ∧ 1738) ∨ 38) ∧ (17 ∨ (1738 ∧ 38))
    = (17 ∨ 38) ∧ (17 ∨ 38) = 38 ∧ 38 = 38
-}

----------------------------------------------------------------
-- Not part of the exam question:

test1 = map (eval assInteger) [q1, q2, q3]  == [1738, 17, 38]
test2 = map (eval assBool)    [q1, q2, q3]  == [True, True, True]

main = do print test1
          print test2

instance (Show v) => Show (L v) where
  show = showL show

showL :: (v->String) -> (L v -> String)
showL sv (V v)      = sv v
showL sv (Sup x y)  = showSup (showL sv x) (showL sv y)
showL sv (Inf x y)  = showInf (showL sv x) (showL sv y)

showSup :: String -> String -> String
showSup xs ys = xs ++ '∨' : ys
showInf :: String -> String -> String
showInf xs ys = xs ++ '∧' : ys

(∨), (∧) :: Lattice a => a -> a -> a
(∨) = sup
(∧) = inf

assocSup :: (Eq a, Lattice a) => a -> a -> a -> Bool
assocSup x y z =  (x ∨ y) ∨ z == x ∨ (y ∨ z)

assocInf :: (Eq a, Lattice a) => a -> a -> a -> Bool
assocInf x y z =  (x ∧ y) ∧ z == x ∧ (y ∧ z)

commSup x y =  x ∨ y == y ∨ x
commInf x y =  x ∧ y == y ∧ x

abs1 x y =  x ∨ (x ∧ y) == x
abs2 x y =  x ∧ (x ∨ y) == x

data Dummy a = Dummy

testAll :: (Show a, Eq a, Arbitrary a, Lattice a) => Dummy a -> IO ()
testAll (Dummy :: Dummy a) = do
  quickCheck (assocSup ::  a -> a -> a -> Bool)
  quickCheck (assocInf ::  a -> a -> a -> Bool)
  quickCheck (commSup  ::  a -> a -> Bool)
  quickCheck (commInf  ::  a -> a -> Bool)
  quickCheck (abs1     ::  a -> a -> Bool)
  quickCheck (abs2     ::  a -> a -> Bool)

tests = do testAll (Dummy :: Dummy Bool)
           testAll (Dummy :: Dummy Integer)
           testAll (Dummy :: Dummy (Op Bool))
           testAll (Dummy :: Dummy (Op Integer))
