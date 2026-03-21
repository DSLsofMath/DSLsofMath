\begin{code}
{-# LANGUAGE GADTs #-}
module Solution_202603_Algebra where
import Prelude (String, Bool(..), Integer, (+), (*), (&&), (||), error, (==), map, Show)
-- More imports for the tests
import Prelude (IO, putStrLn, Eq)
import Test.QuickCheck
\end{code}

[25p] \textbf{Algebraic structure: a DSL for semirings}

* a) [5p] Define a type class |Semiring|.

\begin{code}
class Semiring a where
  add  :: a -> a -> a
  mul  :: a -> a -> a
  zero :: a
  one  :: a
\end{code}

* b) [5p] Define a datatype |SR v| and its |Semiring| instance.

\begin{code}
data SR v where
  Add  :: SR v -> SR v -> SR v
  Mul  :: SR v -> SR v -> SR v
  Zero :: SR v
  One  :: SR v
  V    :: v -> SR v
 deriving Show

instance Semiring (SR v) where add = Add; mul  = Mul; zero = Zero; one  = One
\end{code}

* c) [5p] Find and implement two other instances of the |Semiring| class.

\begin{code}
instance Semiring Bool     where add = (||);  mul = (&&);  zero = False;  one  = True
instance Semiring Integer  where add = (+);   mul = (*);   zero = 0;      one  = 1
\end{code}

* d) [5p] Give a type signature for, and define, a general evaluator.

\begin{code}
evalSR :: Semiring a => (v -> a) -> SR v -> a
evalSR env = e where
  e Zero       =  zero
  e One        =  one
  e (Add x y)  =  add (e x) (e y)
  e (Mul x y)  =  mul (e x) (e y)
  e (V v)      =  env v
\end{code}

* e) [5p] Specialise the evaluator and compute results.

\begin{code}
evalB :: (v -> Bool) -> SR v -> Bool
evalB = evalSR

evalI :: (v -> Integer) -> SR v -> Integer
evalI = evalSR

-- Three expressions
e1, e2, e3 :: SR String
e1 = V "x"
e2 = add e1 one
e3 = mul e2 (V "y")

-- Assignments
envB :: String -> Bool
envB "x" = True
envB "y" = False
envB _   = error "Unbound variable"

envI :: String -> Integer
envI "x" = 2
envI "y" = 3
envI _   = error "Unbound variable"

-- Hand-computation of results expected on the exam:
-- evalB on e1: True
-- evalB on e2: True || True = True
-- evalB on e3: True && False = False

-- evalI on e1: 2
-- evalI on e2: 2 + 1 = 3
-- evalI on e3: 3 * 3 = 9

-- Testing code to verify expected results
resB :: Bool
resB = map (evalB envB) [e1, e2, e3] == [True, True, False]

resI :: Bool
resI = map (evalI envI) [e1, e2, e3] == [2, 3, 9]

--------------------------------------------------------------------------------
-- Some of the properties + QuickCheck tests (not required on the exam)
--------------------------------------------------------------------------------

prop_add_zero :: (Eq a, Semiring a) => a -> Bool
prop_add_zero a = add a zero == a

prop_add_comm :: (Eq a, Semiring a) => a -> a -> Bool
prop_add_comm a b = add a b == add b a

prop_mul_assoc :: (Eq a, Semiring a) => a -> a -> a -> Bool
prop_mul_assoc a b c = mul (mul a b) c == mul a (mul b c)

prop_dist_left :: (Eq a, Semiring a) => a -> a -> a -> Bool
prop_dist_left a b c = mul a (add b c) == add (mul a b) (mul a c)

prop_mul_zero :: (Eq a, Semiring a) => a -> Bool
prop_mul_zero a = mul a zero == zero

main :: IO ()
main = do
  putStrLn "* Checking Semiring Bool"
  quickCheck (prop_add_zero  :: Bool -> Bool)
  quickCheck (prop_add_comm  :: Bool -> Bool -> Bool)
  quickCheck (prop_mul_assoc :: Bool -> Bool -> Bool -> Bool)
  quickCheck (prop_dist_left :: Bool -> Bool -> Bool -> Bool)
  quickCheck (prop_mul_zero  :: Bool -> Bool)

  putStrLn "* Checking Semiring Integer"
  quickCheck (prop_add_zero  :: Integer -> Bool)
  quickCheck (prop_add_comm  :: Integer -> Integer -> Bool)
  quickCheck (prop_mul_assoc :: Integer -> Integer -> Integer -> Bool)
  quickCheck (prop_dist_left :: Integer -> Integer -> Integer -> Bool)
  quickCheck (prop_mul_zero  :: Integer -> Bool)
\end{code}
