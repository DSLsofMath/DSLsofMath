import Control.Applicative (liftA2)
import qualified Data.Map.Strict as Map

type Monomial a = (Int, a)
type PolynomialSupport a = [Monomial a]
newtype Poly a = Poly { unPoly :: PolynomialSupport a }
  deriving (Show)

-- 0. Equality and Properties
canonicalize :: (Num a, Eq a) => PolynomialSupport a -> PolynomialSupport a
canonicalize =
  filter ((0/=) . snd) .  -- remove zero monomials
  Map.toAscList .         -- extract a list sort by the keys
  Map.fromListWith (+)    -- combine coefficients with equal keys (exponents)

instance (Num a, Eq a) => Eq (Poly a) where
  (Poly p1) == (Poly p2) = canonicalize p1 == canonicalize p2

-- Specification: H2(eval, mulPoly, (*))
propMulCorrect :: (Num a, Eq a) => Poly a -> Poly a -> a -> Bool
propMulCorrect p q x =
    eval (mulPoly p q) x == (eval p x * eval q x)

-- 2. Core Utilities
evalMono :: (Num a) => Monomial a -> (a -> a)
evalMono (n, coeff) x = coeff * (x ^ n)

eval :: (Num a) => Poly a -> a -> a
eval (Poly ms) x = sum [evalMono m x | m <- ms ]

fromListSup :: (Num a, Eq a) => [a] -> PolynomialSupport a
fromListSup as = [ (i, a) | (i, a) <- zip [ 0 .. ] as, a /= 0 ]

-- 3. Multiplication Logic
-- Multiplies two monomials (single terms)
monoMul :: (Num a) => (Int, a) -> (Int, a) -> (Int, a)
monoMul (n, a) (m, b) = (n + m, a * b)

-- The "cross product" of all terms
allTerms :: (Num a) => [Monomial a] -> [Monomial a] -> [Monomial a]
allTerms = liftA2 monoMul
-- Perhaps more readable:
--   allTerms as bs = [ miniMul a b | a <- as , b <- bs]

mulPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
mulPoly (Poly p) (Poly q) = Poly (canonicalize (allTerms p q))

-- Some test data

p1, p2, p3, p4 :: (Enum a, Num a, Eq a) => Poly a
p1 = Poly [(17,3), (38, 5)]
p2 = Poly [(1,7),(3,8)]
p3 = Poly (fromListSup [1..100])
p4 = Poly (fromListSup (reverse [1..1000]))

main = do
  print (all (propMulCorrect p1 p2) [1,2,5])
  print (all (propMulCorrect p1 p3) [1,2,5])
  print (all (propMulCorrect p2 p3) [1,2,5])
  print (all (propMulCorrect p3 p4) [1..10])
