import Data.List
import qualified Data.Map as Map

type PolynomialSupport a = [(Int , a)] 
-- The idea now is that such a support xs corresponds to sum [ a * x^n | (n,a) <- xs]
-- We only save the non-zero indices, which means we have to store less information. 

eval :: (Num a) => PolynomialSupport a -> a -> a
eval poly x = sum [ coeff * (x ^ n) | (n, coeff) <- poly ]

fromListSup :: (Num a, Eq a) => [a] -> PolynomialSupport a 
fromListSup as = [ (i , a) | (i ,a ) <- zip [ 0 .. ] as ,  not (a == 0) ]

-- we know how to multiply polynomials if they have only one index as support
-- the problem is that we still need to take sums over things with the same index.

miniMul :: (Num a) => (Int , a) -> (Int , a) -> (Int , a)
miniMul (n , a) (m , b) = (n + m , a * b) 

allTerms :: (Num a) => PolynomialSupport a -> PolynomialSupport a -> [ (Int , a)]
allTerms as bs = [ miniMul a b | a <- as , b <- bs]

combineTerms :: (Num a) => [ (Int , a) ] -> PolynomialSupport a
combineTerms xs = [ (i , sum [ b | (j,b) <- xs , j == i ] ) | i <- indexList xs ] where
  indexList :: [ (Int , a) ] -> [Int]
  indexList = nub . map fst 

mulPolySupport :: (Num a) => PolynomialSupport a -> PolynomialSupport a -> PolynomialSupport a
mulPolySupport xs ys = combineTerms $ allTerms xs ys 
-- Note that while this works, it recurses over the list many times. Perhaps we can be quicker with an aggregator (which we distinguish by using Map).

combineTermsQuickly :: (Num a) => [(Int , a)] -> Map.Map Int a
combineTermsQuickly xs = helper Map.empty xs where
  helper :: (Num a) => Map.Map Int a -> [(Int , a)] -> Map.Map Int a 
  helper mapSoFar [] = mapSoFar
  helper mapSoFar ((i , a) : inputList) = case Map.lookup i mapSoFar of 
    Nothing -> helper (Map.insert i  a mapSoFar) inputList
    Just b -> helper (Map.insert i  (a + b) mapSoFar) inputList

mulPolySupportQuickly :: (Num a) => PolynomialSupport a -> PolynomialSupport a -> PolynomialSupport a
mulPolySupportQuickly p q = Map.toList (combineTermsQuickly (allTerms p q))
-- Exercise: perhaps we can be even quicker if we build the "allTerms" listed in a sorted way.


-- Some properties and helpers
newtype Poly a = Poly { unPoly :: PolynomialSupport a }

mulPoly p q = Poly $ canonicalize $ allTerms (unPoly p) (unPoly q)


instance (Num a, Eq a) => Eq (Poly a) where
  (Poly p1) == (Poly p2) = canonicalize p1 == canonicalize p2

canonicalize :: (Num a, Eq a) => PolynomialSupport a -> PolynomialSupport a
canonicalize =
    filter (\(_, coeff) -> coeff /= 0) .  -- remove zero coefficients
    Map.toAscList .                       -- extract list sorted by exponent
    Map.fromListWith (+)                  -- merge duplicate exponents by adding coeffs
    
-- H2(eval, mulPolySupportQuickly, (*))
propMulCorrect :: (Num a, Eq a) => PolynomialSupport a -> PolynomialSupport a -> a -> Bool
propMulCorrect p q x = 
    eval (mulPolySupportQuickly p q) x == (eval p x * eval q x)

p1, p2, p3, p4 :: (Enum a, Num a, Eq a) => PolynomialSupport a
p1 = [(17,3), (38, 5)]
p2 = [(1,7),(3,8)]
p3 = fromListSup [1..100]
p4 = fromListSup (reverse [1..1000])
