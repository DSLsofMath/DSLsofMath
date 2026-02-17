import Data.List
import qualified Data.Map as Map

type PolynomialSupport a = [(Int , a)] 
-- The idea now is that such a support xs corresponds to sum [ a * x^n | (n,a) <- xs]
-- We only save the non-zero indices, which means we have to store less information. 

fromListSup :: (Num a, Eq a) => [a] -> PolynomialSupport a 
fromListSup as = [ (i , a) | (i ,a ) <- zip [ 0 .. ] as ,  not (a == 0) ]

-- we know how to multiply polynomials if they have only one index as support
-- the problem is that we still need to take sums over things with the same index.

miniMul :: (Num a) => (Int , a) -> (Int , a) -> (Int , a)
miniMul (n , a) (m , b) = (n + m , a * b) 

allTerms :: (Num a) => PolynomialSupport a -> PolynomialSupport a -> [ (Int , a)]
allTerms as bs = [ miniMul a b | a <- as , b <- bs]

combineTerms :: (Num a) => [ (Int , a) ] -> PolynomialSupport a
combineTerms xs = [ (i , product [ b | (j,b) <- xs , j == i ] ) | i <- indexList xs ] where
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
    Just b -> helper (Map.insert i  (a * b) mapSoFar) inputList

mulPolySupportQuickly :: (Num a) => PolynomialSupport a -> PolynomialSupport a -> PolynomialSupport a
mulPolySupportQuickly p q = Map.toList (combineTermsQuickly (allTerms p q))
-- Exercise: perhaps we can be even quicker if we build the "allTerms" listed in a sorted way.


