Just for fun: implement |simp| to show the result nicely.

\begin{code}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module P3.Test where
import Test.QuickCheck
import GHC.Generics
import Data.Ratio
import Data.List(sort, nub)
import PS
import P3

type LinComb v = [(v,Rational)] -- side conditions: sorted, unique variable names, no zeros

simp :: E -> LinComb String
simp (C i)      = cons ("", fromInteger i) []
simp (V x)      = [(x, 1)]
simp (N e)      = simpN (simp e)
simp (R e)      = simpR (simp e)
simp (A e1 e2)  = simpA (simp e1) (simp e2)
simp (M e1 e2)  = simpM (simp e1) (simp e2)

simpN :: LinComb v -> LinComb v
simpN = map (\(v,r)->(v,negate r))

simpA :: Ord v => LinComb v -> LinComb v -> LinComb v
simpA xs ys = simpAs (sort (xs++ys))

simpAs [] = []
simpAs [p] = cons p []
simpAs ((v1,r1):(v2,r2):ps) =
   if v1==v2 then  cons (v1,r1+r2) (simpAs          ps )
             else  cons (v1,r1)    (simpAs ((v2,r2):ps))

cons (v,0)  = id
cons p      = (p:)


-- TODO: extend to multinomials
simpM :: LinComb String -> LinComb String -> LinComb String
simpM [] _ = []
simpM _ [] = []
simpM [("", s)] ps = map (\(v,r)->(v,s*r)) ps
simpM ps [("", s)] = map (\(v,r)->(v,s*r)) ps
simpM _ _ = error "simpM: Nonlinearity not yet handled"

simpR :: LinComb String -> LinComb String
simpR [("",s)] = [("",recip s)]
simpR _ = error "simpR: Nonlinearity not yet handled"

main = putStrLn $ unlines $ map showLC $ take 5 $ map simp $ coeff fs

prop_simple :: Ord v => LinComb v -> Bool
prop_simple ps = isSorted ps && isUniq vs
  where vs = map fst ps

isUniq :: Ord a => [a] -> Bool
isUniq xs = xs == nub xs
isSorted :: Ord a => [a] -> Bool
isSorted (x:xs@(x':_)) = x<=x' && isSorted xs
isSorted _ = True

test1 = quickCheck ((prop_simple :: LinComb String -> Bool).simp)

instance Arbitrary E where
  arbitrary = sized arbE
  shrink = genericShrink

deriving instance Generic E

-- data E = A E E | M E E | N E | C Integer | R E | V String

arbE :: Int -> Gen E
arbE n | n <= 1 = smallE
       | otherwise = oneof  [  A <$> arbE n2 <*> arbE n2
-- add later                ,  M <$> arbE n2 <*> arbE n2
                            ,  M <$> constE <*> arbE (n-1)
                            ,  M <$> arbE (n-1) <*> constE
                            ,  N <$> arbE (n-1)
                            ,  R <$> (constE `suchThat` (C 0/=))
                            ]
  where  n2 = n `div` 2

smallE :: Gen E
smallE = oneof [constE, V <$> arbV]

constE :: Gen E
constE = C <$> arbI

arbI :: Gen Integer
arbI = choose (0,7)    -- arbitrary choice;-)
arbV :: Gen String
arbV = elements ("":map (:[]) "abcdef") -- just a few for testing

-- Some pretty-printing:

showLC :: LinComb String -> String
showLC = cutPlus . concatMap showP
  where  cutPlus ('+':s) = s
         cutPlus s = s

showP :: (String,Rational) -> String
showP (v,r) = showCoeff r v

showCoeff :: Rational -> String -> String
showCoeff r s | r > 0 = '+':showC r s
              | otherwise = showC r s
  where showC r s | denominator r == 1 = showN (numerator r) s
                  | otherwise = showN (numerator r) s ++ "/" ++ show (denominator r)
        showN 1    s = s
        showN (-1) s = '-':s
        showN n    s = show n ++ s
\end{code}
