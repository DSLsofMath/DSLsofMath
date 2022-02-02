{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RebindableSyntax #-}
module DSLsofMath.W05 where
import Prelude hiding (Num(..),(/),(^))
import DSLsofMath.Algebra
type REAL = Double

evalL :: [REAL] -> (REAL -> REAL)
evalL []      = const 0
evalL (a:as)  = const a  +  id * evalL as

newtype Poly a = Poly [a] deriving (Show,Eq)

evalPoly :: Ring a => Poly a -> (a -> a)
evalPoly (Poly [])        _   =  0
evalPoly (Poly (a:as))    x   =  a + x * evalPoly (Poly as) x

instance Additive a => Additive (Poly a) where
  (+)   = addPoly;   zero  = Poly []

addPoly :: Additive a => Poly a -> Poly a -> Poly a
addPoly (Poly xs) (Poly ys) = Poly (addList xs ys)

addList :: Additive a => [a] -> [a] -> [a]
addList = zipWithLonger (+)

zipWithLonger :: (a->a->a) -> ([a] -> [a] -> [a])
zipWithLonger _   []      bs      = bs  -- |0+bs == bs|  
zipWithLonger _   as      []      = as  -- |as+0 == as|
zipWithLonger op  (a:as)  (b:bs)  = op a b : zipWithLonger op as bs

instance AddGroup a => AddGroup (Poly a) where
  negate = negPoly

negPoly :: AddGroup a => Poly a -> Poly a
negPoly = polyMap negate

polyMap :: (a->b) -> (Poly a -> Poly b)
polyMap f (Poly as)   = Poly (map f as)

instance Ring a => Multiplicative (Poly a) where
  (*)   = mulPoly;   one   = Poly [one]

mulPoly :: Ring a => Poly a -> Poly a -> Poly a
mulPoly (Poly xs) (Poly ys) = Poly (mulList xs ys)

mulList :: Ring a => [a] -> [a] -> [a]
mulList  []      _       =  []    -- |0*bs == 0|
mulList  _       []      =  []    -- |as*0 == 0|
mulList  (a:as)  (b:bs)  =  (a * b) :  addList  (scaleList a  bs)
                                                (mulList as   (b:bs))
scaleList :: Multiplicative a => a -> [a] -> [a]
scaleList a = map (a*)

x :: Ring a => Poly a
x = Poly [0,1]

class Monoid' a where
  unit  :: a
  op    :: a -> a -> a

instance Monoid' a => Monoid' (Maybe a) where
  unit  = Just unit
  op    = opMaybe

opMaybe :: Monoid' a => Maybe a -> Maybe a -> Maybe a
opMaybe Nothing    _m         = Nothing    -- |(-Inf) + m  = -Inf|
opMaybe _m         Nothing    = Nothing    -- |m + (-Inf)  = -Inf|
opMaybe (Just m1)  (Just m2)  = Just (op m1 m2)

type PowerSeries a = Poly a   -- finite and infinite lists

evalPS :: Ring a => Int -> PowerSeries a -> (a -> a)
evalPS n as = evalPoly (takePoly n as)

takePoly :: Int -> PowerSeries a -> Poly a
takePoly n (Poly xs) = Poly (take n xs)

instance (Eq a, Field a) => MulGroup (PowerSeries a) where
  (/) = divPS

divPS :: (Eq a, Field a) => PowerSeries a -> PowerSeries a -> PowerSeries a
divPS (Poly as) (Poly bs) = Poly (divL as bs)

divL :: (Eq a, Field a) => [a] -> [a] -> [a]
divL []      _bs     =  []                             -- case |0/q|
divL (0:as)  (0:bs)  =  divL as bs                     -- case |xp/xq|
divL (0:as)  bs      =  0 : divL as bs                 -- case |xp/q|
divL as      [b]     =  scaleList (1 / b) as           -- case |p/c|
divL (a:as)  (b:bs)  =  c : divL (addList as (scaleList (-c) bs)) (b:bs)
                        where c = a/b
divL _       []      = error "divL: division by zero"

ps0, ps1, ps2 :: (Eq a, Field a) => PowerSeries a
ps0  = 1 / (1 - x)                    -- |ps0 == Poly [1, 1, 1, 1, ...]|
ps1  = 1 / (1 - x)^2                  -- |ps1 == Poly [1, 2, 3, 4, ...]|
ps2  = (x^2 - 2 * x + 1) / (x - 1)    -- |ps2 == Poly [-1,1,0]|

example0, example01 :: (Eq a, Field a) => PowerSeries a
example0   = takePoly 10 ps0
example01  = takePoly 10 (ps0 * (1-x))

deriv :: Ring a => Poly a -> Poly a
deriv (Poly as) = Poly (derivL as)

derivL :: Ring a => [a] -> [a]
derivL []      = []
derivL (_:as)  = zipWith (*) oneUp as

oneUp :: Ring a => [a]
oneUp = one : map (one+) oneUp

checkDeriv :: Int -> Bool
checkDeriv n  =  takePoly n (deriv ps0) == takePoly n (ps1 :: Poly Rational)

instance Functor Poly where  fmap = polyMap

instance Ring a => Monoid' (Poly a) where
  unit  = Poly [one]
  op    = (*)

instance Monoid' Integer where
  unit  = 0
  op    = (+)

type Nat = Integer

degree :: (Eq a, Ring a) => Poly a -> Maybe Nat
degree (Poly []) = Nothing
degree (Poly (x:xs)) = mayMax  (if x == zero then Nothing else Just 0)
                               (fmap (1+) (degree (Poly xs)))

mayMax :: Ord a => Maybe a -> Maybe a -> Maybe a
mayMax x        Nothing   = x
mayMax Nothing  (Just d)  = Just d
mayMax (Just a) (Just b)  = Just (max a b)

degreeAlt :: (Eq a,AddGroup a) => Poly a -> Maybe Nat
degreeAlt = mayMaximum . coefIndices

coefIndices :: (Eq a,AddGroup a) => Poly a -> [Nat]
coefIndices (Poly as) = [i | (a,i) <- zip as [1..], a  /= zero]

mayMaximum :: Ord a => [a] -> Maybe a
mayMaximum []      = Nothing
mayMaximum (x:xs)  = mayMax (Just x) (mayMaximum xs)

checkDegree0 = degree (unit :: Poly Integer) == unit
checkDegreeM :: Poly Integer -> Poly Integer -> Bool
checkDegreeM p q = degree (p*q) == op (degree p) (degree q)

