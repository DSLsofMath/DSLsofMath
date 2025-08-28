An implementation of power series.
\begin{code}
module PS where

newtype PS r = PS [r]
  deriving Show

addPS :: Num r => PS r -> PS r -> PS r
addPS (PS as) (PS bs) = PS (addL as bs)

mulPS :: Num r => PS r -> PS r -> PS r
mulPS (PS as) (PS bs) = PS (mulL as bs)

mapPS :: (a->b) -> PS a -> PS b
mapPS f (PS as) = PS (map f as)

addL :: Num r => [r] -> [r] -> [r]
addL = zipWithLonger (+)

zipWithLonger :: (a->a->a) -> [a] -> [a] -> [a]
zipWithLonger op [] bs = bs
zipWithLonger op as [] = as
zipWithLonger op (a:as) (b:bs) = op a b : zipWithLonger op as bs

mulL [] bs = []
mulL as [] = []
mulL (a:as) bs = addL (scaleL a bs) (shiftL (mulL as bs))
-- mulL (a:as) (b:bs) = (a*b) : addL (scaleL a bs) (mulL as (b:bs))

scaleL :: Num r => r -> [r] -> [r]
scaleL a = map (a*)

shiftL :: Num r => [r] -> [r]
shiftL = (0:)

instance Num r => Num (PS r) where
  (+) = addPS
  (*) = mulPS
  negate = mapPS negate
  fromInteger = PS . (:[]) . fromInteger

instance (Fractional r) => Fractional (PS r) where
  (/) = divPS
  fromRational = PS . (:[]) . fromRational

x :: Num a => PS a
x = PS [0,1]

divPS  :: (Fractional r) => PS r  -> PS r  -> PS r
divL   :: (Fractional r) => [r]   -> [r]   -> [r]

divPS (PS x) (PS y) = PS (divL x y)

divL (a:as) (b:bs) = let  c  = a/b
                          cs = divL (addL as (scaleL (-c) bs)) (b:bs)
                     in c : cs

takePS n (PS cs) = PS (take n cs)
\end{code}

\begin{code}
integ :: (Fractional r) => r -> PS r -> PS r
integ a0 (PS as) = PS (a0 : zipWith (/) as countUp)

countUp :: Fractional a => [a]
countUp = map fromInteger [1..]
\end{code}


Approximate equality (small difference in coefficients).
\begin{code}
(~=) :: (Num a, Ord a, Fractional a) => PS a -> PS a -> Bool
p ~= q = smallCoeffs (p-q)

smallCoeffs :: (Ord a, Fractional a) => PS a -> Bool
smallCoeffs (PS cs) = all smallCoeff cs

smallCoeff :: (Ord a, Fractional a) => a -> Bool
smallCoeff c = abs c < eps
  where eps = 1e-10
\end{code}

instance (Eq a, Num a) => Eq (PS a) where (==) = eqPS

eqPS :: (Eq a, Num a) => PS a -> PS a -> Bool
eqPS p q = eqZeroPS (p-q)

eqZeroPS :: (Eq a, Num a) => PS a -> Bool
eqZeroPS (PS xs) = eqZeroL xs

eqZeroL [] = True
eqZeroL (c:cs) = 0==c && eqZeroL cs
