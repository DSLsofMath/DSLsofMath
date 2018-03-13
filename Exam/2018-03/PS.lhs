
An implementation of power series.

\begin{code}
module PS where
-- import Test.QuickCheck

newtype PS r = PS {coeff :: [r]}
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

instance (Eq r, Fractional r) => Fractional (PS r) where
  (/) = divPS
  fromRational = PS . (:[]) . fromRational

x :: Num a => PS a
x = PS [0,1]

divPS  :: (Eq r, Fractional r) => PS r  -> PS r  -> PS r
divL   :: (Eq r, Fractional r) => [r]   -> [r]   -> [r]

divPS (PS x) (PS y) = PS (divL x y)

divL (a:as) (b:bs) = let  c  = a/b
                          cs = divL (addL as (scaleL (-c) bs)) (b:bs)
                     in c : cs

takePS n (PS cs) = PS (take n cs)

\end{code}

\begin{code}

[]   *?  q   = []
p    *?  []  = []
[a]  *?  q   = map (a*) q
p    *?  [b] = map (*b) p
(0:p)*?  q   = 0: (p*?q)
p    *?(0:q) = 0: (p*?q)
(a:as)*?q@(b:bs) = (a*b) : (map (a*) bs +? (as*?q))

[]      +?  q       = q
p       +?  []      = p
(a:as)  +?  (b:bs)  = (a+b) : (as +? bs)

prop1 p q = mulL p q == p *? q

dist p q r =  (p+?q)*?r == (p*?r)+?(q*?r)


integ :: (Fractional r) => PS r -> r -> PS r
integ (PS as) a0 = PS (a0 : zipWith (/) as countUp)

countUp :: Fractional a => [a]
countUp = map fromInteger [1..]
\end{code}
