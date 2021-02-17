\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
module Live_5_2_2021 where
import qualified Prelude
import Prelude (Eq, Show, id, (.), map, Int, Integer, iterate, take, error, zipWith, Rational)
import DSLsofMath.FunExp
import DSLsofMath.FunExpInst ()
import DSLsofMath.Algebra

newtype Poly a = Poly [a] deriving (Show,Eq)

evalP :: Ring a => Poly a -> (a -> a)
evalP (Poly p) = evalL p

evalL :: Ring a => [a] -> a -> a
evalL []      x = zero
evalL (a0:as) x = a0 + x*evalL as x

-- Example polynomials
constP :: a -> Poly a
constP c = Poly [c]

xP :: Ring a => Poly a
xP = Poly [zero, one]

xm12 :: Ring a => Poly a
xm12 = (xP-one)^+2

instance Additive a => Additive (Poly a) where
  (+) = addP; zero = zeroP

instance Ring a => Multiplicative (Poly a) where
  (*) = mulP; one = oneP

addP :: Additive a => Poly a -> Poly a -> Poly a
addP (Poly as) (Poly bs) = Poly (addL as bs)

addL :: Additive a => [a]->[a]->[a]
addL = zipWithLonger (+)

zeroP :: Poly a
zeroP = Poly []

oneP :: Multiplicative a => Poly a
oneP = Poly [one]

mulP :: Ring a => Poly a -> Poly a -> Poly a
mulP (Poly as) (Poly bs) = Poly (mulL as bs)

-- mulL as bs = error "TODO"
mulL :: Ring a => [a] -> [a] -> [a]
mulL p [] = []
mulL [] p = []
mulL (a:as) (b:bs) = (a*b) : addL (scaleL a bs)
                                  (mulL as (b:bs))

-- scaleL :: Num a => a -> Poly a -> Poly a
scaleL :: Multiplicative a => a -> [a] -> [a]
scaleL a = map (a*)

instance AddGroup a => AddGroup (Poly a) where
  negate = negateP

negateP :: AddGroup a => Poly a -> Poly a
negateP (Poly as) = Poly (negateL as)

negateL :: AddGroup a => [a] -> [a]
negateL = map negate

zipWithLonger :: (a->a->a) -> [a] -> [a] -> [a]
zipWithLonger op [] bs = bs  -- 0+p == p
zipWithLonger op as [] = as  -- p+0 == p
zipWithLonger op (a:as) (b:bs) = op a b : zipWithLonger op as bs
\end{code}

\begin{spec}
degree :: Poly a -> Maybe Int
degree []      = Nothing
degree (a:as)  = Just (length as)

mapMaybe :: (a->b) -> (Maybe a -> Maybe b)
mapMaybe f Nothing = Nothing
mapMaybe f (Just x)= Just (f x)

type PS a = Poly a

evalPS :: Num a => Int -> PS a -> (a -> a)
evalPS n as x = evalL (take n as) x

hej :: Num a => PS a
hej = 1 : hej
haj :: Num a => PS a
haj = 0 : map (1+) haj

\end{spec}

Några algebraiska egenskaper:

  a:as = [a] + 0:as

eval [a] = const a
eval (0:as) = \x-> x * eval as x = id*eval as
eval (a:as) = eval ([a] + (0:as))
            = eval [a] + id * eval as
            = const a + id * eval as


-- Computing a definition for mulL
  (a:as)*(b:bs)
= ([a] + 0:as)*(b:bs)
= [a]*(b:bs) + (0:as)*(b:bs)
= scale a (b:bs)  +  (0: (as*(b:bs)))
= ((a*b):scale a bs)  +  (0:(as*q))
= ((a*b)+0) : ((scale a bs) + (as*q))
= (a*b) : (scale a bs + as*q)

Spec. of derL:
  forall cs. eval (derL cs) = D (eval cs)

\begin{code}
derP :: Ring a => Poly a -> Poly a
derP (Poly as) = Poly (derL as)

derL :: Ring a => [a] -> [a]
derL []      = []
derL (_:as)  = zipWith (*) (countUp one) as

countUp :: Ring a => a -> [a]
countUp = iterate (one+)
\end{code}

Specification of intL:
  forall c, cs. derL (intL c cs) = cs
\begin{code}
integ :: Field a => a -> Poly a -> Poly a
integ c (Poly cs) = Poly (intL c cs)

intL :: Field a => a -> [a] -> [a]
intL c cs = c : zipWith (/) cs (countUp one)

expP :: Field a => Poly a
expP = integ one expP

sinP, cosP :: Field a => Poly a
sinP = integ zero cosP            -- sin' =  cos && sin 0 = 0
cosP = integ one  (negate sinP)   -- cos' = -sin && cos 0 = 1

type PS = Poly
takeP :: Int -> PS a -> Poly a
takeP n (Poly as) = Poly (take n as)

evalPS :: Ring a => Int -> PS a -> a -> a
evalPS n = evalP . takeP n

\end{code}


  zipWith (*) [1..] (tail (intL c cs)) = cs


  zipWith (*) [1..] (tail (intL c cs))
=
  zipWith (*) [1..] (tail (c : zipWith (/) cs [1..]))
=
  zipWith (*) [1..] (zipWith (/) cs [1..])
= let cs = x:xs
  zipWith (*) [1..] ((x/1) : zipWith (/) xs [2..])
=
  (x*1) : zipWith (*) [2..] (zipWith (/) xs [2..])
=
  x : (2*(x2/2)) : zipWith (*) [3..] (zipWith (/) xs2 [3..])




intL c [3,2,1] --  I (\x->3+2*x+x^2) = \x->3*x+x^2+(1/3)*x^3+c
 == [c,3,1,1/3]

----------------
