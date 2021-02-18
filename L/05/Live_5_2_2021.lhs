\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
module Live_5_2_2021 where
import qualified Prelude
import Prelude (Eq, Show, id, (.), map, Int, Integer, iterate, take, error, zipWith, Rational)
import DSLsofMath.FunExp
import DSLsofMath.FunExpInst ()
import DSLsofMath.Algebra
\end{code}

The Ring of polynomials (as lists of coefficients).

\begin{code}
newtype Poly a = Poly [a] deriving (Show,Eq)

evalP :: Ring a => Poly a -> (a -> a)
evalP = error "TODO"

evalL :: Ring a => [a] -> a -> a
evalL = error "TODO"

-- Example polynomials
constP :: a -> Poly a
constP = error "TODO"

xP :: Ring a => Poly a
xP = error "TODO"

xm12 :: Ring a => Poly a
xm12 = (xP-one)^+2          -- (x-1)²

instance Additive a => Additive (Poly a) where
  (+) = addP; zero = zeroP

instance Ring a => Multiplicative (Poly a) where
  (*) = mulP; one = oneP

addP :: Additive a => Poly a -> Poly a -> Poly a
addP (Poly as) (Poly bs) = Poly (addL as bs)

addL :: Additive a => [a]->[a]->[a]
addL = error "TODO"

zeroP :: Poly a
zeroP = error "TODO"

oneP :: Multiplicative a => Poly a
oneP = error "TODO"

mulP :: Ring a => Poly a -> Poly a -> Poly a
mulP (Poly as) (Poly bs) = Poly (mulL as bs)

mulL :: Ring a => [a] -> [a] -> [a]
mulL [] p = []
mulL (a:as) (b:bs) = (a*b) : addL  (scaleL a bs)
                                   (mulL as (b:bs))

scaleL :: Multiplicative a => a -> [a] -> [a]
scaleL = error "TODO"

zipWithLonger :: (a->a->a) -> [a] -> [a] -> [a]
zipWithLonger op [] bs = error "TODO"  -- 0+p == p
zipWithLonger op as [] = error "TODO"  -- p+0 == p
zipWithLonger op (a:as) (b:bs) = error "TODO"

instance AddGroup a => AddGroup (Poly a) where
  negate = negateP

negateP :: AddGroup a => Poly a -> Poly a
negateP (Poly as) = Poly (negateL as)

negateL :: AddGroup a => [a] -> [a]
negateL = map negate
\end{code}

Några algebraiska egenskaper:

  evalL [a]    x = a
  evalL (0:as) x = x*evalL as

  a:as = [a] + 0:as


Power Series

\begin{code}
type PS = Poly

evalPS :: Ring a => Int -> PS a -> a -> a
evalPS n = evalP . takeP n

takeP :: Int -> PS a -> Poly a
takeP n (Poly as) = Poly (take n as)
\end{code}

Spec. of derL:
  forall cs. eval (derL cs) = D (eval cs)

\begin{code}
derP :: Ring a => Poly a -> Poly a
derP (Poly as) = Poly (derL as)

derL :: Ring a => [a] -> [a]
derL []      = error "TODO"
derL (_:as)  = error "TODO"

countUp :: Ring a => a -> [a]
countUp = iterate (one+)
\end{code}

Specification of intL:
  forall c, cs. derL (intL c cs) = cs
\begin{code}
integ :: Field a => a -> Poly a -> Poly a
integ c (Poly cs) = Poly (intL c cs)

intL :: Field a => a -> [a] -> [a]
intL c cs = error "TODO"

expP :: Field a => Poly a
expP = error "TODO"

sinP, cosP :: Field a => Poly a
sinP = error "TODO"  -- sin' =  cos && sin 0 = 0
cosP = error "TODO"  -- cos' = -sin && cos 0 = 1
\end{code}



















\begin{spec}
degree :: Poly a -> Maybe Int
degree []      = Nothing
degree (a:as)  = Just (length as)

mapMaybe :: (a->b) -> (Maybe a -> Maybe b)
mapMaybe f Nothing = Nothing
mapMaybe f (Just x)= Just (f x)

hej :: Num a => PS a
hej = 1 : hej
haj :: Num a => PS a
haj = 0 : map (1+) haj

\end{spec}



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
