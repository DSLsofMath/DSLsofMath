\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
module Live_5_2_2020 where

type Poly a = [a]
evalP :: Num a => [a] -> (a -> a)
evalP []      x = 0
evalP (a0:as) x = a0 + x*evalP as x

evalPS :: Num a => Int -> PS a -> (a -> a)
evalPS n as x = evalP (take n as) x

-- Example polynomial
p1 = [-1,0,2]   --   -1 + 2*x^2

instance Num a => Num (Poly a) where
  (+) = addP
  (*) = mulP
  negate = negateP
  fromInteger = fromIntegerP

negateP :: Num a => Poly a -> Poly a
negateP = map negate

fromIntegerP :: Num a => Integer -> Poly a
fromIntegerP 0 = []
fromIntegerP i = [fromInteger i]

addP :: Num a => Poly a -> Poly a -> Poly a
addP as bs = zipWithLonger (+) as bs

zipWithLonger :: (a->a->a) -> [a] -> [a] -> [a]
zipWithLonger op [] bs = bs  -- 0+p == p
zipWithLonger op as [] = as  -- p+0 == p
zipWithLonger op (a:as) (b:bs) = op a b : zipWithLonger op as bs

scale :: Num a => a -> Poly a -> Poly a
scale a = map (a*)

mulP p [] = []
mulP [] p = []
mulP (a:as) q@(b:bs) = (a*b) : (scale a bs  +  mulP as q)

x :: Num a => Poly a
x = [0,1]

-- Assuming there are no trailing zeroes
degree :: Poly a -> Maybe Int
degree []      = Nothing
degree (a:as)  = Just (length as)

mapMaybe :: (a->b) -> (Maybe a -> Maybe b)
mapMaybe f Nothing = Nothing
mapMaybe f (Just x)= Just (f x)

type PS a = Poly a
hej :: Num a => PS a
hej = 1 : hej
haj :: Num a => PS a
haj = 0 : map (1+) haj

\end{code}

Några algebraiska egenskaper:

  a:as = [a] + 0:as

eval [a] = const a
eval (0:as) = \x-> x * eval as x = id*eval as
eval (a:as) = eval ([a] + (0:as))
            = eval [a] + id * eval as
            = const a + id * eval as


-- Computing a definition for mulP
  (a:as)*(b:bs)
= ([a] + 0:as)*(b:bs)
= [a]*(b:bs) + (0:as)*(b:bs)
= scale a (b:bs)  +  (0: (as*(b:bs)))
= ((a*b):scale a bs)  +  (0:(as*q))
= ((a*b)+0) : ((scale a bs) + (as*q))
= (a*b) : (scale a bs + as*q)

Spec. of derP:
  forall cs. eval (derP cs) = D (eval cs)

\begin{code}
derP :: (Enum a, Num a) => Poly a -> Poly a
derP [] = []
derP (_:as) = zipWith (*) [1..] as
\end{code}

Specification of intP:
  forall c, cs. derP (intP c cs) = cs
\begin{code}
intP :: (Enum a, Fractional a) => a -> Poly a -> Poly a
intP c cs = c : zipWith (/) cs [1..]

expP :: (Enum a, Fractional a) => PS a
expP = intP 1 expP

sinP, cosP :: (Enum a, Fractional a) => PS a
sinP = intP 0 cosP      -- sin' = cos && sin 0 = 0
cosP = intP 1 (-sinP)   -- cos' = -sin && cos 0 = 1
\end{code}


  zipWith (*) [1..] (tail (intP c cs)) = cs


  zipWith (*) [1..] (tail (intP c cs))
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




intP c [3,2,1] --  I (\x->3+2*x+x^2) = \x->3*x+x^2+(1/3)*x^3+c
 == [c,3,1,1/3]

----------------

Spec. of divP:  divP as bs =
\begin{code}
divP :: Fractional a => Poly a -> Poly a -> Poly a
divP as bs = mulP as (recipP bs)
\end{code}
Spec. of recipP:
  mulP as (recipP as) == 1
Def. of mulP:
  mulP (a:as) q@(b:bs) = (a*b) : (scale a bs  +  mulP as q)

\begin{code}
recipP :: Fractional a => Poly a -> Poly a
recipP [] = error "Div. by zero"
recipP (a:as) = q
  where
    q = b:bs
    b = recip a
    bs = scale (recip (-a)) (mulP as q)
  -- mulP as bs == 1:0:0:0:0:....
  -- (a*b) : (scale a bs  +  mulP as q) == 1:zeros
  -- a*b == 1 &&  scale a bs  +  mulP as q == zeros
  -- bs == scale (recip (-a)) (mulP as q)

instance (Enum a, Fractional a) => Fractional (PS a) where
  recip = recipP
  (/)   = divP
\end{code}
