First some code from the previous lecture:

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
module Live_6_2_2020 where
import DSLsofMath.FunExp    -- Syntax for functions
import DSLsofMath.Derive    -- Syntactic computation of derivatives
import DSLsofMath.Simplify  -- Algebraic simplification
import Hatlab.Plot

type DS = []
evalAll :: FunExp -> DS Func
evalAll e = eval e : evalAll (d e)
d :: FunExp -> FunExp
d = simplify . derive

mulDS :: Num a => DS a -> DS a -> DS a
mulDS fs@(f:fs') gs@(g:gs') =
  (f*g) : addDS (mulDS fs' gs) (mulDS fs gs')

addDS :: Num a => DS a -> DS a -> DS a
addDS = zipWith (+)
\end{code}

Then code from week 6 lecture 2:

Some test code: |apply c| is a homomorphism from |DS Func| to |DS REAL| and also useful for testing the function lists (streams of derivative).

\begin{code}
apply :: a -> [a->b] -> [b]
apply x = map (\f->f x)

e1s :: DS Func
e1s = evalAll (Exp Id)
e2s :: DS Func
e2s = evalAll (Exp Id :*: Exp Id)
e3s :: DS Func
e3s = mulDS e1s e1s

t :: DS Func -> [REAL]
t = take 6 . apply 0
\end{code}

Now, we have done all of this with streams of functions,
but all the definitions go through also after picking a
particular point using |apply x|. The classical example is
to pick |x=0| to get the "Maclaurin series".  (See, for
example, https://www.khanacademy.org/math/ap-calculus-bc/bc-series-new/bc-10-11/v/maclaurin-and-taylor-series-intuition
)

\begin{code}
newtype DerStream a = DS [a]   -- could be called Maclaurin

instance Num a => Num (DerStream a) where
  (+) = addDerStream
  (*) = mulDerStream
  negate = negateDerStream
  fromInteger = fromIntegerDerStream

addDerStream :: Num a =>
   DerStream a -> DerStream a -> DerStream a
addDerStream (DS xs) (DS ys) = DS (addDS xs ys)

mulDerStream :: Num a =>
   DerStream a -> DerStream a -> DerStream a
mulDerStream (DS xs) (DS ys) = DS (mulDS xs ys)

fromIntegerDerStream :: Num a => Integer -> DerStream a
fromIntegerDerStream i = DS (fromInteger i : repeat 0)

negateDerStream :: Num a => DerStream a -> DerStream a
negateDerStream (DS xs) = DS (map negate xs)
\end{code}

Testing:
\begin{code}
e2 :: FunExp
e2 = Sin Id
fs2 :: DS Func
fs2 = evalAll e2
ys2 :: DerStream REAL
ys2 = DS (apply 0 fs2)
ps2 :: PS REAL
ps2 = ds2ps ys2

plot0 = plot [Fun (sin.(pi*)) "sin"]
plot2 = myplot fs2 "sin"

plotMac n = plot [Fun (sin.(pi*)) "sin",
                  Fun (evalMac ys2 (n+1) . (pi*)) "ys2"]


myplot fs name = plot [ Fun (f.(pi*)) (name ++ "^{("++show i++")}")
                      | (f,i) <- zip fs [0..3]
                      ]


evalMac :: Fractional a => DerStream a -> Int -> a -> a
evalMac dS n = evalPS n (ds2ps dS)

-- derivative stream to power series
ds2ps :: Fractional a => DerStream a -> PS a
ds2ps (DS ds) = PS (zipWith (/) ds factorials)

factorials :: Num a => [a]
factorials = 1 : factorialsFrom 1

factorialsFrom :: Num a => a -> [a]
factorialsFrom n = n : map (n*) (factorialsFrom (n+1))

type Poly a = [a]
evalP :: Num a => [a] -> (a -> a)
evalP []      x = 0
evalP (a0:as) x = a0 + x*evalP as x
\end{code}


\begin{code}
newtype PS a = PS (Poly a)

evalPS :: Num a => Int -> PS a -> (a -> a)
evalPS n (PS as) x = evalP (take n as) x

instance Num a => Num (PS a) where
  PS x + PS y = PS (addP x y)
  PS x * PS y = PS (mulP x y)
  negate (PS x) = PS (negateP x)
  fromInteger i  = PS (fromIntegerP i)

negateP :: Num a => Poly a -> Poly a
negateP = map negate

fromIntegerP :: Num a => Integer -> Poly a
fromIntegerP 0 = []
fromIntegerP i = [fromInteger i]

zipWithLonger :: (a->a->a) -> [a] -> [a] -> [a]
zipWithLonger op [] bs = bs  -- 0+p == p
zipWithLonger op as [] = as  -- p+0 == p
zipWithLonger op (a:as) (b:bs) = op a b : zipWithLonger op as bs

scale :: Num a => a -> Poly a -> Poly a
scale a = map (a*)

mulP :: Num a => [a] -> [a] -> [a]
mulP p [] = []
mulP [] p = []
mulP (a:as) q@(b:bs) = (a*b) : addP (scale a bs) (mulP as q)

addP :: Num a => Poly a -> Poly a -> Poly a
addP as bs = zipWithLonger (+) as bs

x :: Num a => Poly a
x = [0,1]
\end{code}

Not covered in the lecture - just here for reference.

\begin{code}
derP :: (Enum a, Num a) => Poly a -> Poly a
derP [] = []
derP (_:as) = zipWith (*) [1..] as

derPS :: (Enum a, Num a) => PS a -> PS a
derPS (PS x) = PS (derP x)
\end{code}

Specification of intP:
  forall c, cs. derP (intP c cs) = cs
\begin{code}
intP :: (Enum a, Fractional a) => a -> Poly a -> Poly a
intP c cs = c : zipWith (/) cs [1..]

intPS :: (Enum a, Fractional a) => a -> PS a -> PS a
intPS c (PS x) = PS (intP c x)

expPS :: (Enum a, Fractional a) => PS a
expPS = intPS 1 expPS

sinPS, cosPS :: (Enum a, Fractional a) => PS a
sinPS = intPS 0 cosPS      -- sin' = cos && sin 0 = 0
cosPS = intPS 1 (-sinPS)   -- cos' = -sin && cos 0 = 1
\end{code}


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
  recip (PS x) = PS (recipP x)
  PS x / PS y  = PS (divP x y)
\end{code}
