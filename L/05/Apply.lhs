Some "left-overs" from Week 4.

\begin{code}
module Live_5_1a_2020 where
import DSLsofMath.FunExp
import DSLsofMath.FunNumInst
newtype FD a = FD (a->a, a->a)   -- Function + Derivative
newtype Bi a = Bi (a, a) deriving (Eq, Show)  -- Place + Speed

-- forall c. H2(applyFD c, mulFD, mulBi)
applyFD :: a -> FD a -> Bi a
applyFD c (FD (f, f')) = Bi (f c, f' c)

e1 :: FD REAL
e1 = FD (\x -> (x-1)^2, \x -> 2*(x-1))

idFD :: FD REAL
idFD = FD (id, 1)

e2 :: Num a => a -> a
e2 x = (x-1)^2

test2 = let fd = e2 idFD
        in map (\c-> applyFD c fd) [-1..2]

idBi :: Num a => a -> Bi a
idBi c = Bi (c, 1)


instance Num a => Num (FD a) where
  (+) = addFD; (*) = mulFD; negate = negateFD; fromInteger = fromIntegerFD

addFD :: Num a => FD a -> FD a -> FD a
addFD (FD (f, f')) (FD (g, g')) = FD (f+g, f'+g')

mulFD :: Num a => FD a -> FD a -> FD a
mulFD (FD (f, f')) (FD (g, g')) = FD (f*g, f*g' + f'*g)

-- unary minus
negateFD :: Num a => FD a -> FD a
negateFD (FD (f, f')) = FD (negate f, negate f')

fromIntegerFD :: Num a => Integer -> FD a
fromIntegerFD c = FD (const (fromInteger c), const 0)

absFD :: Num a => FD a -> FD a  -- Not really defined at 0
absFD (FD (f, f')) = FD (abs f, signum f * f')

signumFD :: Num a => FD a -> FD a  -- Not really defined at 0
signumFD (FD (f, f')) = FD (signum f, 0)


instance Num a => Num (Bi a) where
  (*) = mulBi; (+) = addBi; negate = negateBi
  fromInteger = fromIntegerBi; abs = absBi; signum = signumBi

-- Spec.: forall c. H2(applyFD c, mulFD, mulBi)
mulBi :: Num a => Bi a -> Bi a -> Bi a
mulBi (Bi (f,f')) (Bi (g,g')) = Bi (f*g, f*g' + f'*g)

addBi :: Num a => Bi a -> Bi a -> Bi a
addBi (Bi (f,f')) (Bi (g,g')) = Bi (f+g, f'+g')

negateBi :: Num a => Bi a -> Bi a
negateBi (Bi (f,f')) = Bi (negate f, negate f')

fromIntegerBi :: Num a => Integer -> Bi a
fromIntegerBi i = Bi (fromInteger i, fromInteger 0)

absBi :: Num a => Bi a -> Bi a
absBi (Bi (f, f')) = Bi (abs f, signum f * f')

signumBi :: Num a => Bi a -> Bi a
signumBi (Bi (f, f')) = Bi (signum f, 0)

\end{code}

H2(applyFD c, mulFD, mulBi)
forall fd1, fd2. applyFD c (mulFD fd1 fd2) == mulBi (applyFD c fd1) (applyFD c fd2)
-- steg 1: expandera fd1 = FD (f, f') och fd2 = FD (g, g')
  mulBi (applyFD c fd1) (applyFD c fd2)
== -- expandera fd1, fd2
  mulBi (applyFD c (FD (f, f'))) (applyFD c (FD (g, g')))
== -- def. av applyFD
  mulBi (Bi (f c, f' c)) (Bi (g c, g' c))
== -- nya namn x = f c, x' = f' x, y = g c, y' g' c
  mulBi (Bi (x, x')) (Bi (y, y'))
== -- Sista steget: definiera mulBI så att detta gäller
  Bi (x*y, x*y' + x'*y)
== -- nya namn enligt ovan
  Bi ((f c)*(g c), (f c)*(g' c) + (f' c)*(g c))
== -- def. (*), (+) för funktioner
  Bi ((f*g) c, (f*g' + f'*g) c)
== -- def. av applyFD
  applyFD c (FD (f*g, f*g' + f'*g))
== -- def. mulFD
  applyFD c (mulFD (FD (f, f') (g, g'))
== -- exp.
  applyFD c (mulFD fd1 fd2)
