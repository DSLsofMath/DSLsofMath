\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module P1_Algebra where
\end{code}

P1 a)
\begin{code}
class Group g where
  mul  :: g -> g -> g
  one  :: g
  inv  :: g -> g
\end{code}

P1 b)
\begin{code}
data G v where
  Mul :: G v -> G v -> G v
  One :: G v
  Inv :: G v -> G v
  V   :: v -> G v

instance Group (G v) where mul=Mul; one=One; inv=Inv
\end{code}

P1 c)
\begin{code}
type RPos = Double -- Intended to be reals > zero
instance Group RPos    where mul=(*); one=1; inv=recip
instance Group Integer where mul=(+); one=0; inv=negate
\end{code}

P1 d)

The first argument (var) is the assignment function.

\begin{code}
eval :: Group g => (v->g) -> (G v -> g)
eval var = e where
  e (Mul a b)  = mul (e a) (e b)
  e One        = one
  e (Inv a)    = inv (e a)
  e (V v)      = var v
\end{code}

P1 e)
\begin{code}
evalR :: (v->RPos) -> (G v -> RPos)
evalR = eval
evalI :: (v->Integer) -> (G v -> Integer)
evalI = eval
x, y, z, e1, e2, e3 :: G String
[x, y, z] = map V ["x","y", "z"]
e1 = Mul x y
e2 = Inv x
e3 = Mul e1 e2

-- Assignment function for RPos
varR :: String -> RPos
varR "x" = 0.1
varR "y" = 1
varR "z" = 2

-- Assignment function for Integer
varI :: String -> Integer
varI "x" = 1
varI "y" = 2
varI "z" = 3

-- Evaluation of the different example expressions should give (True, True).
tests = ( map (evalR varR) [e1,e2,e3]  ==  [0.1, 10, 1]
        , map (evalI varI) [e1,e2,e3]  ==  [3, -1, 2]
        )
\end{code}
