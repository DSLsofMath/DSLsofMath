\begin{code}
module DSLsofMath.Live_8_2 where
-- import DSLsofMath.W05
-- import DSLsofMath.W06
-- import DSLsofMath.W08
\end{code}


exp [1/1, 1/1, 1/2 ,  1/6, 1/24, ...

sin [0/1, 1/1, 0   , -1/6, 0   , ...
cos [1/1, 0/1,-1/2 , 0/1 , 1/24, ...

\begin{spec}
expx :: Fractional a => PowerSeries a
expx = integ 1 expx
\end{spec}

and approximated by

\begin{spec}
expf :: Fractional a => a -> a
expf = eval 20 expx
\end{spec}


expf i = C (0.5403023058681398,0.8414709848078965)
                      sinf 1 = 0.8414709848078965
  cosf 1 =  0.5403023058681398



\begin{code}
newtype Complex r = C (r , r)
  deriving (Eq, Show)

i :: Num a => Complex a
i = C (0, 1)

instance Num r => Num (Complex r) where
  (+) = addC
  (*) = mulC
  fromInteger = fromIntegerC

addC :: Num a => Complex a -> Complex a -> Complex a
addC (C (x1,y1)) (C (x2,y2)) = C (x1+x2,y1+y2)

fromIntegerC :: Num a => Integer -> Complex a
fromIntegerC i = C (fromInteger i,0)

mulC = error "TBD"
\end{code}
  1 + i  :: Complex Double
=
  fromInteger 1    +   i
=
  fromIntegerC 1   +   i
=
  C (fromInteger 1,fromInteger 0) + C (fromInteger 0, fromInteger 1)
=
  C (1.0, 0.0) + C (0.0, 1.0)
=
  addC (C (1.0, 0.0)) (C (0.0, 1.0))
=
  addC (C (1.0, 0.0)) (C (0.0, 1.0))
=
  let x1=1.0; y1=0.0; x2=0.0; y2=1.0
  in C (x1+x2,y1+y2)
=
  C (1.0+0.0,0.0+1.0)
=
  C (1.0,1.0)
