
An implementation of power series.

\begin{code}
module PS where
import Test.QuickCheck

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

Derivation of one of the |divL| cases:

Spec.:   as /' bs = cs    <=>    as = cs *' bs

Substitute as := a:as and cs:=c:cs

  a:as
= spec.
  (c:cs) *' bs
= def. *'
  scaleL c bs +' shiftL (cs *' bs)
= substitute bs:=b:bs
  scaleL c (b:bs) +' shiftL (cs *' (b:bs))
= scale+shift lemma
  (c*b) : (scaleL c bs  +'  cs *' (b:bs))

Componentwise equality:

  a  = c*b
  as = scaleL c bs  +'  cs *' (b:bs)

arithmetics:

  c  = a/b
  cs = (as - scaleL c bs) /' (b:bs)


Derivation of one of the |mulL| cases:

  eval ((a:as) *' bs) x
= homomorphism
  eval (a:as) x  *  eval bs x
= def. eval
  (a+ x* eval as x)  *  eval bs x
= arithmetics
  (a*eval bs x)  +  x * (eval as x * eval bs x)
= scale lemma, |eval| homomorphism for |*|
  eval (scaleL a bs) x  +  x*eval (as *' bs) x
= shift lemma,
  eval (scaleL a bs) x  +  eval (shiftL (as *' bs)) x
= |eval| homomorphism for |+|
  eval (scaleL a bs  +'  shiftL (as *' bs)) x




----------------------------------------------------------------
  eval ((a:as) * (b:bs)) x
=
  eval (a:as) x  *  eval (b:bs) x
=
  (a+x*eval as x) * eval (b:bs) x
=
  a*(b+x*eval bs x) + (x*eval as x)*eval (b:bs) x
=
  (a*b)+a*(x*eval bs x) + x*(eval as x * eval (b:bs) x)
=
  (a*b)+x*(a*eval bs x  +  eval (as * (b:bs)) x)
=
  (a*b)+x*(eval (scale a bs) x  +  eval (as * (b:bs)) x)
=
  (a*b)+x*(eval ((scale a bs)  +  (as * (b:bs))) x)
=
  eval ((a*b) : ((scale a bs)  +  (as * (b:bs)))) x
=
  (a*b)+x*(a*eval bs x) + x*(b*eval as x) + x*(x*(eval as x)*(eval bs x))
= {- Scale lemma + Shift lemma -}
  (a*b)+x*(eval (scale a bs) x) + x*(eval (scale b as) x) + x*(eval (shiftL as) x)*(eval bs x))
= {- eval for |*| -}
  (a*b)+x*(eval (scale a bs) x) + x*(eval (scale b as) x) + x*(eval ((shiftL as) * bs) x)
= {- eval for |+| -}
  (a*b)+x*(eval ((scale a bs) + (scale b as) + (shiftL as) * bs) x)
= {- eval for |:| -}
  eval (a*b : (scale a bs + scale b as + shiftL as * bs)) x

Thus we can define |mulL| as above.

----------------

mulLShiftLemma as [] =
  [ evalL (mulL (shiftL as) []) x
  , evalL ([]) x
  , 0
  ]
mulLShiftLemma as (b:bs) =
  [ evalL (mulL (shiftL as) (b:bs)) x
  , evalL (mulL (0:as) (b:bs)) x
  , evalL ((0*b) : addL   (scaleL 0 bs) (addL (scale b as) (mulL (shiftL as) bs))) x
  , evalL (0 : addL (scale b as) (mulL (shiftL as) bs)) x
  , 0
  ]


mulL (a:as) (b:bs) = (a*b) :  addL   (scaleL a bs)
                              (addL  (scaleL b as)
                                     (mulL (shiftL as) bs))

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
