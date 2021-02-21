\begin{code}
module DSLsofMath.DerAll where
import Prelude hiding (Num(..),(^),Fractional(..),Floating(..))
import DSLsofMath.Algebra
  (Additive(..), two, AddGroup(..), (-),
   Multiplicative(..), (^+), MulGroup(..), (^), Ring, Field,
   Transcendental(..))
import DSLsofMath.FunExp(FunExp(..), REAL)
import DSLsofMath.Simplify(simplify, size)

type FE = FunExp
type DS = []
derAll :: FunExp    ->  DS FunExp
derAll  (Const c)    =  constdA c
derAll  X            =  xdA
derAll  (e1 :+: e2)  =  adddA     (derAll e1)  (derAll e2)
derAll  (e1 :*: e2)  =  muldA     (derAll e1)  (derAll e2)
derAll  (Recip e)    =  recipdA   (derAll e)
derAll  (Negate e)   =  negatedA  (derAll e)
derAll  (Exp e)      =  expdA     (derAll e)
derAll  (Sin e)      =  sindA     (derAll e)
derAll  (Cos e)      =  cosdA     (derAll e)

constdA :: REAL -> DS FunExp
constdA c = Const c : zero

xdA :: DS FunExp
xdA = X : constdA one

adddA :: DS FunExp -> DS FunExp -> DS FunExp
adddA = addDS

negatedA :: DS FunExp -> DS FunExp
negatedA = negateDS

muldA :: DS FunExp -> DS FunExp -> DS FunExp
muldA fs@(f:fs') gs@(g:gs') = (f:*:g) : adddA (muldA fs' gs) (muldA fs gs')
muldA _ _ = zero
-- muldA = mulDS  -- alternative definition with Multiplicative FunExp and Ring [a] in scope

recipdA :: DS FunExp -> DS FunExp
recipdA = recipDS

expdA, sindA, cosdA :: DS FunExp -> DS FunExp
expdA = expDS
sindA = sinDS
cosdA = cosDS
\end{code}


General DS instances and helpers (see also Chapters 5 and 6).
\begin{code}
instance Additive a => Additive [a] where
  zero = zeroDS
  (+) = addDS

zeroDS :: [a]
zeroDS = []

constDS :: Additive a => a -> [a]
constDS c = c : zero

addDS :: Additive a => DS a -> DS a -> DS a
addDS = zipWithLonger (+)

zipWithLonger :: (a->a->a) -> ([a] -> [a] -> [a])
zipWithLonger _   []      bs      = bs  -- |0+bs == bs|
zipWithLonger _   as      []      = as  -- |as+0 == as|
zipWithLonger op  (a:as)  (b:bs)  = op a b : zipWithLonger op as bs

instance AddGroup a => AddGroup [a] where
  negate = negateDS

negateDS :: AddGroup a => [a] -> [a]
negateDS = map negate

instance Ring a => Multiplicative [a] where
  one = constDS one
  (*) = mulDS

mulDS :: Ring a => DS a -> DS a -> DS a
mulDS fs@(f:fs') gs@(g:gs') = (f*g) : (fs'*gs)+(fs*gs')
mulDS _ _ = zero

instance Field a => MulGroup [a] where
  (/) = divDS

divDS :: Field a => DS a -> DS a -> DS a
divDS fs@(f:fs') gs@(g:gs')  = (f/g): fs'/gs - fs*(gs'/gs/gs)
divDS []         _           = zero
divDS _          []          = error "divDS: division by zero"

recipDS :: Field a => DS a -> DS a
recipDS []          = error "recipDS: division by zero"
recipDS gs@(g:gs')  = (recip g): negate (gs'*rgs*rgs)
  where rgs = recip gs

piDS :: Transcendental a => [a]
piDS = constDS pi

instance Transcendental a => Transcendental [a] where
  pi   = piDS
  exp  = expDS
  sin  = sinDS
  cos  = cosDS

expDS, sinDS, cosDS :: Transcendental a => [a] -> [a]
expDS fs@(f:fs')  = exp f :           exp fs   * fs'
expDS []          = constDS (exp zero)

sinDS fs@(f:fs')  = sin f :           cos fs   * fs'
sinDS []          = constDS (sin zero)

cosDS fs@(f:fs')  = cos f :   negate (sin fs)  * fs'
cosDS []          = constDS (cos zero)

\end{code}
