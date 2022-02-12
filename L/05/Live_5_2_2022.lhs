\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RebindableSyntax #-}
module Live_5_2_2022 where
import qualified Prelude
import Prelude (Eq((==)), Show, id, (.), error, otherwise, 
                map, zipWith, iterate, take,
                Bool, Int, Integer, Rational, Maybe(..))
import DSLsofMath.FunExp hiding (eval)
import DSLsofMath.FunExpInst ()
import DSLsofMath.Algebra
\end{code}

The Ring of polynomials (as lists of coefficients).

Ring a = (Additive a, AddGroup a, Multiplicative a)
          (+), zero,  negate,     (*), one


defined derP, intP
define expP in terms of intP

\begin{code}
newtype Poly a = Poly [a] deriving (Show,Eq)

evalP :: Ring a => Poly a -> (a -> a)
evalP (Poly as) = evalL as

evalL :: Ring a => [a]    -> (a -> a)
evalL []      x = zero
evalL (a:as)  x = a + x*(evalL as x)

-- Example polynomials
constP :: a -> Poly a
constP c = Poly [c]

xP :: Ring a => Poly a
xP = Poly [zero, one]     -- the polynomial 'x'

xm12 :: Ring a => Poly a
xm12 = (xP-one)^+2          -- (x-1)²

instance Additive a =>   Additive (Poly a) where  (+) = addP; zero = zeroP
instance Ring a => Multiplicative (Poly a) where  (*) = mulP; one  = oneP

-- Separately defined to show the type and simplify testing.
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

mulL :: Ring a => [a] -> [a] -> [a]
mulL [] p = []
mulL p [] = []
mulL (a:as) (b:bs) = (a*b) : addL  (scaleL a bs)
                                   (mulL as (b:bs))

scaleP :: Multiplicative a => a -> PS a -> PS a
scaleP s (Poly as) = Poly (scaleL s as)

scaleL :: Multiplicative a => a -> [a] -> [a]
scaleL s = map (s*)

zipWithLonger :: (a->a->a) -> [a] -> [a] -> [a]
zipWithLonger op [] bs = bs            -- 0+p == p
zipWithLonger op as [] = as            -- p+0 == p
zipWithLonger op (a:as) (b:bs) = first : rest
  where  first  = op a b
         rest   = zipWithLonger op as bs

instance AddGroup a => AddGroup (Poly a) where  negate = negateP

negateP :: AddGroup a => Poly a -> Poly a
negateP (Poly as) = Poly (negateL as)

negateL :: AddGroup a => [a] -> [a]
negateL = map negate
\end{code}

Some algebraic properties:

  evalL [a]    x == a
  evalL (0:as) x == x * evalL as x

  a:as = [a] + (0:as)

Power Series

  f x = sum a_i*x^i
  if all ai are natural numbers in 0..9
  then we can read them off from the the decimal expansion of
    f 0.1

\begin{code}
type PS = Poly

evalPS :: Ring a => Int -> PS a -> a -> a
evalPS n = evalP . takeP n

takeP :: Int -> PS a -> Poly a
takeP n (Poly as) = Poly (take n as)
\end{code}

Spec. of derP:
  forall cs. evalP (derP cs) = D (evalP cs)

\begin{code}
derP :: Ring a => Poly a -> Poly a
derP (Poly as) = Poly (derL as)

derL :: Ring a => [a] -> [a]
derL []      = []
derL (_:as)  = zipWith (*) (countUp one) as

countUp :: Ring a => a -> [a]
countUp = iterate (one+)
\end{code}
a0:a1:a2:a3:...

  a0 + a1*x^1 + a2*x^2 + ...
     1*a1*x^0 + 2*a2*x^1 + 3*a3*x^2 + ...
     1*a1 :     2*a2     : 3*a3     :


Specification of integ:
  forall c, cs. derP (integ c cs) = cs
\begin{code}
integ :: Field a => a -> Poly a -> Poly a
integ c (Poly cs) = Poly (intL c cs)

intL :: Field a => a -> [a] -> [a]
intL c cs = c : zipWith (/) cs (countUp one)

specInt c cs =  derP (integ c cs) == cs

-- exp 0 = 1; exp 1 = 2.718..
expP :: Field a => Poly a
expP = integ one expP  -- D exp = exp, exp 0 = 1
                       -- exp = I exp, exp 0 = 1
sinP, cosP :: Field a => Poly a
sinP = integ zero cosP
cosP = integ one (negate sinP)
-- sin eps ~= eps
-- sin = I cos && sin 0 = 0
-- cos = I (-sin) && cos 0 = 1
\end{code}

Solve   f'' + 2*f' + f = sin,  f 0 = 2, f' 0 = 1

Step 0: solve for the highest derivative:
  f'' = sin - f - 2*f'
Step 1: Ansatz: f = eval as; f' = eval as'; f'' = eval as''
Step 2: transform to power series
Step 3: fill in "integ-equations" for as' and as
Step 4: If you do this by hand: fill in the coefficient lists step by step.
\begin{code}
as, as', as'' :: Field a => PS a
as'' = sinP - as - scaleP 2 as'
as'  = integ 1 as''   -- uttryck f' som integral av f''
as   = integ 2 as'

lhs :: Field a => Int -> a -> a

lhs n = f'' + 2*f' + f
  where f   = evalPS n as
        f'  = evalPS n as'
        f'' = evalPS n as''
rhs :: Transcendental a => Int -> a -> a
rhs n = evalPS n sinP

testEq n x = lhs n x - rhs n x -- should be close to zero
\end{code}

Don't forget to check the original equation (often catches simple
 mistakes).
















\begin{code}
instance Field a => MulGroup (Poly a) where recip = recipP
recipP :: Field a => Poly a -> Poly a
recipP (Poly as) = Poly (recipL as)

recipL :: Field a => [a] -> [a]
recipL [] = error "recipL: division by zero"
recipL (a:as) = r
  where  r  = b:bs
         b  = recip a
         bs = scaleL (negate b) (mulL as r)

test1 :: Field a => Poly a
test1 = takeP 5 (recip (one-xP))
\end{code}


\begin{code}
ev :: Ring a => FunExp -> Poly a
ev X            = xP
ev (Const c)    = cP c
ev (f :+: g)    = ev f + ev g
ev (f :*: g)    = ev f * ev g
ev (Negate f)   = negate (ev f)

test3 :: FunExp
test3 = ((X-one)^+2)

cP :: Ring a => REAL -> Poly a
cP c = case integral c of
  Nothing -> error "cP: only works with integers"
  Just i  -> fromInteger i

integral :: REAL -> Maybe Integer
integral c | fromInteger i == c  = Just i
           | otherwise           = Nothing
  where i = Prelude.round c 
\end{code}













\begin{spec}
as, as', as'' :: Field a => PS a
as'' = sinP - scaleP 2 as' - as
as'  = integ 1 as''
as   = integ 2 as'
\end{spec}
