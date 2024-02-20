\begin{code}
module Live_6_1 where
import qualified Prelude
import Prelude (Int, Double, Maybe(Nothing, Just), Show, Eq, Rational,
                error, reverse, (.), dropWhile, (==), length, zipWith
               , id, const, take, map
               )
import DSLsofMath.Algebra  -- defines some numerical type classes
type REAL = Double
\end{code}
Domain-Specific Languages of Mathematics course
Chapter 6 (Week 6), Lecture 1, Live coding part.

The Ring of polynomials (as lists of coefficients).

0. Define the DSL (types for syntax & semantics, eval)
1. Define the Ring methods for polynomials & Power Series
2. Define methods for derivative and integral
3. Solve simple ODEs with power series [if time permits]

----------------
0. Define the DSL (types for syntax & semantics, eval)

Domain(s): polynomial functions and power series

\begin{code}
newtype Poly a = P [a] deriving Show
type PSem a = a->a
evalP :: Ring a => Poly a -> (a->a)
evalP (P as) = evalL as

evalL :: Ring a => [a] -> (a->a)
evalL []      = zero
evalL (a:as)  = const a  +  id * evalL as

type PS = Poly
evalPS :: Ring a => Int -> PS a -> (a->a)
evalPS n = evalP . takePS n
-- evalPS n ps = evalP (takePS n ps)

-- takePS :: Int -> PS a -> PS a
-- takePS :: Int -> Poly a -> Poly a
takePS :: Int -> PS a -> Poly a
takePS n (P as) = P (take n as)

-- evalP xP x = x
xP :: Ring a => Poly a
xP = P [zero, one]   -- 0 + 1*x
\end{code}

----------------
1. Define the Ring methods for Poly and PS

Reminder:
Ring a = (Additive a,  AddGroup a, Multiplicative a)
      ~= (((+), zero),  negate,    ((*), one)      )


\begin{code}
instance Additive a => Additive (Poly a) where zero = zeroP; (+) = addP
instance AddGroup a => AddGroup (Poly a) where negate = negateP
instance (Additive a, Multiplicative a) => Multiplicative (Poly a) where one = oneP; (*) = mulP

oneP :: Multiplicative a => Poly a
oneP = P oneL
oneL :: Multiplicative a => [a]
oneL = [one]

mulP :: (Additive a, Multiplicative a) => Poly a -> Poly a -> Poly a
mulP (P as) (P bs) = P (mulL as bs)

mulL :: (Additive a, Multiplicative a) => [a] -> [a] -> [a]
mulL = error "Don't touch"
-- mulL [] bs = []
-- mulL as [] = []
--mulL (a:as) bs = addL (scaleL a bs) (zero:(mulL as bs))
-- Def.: a - b = a + negate b
negateP :: AddGroup a => Poly a -> Poly a
negateP (P as) = P (negateL as)
negateL :: AddGroup a => [a] -> [a]
negateL = map negate

zeroP :: Poly a
zeroP = P zeroL
zeroL :: [a]
zeroL = []

addP :: Additive a => Poly a -> Poly a -> Poly a
addP (P as) (P bs) = P (addL as bs)

addL :: Additive a => [a] -> [a] -> [a]
addL [] bs = bs
addL as [] = as
addL (a:as) (b:bs) = (a+b) : addL as bs

scaleP :: Multiplicative a => a -> Poly a -> Poly a
scaleP a (P bs) = P (scaleL a bs)

scaleL :: Multiplicative a => a -> [a] -> [a]
scaleL a = map (a*)
-- scaleL = map . (*)

p1 :: Ring a => Poly a
p1 = P [one,two,one+two]
p2 :: Ring a => Poly a
p2 = (xP-one) ^+ 2
p3 :: Ring a => PS a
p3 = P countUp
p4 :: Ring a => PS a
p4 = derP p3
p5 :: Field a => PS a
p5 = integP zero p3
\end{code}

  -- Lemma1: (a:as) = (a:zeroL) + (zero:as)
  -- Lemma2: (zero:as)*bs = zero:(as*bs)
  -- eval (zero:as) x = zero + x * eval as x = x * eval as x

  mulL (a:as) bs
= -- notation
  (a:as)*bs
= -- Lemma1
  ((a:zeroL) + (zero:as))*bs
= -- Distributivity
  (a:zeroL)*bs   +  (zero:as)*bs
= -- Simplify + Lemma 2
  scaleL a bs    +   zero:(as*bs)
= -- notation
  addL (scaleL a bs) (zero:(mulL as bs))




1b. Connect back to (a variant of) FunExp
\begin{code}
data F = X | Zero | Add F F | Negate F | One | Mul F F -- ...
  deriving Show

evalF :: Ring a => F -> PS a
evalF = error "TODO"
\end{code}


----------------
2. Define methods for derivative and integral

Spec. of derP: H₁(evalP, derP, D)
 which expands to
  forall cs. evalP (derP cs) = D (evalP cs)

(Done last week, just repeated here.)
\begin{code}
derP :: Ring a => Poly a -> Poly a
derP (P as) = P (derL as)

derL :: Ring a => [a] -> [a]
derL [] = []
derL (_a:as) = zipWith (*) countUp as

countUp :: Ring a => [a]
countUp = Prelude.iterate (one+) one
\end{code}

Specification of integ:
  forall c, cs.  derP (integP c cs) = cs

\begin{code}
integP :: Field a => a -> Poly a -> Poly a
-- integP a (P as) = P (integL a as)
integP a (P as) = P (integL a as)

integL :: Field a => a -> [a] -> [a]
integL a cs = a : zipWith (/) cs countUp
\end{code}
integL a []        = [a]
integL a [c]       = [a,c]
integL a [c0,c1]   = [a,c0,c1/2]
integL a (c0:c1:c2:... = a:c0/1:c2/2:c3/3:...
  forall c, cs . derL (integL c cs) = cs
  test: cs=[]

   derL (integL c []) = []
   derL (c:[]) = []


3. Example: Power Series and the exponential

TODO expP, sinP, cosP

   integP one expP == expP
   derP expP == expP   &&   evalP expP zero = one

Eq 1'':   f' - f = 0, f(0) = 1
Eq 1'':   f' == f && f(0) = 1
Eq 1':    derP f == f && eval f 0 = one

Eq 1:     integP one f == f
\begin{code}
expP :: Field a => PS a
expP = integP one expP
expL :: Field a => [a]
expL = integL one expL

sinP :: Field a => PS a
cosP :: Field a => PS a
sinP = integP zero cosP            -- sin 0, D sin
cosP = integP one  (negateP sinP)  -- cos 0, D cos
\end{code}
  expL
=  -- def. av expL
  integL one expL
= -- def. integL
  1 : zipWith (/) expL countUp
= -- def. zipWith
  1 : (1/1) : zipWith (/) (tail expL) (tail countUp)
= -- def. zipWith
  1 : (1/1) : (1/2) : zipWith (/) (tail^2 expL) (tail^2 countUp)

--   zipWith op (a:as) (b:bs)
  =  op a b : zipWith as bs



take 10 expL = [1 % 1,1 % 1,1 % 2,1 % 6,1 % 24,1 % 120,1 % 720,1 % 5040,1 % 40320,1 % 362880]
   map (\i -> 1/fact i) [0..]


A. Appendix: From Ring to Field.

\begin{spec}
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
\end{spec}
