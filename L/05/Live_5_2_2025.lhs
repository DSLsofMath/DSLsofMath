\begin{code}
module Live_5_2 where
import qualified Prelude
import Prelude (id, const, error, Show, take, Bool(False, True), (&&), Eq((==)), Int)
import DSLsofMath.Algebra (Additive(zero,(+)), Multiplicative((*),one), MulGroup((/)), Ring, Field)
\end{code}
Domain-Specific Languages of Mathematics course
Chapter 5 (Week 5), Lecture 2, Live coding part.

The Ring of polynomials (as lists of coefficients).

0. Define the DSL (types for syntax & semantics, eval)
1. Define the Ring methods for polynomials (& Power Series)
2. Defined methods for derivative and integral

----------------
0. Define the DSL (types for syntax & semantics, eval)
\begin{code}
newtype Poly a = P [a] deriving Show
type Fun a = a -> a
evalP :: Ring a => Poly a -> Fun a
evalP (P cs) = evalL cs

evalL :: Ring a => [a] -> Fun a
evalL []      =  zero
evalL (c:cs)  =  const c  +  (id * (evalL cs))
-- evalL (c:cs) x = const c x +  (id x * (evalL cs) x)
-- evalL (c:cs) x = c + x * evalL cs x

xP :: Ring a => Poly a
xP = P [zero,one]

p1 = P [1,2]
p2 = P [1,2,3]

\end{code}

----------------
1. Define (some of) the Ring methods for polynomials

Ring a = (Additive a, AddGroup a, Multiplicative a)
          (+), zero,  negate,     (*), one

\begin{code}
instance Additive a => Additive (Poly a) where (+) = addP; zero = zeroP

zeroP :: Poly a
zeroP = P zeroL

zeroL :: [a]
zeroL = []

addP :: Additive a => Poly a -> Poly a -> Poly a
addP (P as) (P bs) = P (addL as bs)

addL :: Additive a => [a] -> [a] -> [a]
addL [] bs = bs    -- 0 + p = p
addL as [] = as    -- q + 0 = q
addL (a:as) (b:bs) = (a + b):(addL as bs)

onesP :: Ring a => Poly a
onesP = P onesL
onesL :: Ring a => [a]
onesL = one : onesL
\end{code}
Spec. av addP
  H2(evalP, addP, (+))
==
  Forall x, y. evalP (addP x y) == evalP x + evalP y

Spec. av addL
  H2(evalL, addL, (+))
=
  Forall x, y. evalL (addL x y) == evalL x + evalL y

-- Main case:
  Forall a, as, b, bs.

Vi vill visa detta:
  evalL (addL (a:as) (b:bs)) == evalL (a:as) + evalL (b:bs)
  givet IndHyp = evalL (addL as bs) == evalL as + evalL bs
Vi börjar med
RHS =
  evalL (a:as) + evalL (b:bs)
= -- def. av evalL
  (const a  +  (id * (evalL as)))  +  (const b  +  (id * (evalL bs)))
= Ring-laws
  (const a + const b) +  id * ((evalL as) + (evalL bs))
= -- const + ring laws
  (const (a + b)) +  id * ((evalL as) + (evalL bs))
= -- Ind.Hyp.
  (const (a + b)) +  id * (evalL (addL as bs))
= -- def. av evalL (baklänges)
  evalL ((a + b):(addL as bs))
=?=  -- by def. of addL (core case): addL (a:as) (b:bs) = (a + b):(addL as bs)
  evalL (addL (a:as) (b:bs))
=
  LHS


-- IndHyp: (evalL as) + (evalL bs) = evalL (addL as bs)

----------------
Structural induction:
We want to prove Forall as, bs. P(as, bs)
  P(as,bs)  =   evalL (addL as bs) == evalL as + evalL bs
We need
  P([], []) and          -- base case 0
  Forall as. P(as, [])   -- base case 1
  Forall bs. P([], bs)   -- base case 2
  -- induction step
  Forall a, as, b, bs. P(as,bs) => P(a:as, b:bs)



evalL (addL (a:as) (b:bs))
=
LHS





----------------
2. Define methods for derivative and integral

Spec. of derP: H₁(evalP, derP, D)
 which expands to
  forall cs. evalP (derP cs) = D (evalP cs)

\begin{code}
instance Eq a => Eq (Poly a) where (==) = eqP

eqP :: Eq a => Poly a -> Poly a -> Bool
eqP (P as) (P bs) = eqL as bs

eqL :: Eq a => [a] -> [a] -> Bool
eqL [] [] = True
eqL [] (b:bs) = False  -- TODO implement isZeroL
eqL (a:as) [] = False  -- TODO implement isZeroL
eqL (a:as) (b:bs) = (a==b) && (eqL as bs)

test1 = derP (P [1,3,1::Prelude.Integer]) == P [3,2]   -- D (\x -> 1+3*x+x^2) = \x -> 3 + 2*x

derP :: Ring a => Poly a -> Poly a
derP (P cs) = P (derL cs)

derL :: Ring a => [a] -> [a]
derL (_:cs) = Prelude.zipWith (*) cs oneUp

oneUp :: Ring a => [a]
oneUp = one : Prelude.map (one+) oneUp
\end{code}

Specification of integ:
  forall c, cs. derP (integP c cs) = cs

\begin{code}
integP :: Field a => a -> Poly a -> Poly a
integP c (P cs) = P (integL c cs)

integL :: Field a => a -> [a] -> [a]
integL c cs = c : Prelude.zipWith (/) cs oneUp
\end{code}

Example: Power Series and the exponential

\begin{code}
expP :: Field a => Poly a
expP = integP one expP

takeP :: Int -> Poly a -> Poly a
takeP n (P as) = P (take n as)
\end{code}

























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
