\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RebindableSyntax #-}
module Live_6_2 where
import Prelude (Int, Double, Show, error, Bool, (==), (&&),
                (.), zipWith, map, head, tail, take)
import qualified Prelude
import DSLsofMath.Simplify
import DSLsofMath.FunExp
import DSLsofMath.Algebra (Additive((+),zero), AddGroup(negate), (-),
                           Multiplicative((*),one), Ring, (^+), fromInteger,
                           Field, (/),
                           Algebraic(sqrt),
                           Transcendental(pi,sin,cos,exp) )
\end{code}

Chapter 6. Taylor and Maclaurin series
 - or -
Yet another type for representing functions.

Code continued from file:../04/Live_4_3_2023.lhs and file:../05/Live_5_2_2023.lhs

1. Introduce the remaining type classes Transcendental and Algebraic
  See file:../DSLsofMath/Algebra.hs::87
2. Reminder of the numeric instances for Power Series (from L5.2)
3. Implement "Derivative Series" (as yet another type to rep. functions)


----------------
2. Reminder of the numeric instances for Power Series (from L5.2)

----
2a. Types + eval
\begin{code}
newtype Poly a = P [a] deriving Show
evalP :: Ring a => Poly a -> (a -> a)
evalP (P cs) = evalL cs

evalL :: Ring a => [a] -> (a -> a)
evalL []      = zero
evalL (a:as)  = evalCons a (evalL as)

evalCons :: Ring a => a -> (a -> a) -> (a -> a)
evalCons a0 p = \x -> a0 + x * p x

xP :: Ring a => Poly a
xP = P [zero, one]

type PS = Poly
takeP :: Int -> PS a -> Poly a
takeP n (P as) = P (take n as)

evalPS :: Ring a => Int -> Poly a -> (a -> a)
evalPS n = evalP . takeP n
\end{code}

2b. Numeric instances for PS a = Poly a
\begin{code}
instance Additive a => Additive       (Poly a) where zero=zeroP; (+)=addP
instance AddGroup a => AddGroup       (Poly a) where negate=negateP
instance Ring a     => Multiplicative (Poly a) where one=oneP;   (*)=mulP

zeroP :: Poly a;            zeroP = P zeroL
zeroL :: [a];               zeroL = []

oneP :: Ring a => Poly a;   oneP = P oneL
oneL :: Ring a => [a];      oneL = [one]

addP :: Additive a => Poly a -> Poly a -> Poly a
addP (P as) (P bs) = P (addL as bs)

addL :: Additive a => [a] -> [a] -> [a]
addL = zipWithLonger (+)

zipWithLonger :: (a->a->a) -> ([a] -> [a] -> [a])
zipWithLonger op = zWL
  where  zWL []      bs      = bs   -- 0 + b = b
         zWL as      []      = as   -- a + 0 = a
         zWL (a:as)  (b:bs)  = (op a b) : zWL as bs

mulP :: Ring a => Poly a -> Poly a -> Poly a
mulP (P as) (P bs) = P (mulL as bs)

mulL :: Ring a => [a] -> [a] -> [a]
mulL []      bs  =  []    -- 0*b=0
mulL as      []  =  []    -- a*0=0
mulL (a:as)  bs  =  addL (scaleL a bs) (zero:mulL as bs)
 -- a:as = (a:zeroL) `addL` (zero:as)

scaleP :: Ring a => a -> PS a -> PS a
scaleP c (P as) = P (scaleL c as)

scaleL :: Ring a => a -> [a] -> [a]
scaleL c = map (c*)

negateP :: AddGroup a => Poly a -> Poly a
negateP (P as) = P (negateL as)

negateL :: AddGroup a => [a] -> [a]
negateL = map negate
\end{code}

----
2c. Motivating examples:
\begin{code}
two :: Ring a => a
two = one+one

pythagorean :: Transcendental a => a -> a
pythagorean = sin*sin + cos*cos

factorials :: Ring a => [a]
factorials = factorialsFrom zero one

factorialsFrom :: Ring a => a -> a -> [a]
factorialsFrom n factn = factn : factorialsFrom n' (factn * n')
  where n' = one+n

p1 :: Ring a => Poly a
p1 = (xP-one)^+2
\end{code}

----
2d. derivative and integral for power series

\begin{code}
derP :: Ring a => Poly a -> Poly a
derP (P as) = P (derL as)

derL :: Ring a => [a] -> [a]
derL [] = []
derL (_:as) = zipWith (*) as countUp

countUp :: Ring a => [a]
countUp = Prelude.iterate (one+) one

integP :: Field a => a -> Poly a -> Poly a
integP a (P as) = P (integL a as)

integL :: Field a => a -> [a] -> [a]
integL a as = a : zipWith (/) as countUp
\end{code}

----------------
3. Implement "Derivative Series" (as yet another type to rep. functions)

* In the blackboard lecture parts we saw how

  + ODEs could be transformed from equations on functions to equations
    on power series (infinite lists of coefficients).

  + the power series equations can be solved step by step to obtain
    better and better approximations

  + to get from a function f, via the "derivative series" of f, to the
    coefficients of the Maclaurin series expansion of f.

* Here we will focus on the implementation of the "derivative series"

+ 3a. Explain DAll (semantic) and derAll (syntactic)

DAll :: (R->R) -> [R->R]
DAll f = f : DAll (D f)   -- cannot be directly implemented in Haskell

Informally:
  DAll f = f : f' : f'' : ....
      DAll f'= f' : f''
specDerAll :: FunExp -> Bool -- cannot actually be run due to inf. lists
specDerAll e =  head (derAll e) == e  &&
                tail (derAll e) == derAll (derive e)   -- H1(derAll, derive, tail)

We can implement
  derAll e = e : derAll (derive e)
but note that derive is not a homomorphism - can we do better?

+ 3b. Explain the type DS, evalDS, translation from DS to PS and back

H2(derAll, Mul, mulDS)
... Forall f, g. derAll (Mul f g) = mulDS (derAll f) (derAll g)

\begin{code}
newtype DS a = DS [a]   -- basically the same type as Poly and PS
  deriving Show
-- f 0 : f' 0 : f'' 0 :    (all derivatives of f, evaluated at zero)
evalDS :: Field a => Int -> DS a -> (a -> a)
evalDS n ds = evalPS n (fromMaclaurin ds)

fromMaclaurin :: Field a => DS a -> PS a
fromMaclaurin (DS as) = P (zipWith (/) as factorials)
-- divFact

toMaclaurin :: Ring a => PS a -> DS a
toMaclaurin (P as) = DS (zipWith (*) factorials as)
-- mulFact
\end{code}

+ 3c. Implement derDS, integDS
derAll :: FunExp -> DS FunExp
Spec: derDS (derAll e) = derAll (derive e)
\begin{code}
derDS :: DS a -> DS a
derDS (DS ds) = DS (derD ds)
derD :: [a] -> [a]
derD [] = []
derD xs = tail xs

xDS :: Ring a => DS a
xDS = DS (zero:one:zeroL)


mulDS :: Ring a => DS a -> DS a -> DS a
mulDS (DS as) (DS bs) = DS (mulD as bs)

mulD :: Ring a => [a] -> [a] -> [a]
mulD [] bs = []
mulD as [] = []
mulD f@(f0:f') g@(g0:g') = (f0*g0) : error "övning"

\end{code}

+ 3d. Implement numeric operators on DS a
  Core question: Is there a definition of |mulDS| which makes |derAll| a
  homomorphism from (:*:) to mulDS?

We want
  H2(derAll,(:*:),mulDS)
which expands to
  forall f, g :: FunExp.
    derAll (f :*: g) == mulDS (derAll f) (derAll g)
and
  forall e :: FunExp.
    map eval (derAll e) == DAll (eval e)
where
  DAll f = f : DAll (D f)

\begin{code}
\end{code}
1. Wishful thinking (also called induction hypothesis): assume the two
  arguments already satisfy the desired properties (specDerAll).

2. Thus we can match on the pattern fs@(f:fs') knowing that
     fs == derAll f
   and
     fs' == derAll (derive f) == derDS fs
   + similarly for gs@(g:gs')

----------------
If we have time:

Payback time: ODE solving again (even easier this time).
\begin{spec}
expX :: Ring a => DS a
expX = integDS 1 expX

expx :: Ring a => [a]
expx = 1:expx

sinX :: Ring a => DS a
sinX = integDS 0 cosX
cosX :: Ring a => DS a
cosX = integDS 1 (-sinX)

-- pythagorean :: Ring a => DS a
-- pythagorean = sinX * sinX + cosX * cosX

takeDS :: Int -> DS a -> [a]
takeDS n (DS ds) = take n ds
\end{spec}

Extra material:
MulGroup and Transcendental instances.

MulGroup:
\begin{spec}
instance Field a => MulGroup (DS a) where recip = recipDS

recipDS :: Field a => DS a -> DS a
recipDS (DS ds) = DS (recipDL ds)

recipDL :: Field a => [a] -> [a]
recipDL [] = error "recipDL: divide by zero"
recipDL (fs@(f:fs')) = rs
  where  rs  = r : rs'
         r   = recip f
         rs' = mulDL (negateDL fs') (mulDL rs rs)
         -- D(1/fs) = -1/fs² * fs' = negate (rs*rs)*fs'
\end{spec}

   1/(1-x) = 1+x+x²+x³+...
           ~= P  [1,1,1,1,1,1,1,]
           ~= DS [1,1,2,6,24,720,...]



Transcendental:
\begin{spec}
instance Transcendental a => Transcendental (DS a) where
  pi=piDS;exp=expDS;sin=sinDS;cos=cosDS

piDS :: Transcendental a => DS a
piDS = DS [pi]

expDS, sinDS, cosDS :: Transcendental a => DS a -> DS a
expDS  ds  = integDS  (exp  (val0 ds))  (expDS ds   * derDS ds)
sinDS  ds  = integDS  (sin  (val0 ds))  (cosDS ds   * derDS ds)
cosDS  ds  = integDS  (cos  (val0 ds))  (-sinDS ds  * derDS ds)

val0 :: Additive a => DS a  ->  a
val0 (DS [])     = zero
val0 (DS (d:_))  = d
\end{spec}
