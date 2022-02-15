\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Live_6_1 where
import Prelude (Int, Double, Show, error, Bool, (==), (&&), 
                (.), zipWith, map, head, tail, take)
import qualified Prelude
import DSLsofMath.FunExp
import DSLsofMath.Algebra (Additive((+),zero), AddGroup(negate), (-), 
                           Multiplicative((*),one), Ring, (^+), 
                           Field, (/),
                           Algebraic(sqrt),
                           Transcendental(pi,sin,cos,exp) )
\end{code}

Chapter 6. Taylor and Maclaurin series
 - or -
Yet another type for representing functions.

Code cont. from file:../04/Live_4_3_2022.lhs and file:../05/Live_5_2_2022.lhs

1. Introduce the remaining type classes Transcendental and Algebraic
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

zeroP :: Poly a;   zeroP = P zeroL
zeroL :: [a];      zeroL = []

oneP :: Ring a => Poly a;   oneP = P oneL
oneL :: Ring a => [a];      oneL = [one]

addP :: Additive a => Poly a -> Poly a -> Poly a
addP (P as) (P bs) = P (addL as bs)

addL :: Additive a => [a] -> [a] -> [a]
addL = zipWithLonger (+)

zipWithLonger :: (a->a->a) -> ([a] -> [a] -> [a])
zipWithLonger op = zWL
  where  zWL []      bs      = bs
         zWL as      []      = as
         zWL (a:as)  (b:bs)  = (op a b) : zWL as bs

mulP :: Ring a => Poly a -> Poly a -> Poly a
mulP (P as) (P bs) = P (mulL as bs)

mulL :: Ring a => [a] -> [a] -> [a]  
mulL [] bs = []
mulL as [] = []
mulL (a:as) bs = addL (scaleL a bs) (zero:mulL as bs)

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
derL (_:as) = zipWith (*) as fromOne

fromOne :: Ring a => [a]
fromOne = Prelude.iterate (one+) one

integP :: Field a => a -> Poly a -> Poly a
integP a (P as) = P (integL a as)

integL :: Field a => a -> [a] -> [a]
integL a as = a : zipWith (/) as fromOne
\end{code}

----------------
3. Implement "Derivative Series" (as yet another type to rep. functions)

* In the Jamboard lecture parts we saw how

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
  DAll f = f : f' : f'' : ....
      DAll f'= f' : f''
specDerAll :: FunExp -> Bool -- cannot actually be run due to inf. lists
specDerAll e = head (derAll e) == e  &&                
               tail (derAll e) == derAll (derive e)   -- H1(derAll, derive, tail)

We can implement
  derAll e = e : derAll (derive e)
but note that derive is not a homomorphism - can we do better?

+ 3b. Explain the type DS, evalDS, translation from DS to PS and back

\begin{code}
newtype DS a = DS [a]   -- basically the same as Poly and PS
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

\begin{code}
xDS :: Ring a => DS a   -- id 0 : const 1 0 : id'' 0 : ...
xDS = DS (zero:one:zeroL)

zeroDS :: Additive a => DS a
zeroDS = DS zeroL

oneDS :: Ring a => DS a
oneDS = DS oneL    -- const 1 0, (const 1)' 0, 

derDS :: DS a -> DS a
derDS (DS as) = DS (derD as)

derD :: [a] -> [a]
derD [] = []
derD as@(_:as') = as'
  -- a   == head as
  -- as' == tail as =~ derDS as

integDS :: Field a => a -> DS a -> DS a
integDS c (DS cs) = DS (integD c cs)

integD :: Field a => a -> [a] -> [a]
integD c cs = c : cs   -- c here is f(0), cs are all the derivatives
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
addDS :: Additive a => DS a -> DS a -> DS a
addDS (DS as) (DS bs) = DS (addL as bs)  
instance Additive a => Additive (DS a) where zero=zeroDS; (+)=addDS

mulDS :: Ring a => DS a -> DS a -> DS a
mulDS (DS as) (DS bs) = DS (mulD as bs)

mulD :: Ring a => [a] -> [a] -> [a]
mulD [] _ = []
mulD _ [] = []
mulD fs@(f:fs') gs@(g:gs') = m : ms'
  where m = f*g
        ms' = addL (mulD fs' gs) (mulD fs gs')
            -- fs'*gs + fs*gs'

instance Ring a => Multiplicative (DS a) where one=oneDS; (*)=mulDS

derAll :: Ring a => FunExp -> DS a
derAll X = xDS
derAll (f:*:g) = derAll f * derAll g  -- H2(derAll,(:*:),mulDS)
\end{code}
-- (\x -> x^2) 0 : (\x -> 2*x) 0 : (\x -> 2) 0 : zero
--             0 :             0 :           2 : []

1. Wishful thinking (also called induction hypothesis): assume the two
  arguments already satisfy the desired properties (specDerAll).

2. Thus we can match on the pattern fs@(f:fs') knowing that
     fs == derAll f    
   and
     fs' == derAll (derive f) == derDS fs
   + similarly for gs@(g:gs')


