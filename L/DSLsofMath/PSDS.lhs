\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
module DSLsofMath.PSDS where
import Prelude (Int, Double, Show, Eq, Bool, (==), (&&), 
                error, (.), zipWith, map, head, tail, take)
import qualified Prelude
import DSLsofMath.FunExp
import DSLsofMath.Algebra (Additive((+),zero), AddGroup(negate), (-),
                           Ring, fromInteger, (^+), 
                           Multiplicative((*),one), 
                           MulGroup(recip), (/), Field, (/),
                           Algebraic(sqrt),
                           Transcendental(pi,sin,cos,exp) )
\end{code}

Chapter 6. Taylor and Maclaurin series

 - or -
Yet another type for representing functions.

----------------
2. Reminder of the numeric instances for Power Series (from L5.2)

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

scaleP :: Ring a => a -> Poly a -> Poly a
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

+ 3b. Explain the type DS, evalDS, translation from DS to PS and back

\begin{code}
newtype DS a = DS [a]   -- basically the same internals as Poly and PS
  deriving Show         --   but different meaning / operations
-- f 0 : f' 0 : f'' 0 :    (all derivatives of f, evaluated at zero)
evalDS :: Field a => Int -> DS a -> (a -> a)
evalDS n ds = evalPS n (fromMaclaurin ds)

takeDS :: Int -> DS a -> DS a
takeDS n (DS ds) = DS (take n ds)

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

\begin{code}
addDS :: Additive a => DS a -> DS a -> DS a
addDS (DS as) (DS bs) = DS (addL as bs)  

mulDS :: Ring a => DS a -> DS a -> DS a
mulDS (DS as) (DS bs) = DS (mulD as bs)

mulD :: Ring a => [a] -> [a] -> [a]
mulD [] _ = []
mulD _ [] = []
mulD fs@(f:fs') gs@(g:gs') = m : ms'
  where m = f*g
        ms' = addL (mulD fs' gs) (mulD fs gs')
            -- fs'*gs + fs*gs'

instance Additive a => Additive   (DS a) where zero=zeroDS; (+)=addDS
instance AddGroup a => AddGroup   (DS a) where negate=negateDS
instance Ring a => Multiplicative (DS a) where one=oneDS; (*)=mulDS

negateDS :: AddGroup a => DS a -> DS a
negateDS (DS as) = DS (negateL as)  -- same as for PS
\end{code}

\begin{code}
derAll :: Ring a => FunExp -> DS a
derAll X = xDS
derAll (f:*:g) = derAll f * derAll g  -- H2(derAll,(:*:),mulDS)
derAll _ = error "derAll: TODO implement more cases"
\end{code}


\begin{code}
instance (Eq a, Field a) => MulGroup (PS a) where (/) = divPS

divPS :: (Eq a, Field a) => PS a -> PS a -> PS a
divPS (P as) (P bs) = P (divL as bs)

divL :: (Eq a, Field a) => [a] -> [a] -> [a]
divL []      _bs     =  []                             -- case |0/q|
divL (0:as)  (0:bs)  =  divL as bs                     -- case |xp/xq|
divL (0:as)  bs      =  zero : divL as bs              -- case |xp/q|
divL as      [b]     =  scaleL (recip b) as            -- case |p/c|
divL (a:as)  (b:bs)  =  c : divL (addL as (scaleL (-c) bs)) (b:bs)
                        where c = a/b
divL _       []      = error "divL: division by zero"
\end{code}


\begin{code}
instance Prelude.Functor PS where fmap=fmapPS
instance Prelude.Functor DS where fmap=fmapDS
-- TODO add types
fmapPS f (P  as) = P  (map f as)
fmapDS f (DS as) = DS (map f as)
\end{code}

