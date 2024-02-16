\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
module DSLsofMath.PSDS where
import Prelude (Int, Double,
                Show, String,
                Ord, Ordering, (<), (<=), (>=), compare,
                Eq, Bool, (==), (&&), otherwise, snd,
                error, (.),
                (++), zipWith, map, head, tail, init, last, take, reverse, length, replicate)
import qualified Prelude
import DSLsofMath.FunExp
import DSLsofMath.Algebra (Additive((+),zero), AddGroup(negate), (-),
                           Ring, fromInteger, (^+),
                           Multiplicative((*),one),
                           MulGroup(recip), (/), Field, (/),
                           Algebraic(sqrt),
                           Transcendental(pi,sin,cos,exp) )
\end{code}

From Chapter 6. Taylor and Maclaurin series


2. Reminder of the numeric instances for Power Series (from L5.2)

2a. Types + eval
\begin{code}
newtype Poly a = P {unP :: [a]}

instance Show a => Show (Poly a) where show = showP

showP :: Show a => Poly a -> String
showP (P cs) = "P " ++ Prelude.show cs


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

-- Compare also to mulD later
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
derL []      = []
derL (_:as)  = zipWith (*) as fromOne

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
derD []          = []
derD as@(_:as')  = as'
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

-- Note that the name "mulL" is already taken.
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

--alternative with remainder
divP :: (Eq a, Field a) => PS a -> PS a -> (PS a, PS a)
divP (P as) (P bs) = (P q, P r)
  where (q,r) = divR as bs

divR :: (Eq a, Field a) => [a] -> [a] -> ([a],[a])
divR []      _bs     =  ([],[])                        -- case |0/q|
divR (0:as)  (0:bs)  =  divR as bs                     -- case |xp/xq|
--divL (0:as)  bs      =  zero : divR as bs              -- case |xp/q|
divR as      [b]     =  (scaleL (recip b) as,[])       -- case |p/c|
divR _       []      = error "divR: division by zero"
divR as bs | length as >= length bs = divrec bs ([],as)
--divP (P (a:as)) (P (b:bs)) | degree (P (a:as)) == degree (P (b:bs)) = (P [q], P ((a:as) - q*(b:bs)))
--  where al = tail as
--        bl = tail bs
--        q = al/bl
--divP p q = (divPS p q, P [])

divrec :: (Eq a, Field a) => [a] -> ([a],[a]) -> ([a],[a])
divrec d (q,[]) = (q,[])
divrec d (q,r) | last r == 0 = divrec d (q, init r)
               | diff >= 0 = divrec d (addL q t, addL r (negateL (mulL t d)))
               | Prelude.otherwise = (q,r)
  where t = replicate diff 0 ++ [last r / last d]
        diff = rd - dd
        rd = length r
        dd = length d
\end{code}


\begin{code}
instance Prelude.Functor PS where fmap=fmapPS
instance Prelude.Functor DS where fmap=fmapDS
fmapPS :: (a->b) -> (PS a -> PS b)
fmapDS :: (a->b) -> (DS a -> DS b)
fmapPS f (P  as) = P  (map f as)
fmapDS f (DS as) = DS (map f as)
\end{code}

Note that degree will return |-1| as the "degree" of the zero polynomial  (which is wrong).
\begin{code}
degree :: (Eq a, Additive a) => Poly a -> Int
degree p = Prelude.length (unP (normalPoly p)) - 1

normalPoly :: (Eq a, Additive a) => Poly a -> Poly a
normalPoly (P cs) = P (normalL cs)

normalL :: (Eq a, Additive a) => [a] -> [a]
normalL xs | isZeroL xs = []
normalL (x:rest) = x : normalL rest

eqPoly :: (Eq a, Additive a) => Poly a -> Poly a -> Bool
eqPoly p1 p2 = let p1L = unP (normalPoly p1)
                   p2L = unP (normalPoly p2)
               in p1L == p2L

comparePoly :: (Ord a, Additive a) => Poly a -> Poly a -> Ordering
comparePoly p1 p2 = let P p1N = normalPoly p1
                        P p2N = normalPoly p2
                    in compare p1N p2N

instance (Eq  a, Additive a) => Eq  (Poly a) where (==) = eqPoly
instance (Ord a, Additive a) => Ord (Poly a) where compare = comparePoly

isZero :: (Eq a, Additive a) => Poly a -> Bool
isZero = isZeroL . unP

isZeroL :: (Eq a, Additive a) => [a] -> Bool
isZeroL = Prelude.all (zero==)
\end{code}

Function composition as an operation on polynomials.
\begin{code}
comP :: Ring a => Poly a -> Poly a -> Poly a
comP (P xs) (P ys) = P (comL xs ys)

comL :: Ring a => [a] -> [a] -> [a]
comL  []      q   = []
comL  (a:as)  []  = [a]
comL  (a:as)  bs  = addL [a] (mulL bs (comL as bs))
\end{code}

Specification:
  (cs, rs) = divModPS as bs
when
  as == bs*cs + rs && degree rs < degree bs

\begin{code}
divModP :: (Eq a, Field a) => PS a -> PS a -> (PS a, PS a)
divModP p q = divModP' (normalPoly p) (normalPoly q)
divModP' :: (Eq a, Field a) => PS a -> PS a -> (PS a, PS a)
divModP' (P as) (P bs) = (P (reverse cs), P (reverse rs))
  where (cs, rs) = divModL (length as - length bs) (reverse as) (reverse bs)

-- Note that the lists start with the highest degree
divModL :: (Eq a, Field a) => Int -> [a] -> [a] -> ([a], [a])
divModL d _ [] = error "divModL: div. by zero"
divModL d as bs | d < 0 = ([], as)
divModL d [] _ = ([],[])
divModL d (a:as) q@(b:bs) = (c:cs, rs)
  where  c = a/b
         (cs, rs) = divModL (d-1) (addL as (scaleL (-c) bs)) q


\end{code}
TODO: Unfinished notes
case |xp/xq|:
  (0:as) == (0:bs)*cs + (0:rs) && degree (0:rs) < degree (0:bs)
  as == bs*cs + rs && degree rs < degree bs
case |xp/q|:
 (0:as) == bs*(0:cs) + (0:rs)     -- TODO fix degrees
  as == bs*cs + rs && degree rs < degree bs
case (a+xp)/(b+xq):
  as == c*bs+b*cs + 0:bs*cs + rs
  (as-c*bs) == (b:bs)*cs + rs && degree rs < degree (b:bs)


Example:
  (1+x*(3+x))/(1+x)
== let a=1, as=[3,1], b=1, bs=[1], c=a/b=1
  let (cs, rs) = divMod (as-bs) (b:bs)
  in ?
==
  let (cs, rs) = divMod [2,1] [1,1]
  in ?
==
  let (cs, rs) = ([2],[-1])
  in (


Computing a "Greatest Common Divisor".
Divisor(p,q) = Exists s. q*s==q
DivBoth(a,p,q) = Divisor(a,p) && Divisor(a,q)
Let g = gcdP p q
then  DivBoth(g,p,q)
      && Forall d. DivBoth(d,p,q) =>  d <= g
    where p <= q = degree p <= degree q

There are many GCDs, so it makes sense to further restrict it to have
A) integral coefficients or B) leading coefficient == one. For our use
case we only need to know if the GCD has degree <= 0, thus it does not
really matter (but may be good further on).

\begin{code}
gcdP :: (Field a, Eq a) => Poly a -> Poly a -> Poly a
gcdP a b  | isZero b = toMonic a
          | degree b <= degree a = gcdP b (snd (divModP a b))
          | otherwise            = gcdP b a


toMonic :: (Eq a, Field a) => Poly a -> Poly a
toMonic p = P (map (/cn) cs)
  where  pn@(P cs) = normalPoly p
         cn = last cs
\end{code}

Square-free factorisation by Yun's algorithm
https://en.wikipedia.org/wiki/Square-free_polynomial#Yun's_algorithm

If f = Prod_{i=1}^{k} a_i^i
is the desired factorization, we have
  a_0   = gcd(f,f')
  a_0   = Prod_{i=2}^{k} a_i^{i-1}
  f/a_0 = Prod_{i=1}^{k} a_i

let a0 = gcd(f,f')
    b1 = f/a0
    c1 = f'/a0
    d1 = c1 - b1'
    i  = 1

\begin{code}
yun :: (Eq a, Field a) => Poly a -> [Poly a]
yun f = yunGo b1 c1 d1
  where  a0 = gcdP f f'
         f' = derP f
         b1 = f/a0
         c1 = f'/a0
         d1 = c1 - derP b1
yunGo bi ci di  | degree bi <= 0 = []
                | otherwise =
  let  ai = gcdP bi di
       bi1 = bi / ai
       ci1 = di / ai
       di1 = ci1 - derP bi1
  in ai : yunGo bi1 ci1 di1
\end{code}
