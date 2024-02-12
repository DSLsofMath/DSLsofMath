Continuing on from Week 4.

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
module Live_5_1_2022 where
import DSLsofMath.FunExp
import DSLsofMath.FunExpInst () -- just import instances
import DSLsofMath.Simplify
import qualified Prelude
import Prelude (Eq, Show, id, map, Int, Integer, iterate, take, error, const)
import DSLsofMath.Algebra
\end{code}

More work on "the DSL of functions and derivatives".

1. Two semantics of FunExp and a homomorphism between them: applyFD
2. Numeric class instances for Bi (pairs of "position + speed")

----------------
0. Reminder:

data FunExp  =  Const REAL
             |  X
             |  FunExp :+: FunExp
             |  FunExp :*: FunExp
             |  Negate FunExp

--             |  -- some more not covered today ...
eval :: Ring a => FunExp -> a -> a
class Additive a       where  zero :: a;  (+) :: a -> a -> a
class Multiplicative a where  one  :: a;  (*) :: a -> a -> a
class Additive a => AddGroup a where  negate :: a -> a
-- Ring a = (AddGroup a, Multiplicative a)
+ Remember that derive :: FunExp -> FunExp is not a homomorphism by itself - 
+ we need a pair of function + derivative.

----------------
1. Two different, but related, semantics of FunExp: FD a, a -> Bi a
   and a homomorphism between them: applyFD

\begin{code}
newtype FD a = FD (a->a, a->a)   -- Function + Derivative
newtype Bi a = Bi (a,    a)      -- Position + Speed
  deriving (Eq, Show)

-- forall c. H2(applyFD c, mulFD, mulBi)
applyFD :: a -> FD a -> Bi a
applyFD c (FD (f, f')) = Bi (f c, f' c)

-- 1a: implement \x -> (x-1)^+2 "by hand" as an FD:
fd1 :: Ring a => FD a
fd1 = FD (\x -> (x-1) ^+ 2, \x -> 2*(x-1))
     --   f                D f
      
-- 1b: implement "x" as an FD
xFD :: Ring a => FD a
xFD = FD (id, one)   -- one = const (one :: a )   :: a->a

-- 1c: implement \x -> (x-1)^+2 in any Ring:
e2 :: Ring a => a -> a
e2 x = (x-1)^+2
-- (x-1)² = x² -2x + 1 = 1 + (-2)*x + x²
-- 1d: implement \x -> (x-1)^+2 "automatically" as an FD:
e2FD :: Ring a => FD a
e2FD = e2 xFD

test2 :: Ring a => a -> Bi a
test2 c = applyFD c e2FD

test2Syn, test2SynSimp :: Bi FunExp
test2Syn = test2 X
test2SynSimp = simpBi test2Syn

list2 :: Ring a => [Bi a]
list2 = map test2 (countUp 4 zero)

countUp :: Ring a => Int -> a -> [a]
countUp steps start = take steps (iterate (one+) start)

-- applyFD c xFD =önskas= xBi c
--   Def. av xFD
-- applyFD c (FD (id, one))
--   Def. av applyFD c (FD (f, f')) = Bi (f c, f' c)
-- Bi (id c, one c)
--    Förenklat
-- Bi (c, one)
xBi :: Ring a => a -> Bi a
xBi c = Bi (c, one)

instance Additive a   => Additive (FD a) where  (+) = addFD;  zero = zeroFD
instance Ring a => Multiplicative (FD a) where  (*) = mulFD;  one = oneFD
instance AddGroup a   => AddGroup (FD a) where  negate = negateFD

addFD :: Additive a => FD a -> FD a -> FD a
addFD (FD (f, f')) (FD (g, g')) = FD (f+g, f'+g')
  -- (+) :: (a->a) -> (a->a) -> (a->a)
negateFD :: AddGroup a => FD a -> FD a
negateFD (FD (f, f')) = FD (negate f, negate f' )

zeroFD :: Additive a => FD a
zeroFD = FD (zero,zero)  -- zero :: Additive a => a -> a

oneFD :: Ring a => FD a
oneFD = FD (one, zero)     -- (constant function returning 1, its derivative)

mulFD :: Ring a => FD a -> FD a -> FD a
mulFD (FD (f, f')) (FD (g, g')) = FD (f*g, f'*g + f*g')
  -- (*) :: (a->a) -> (a->a) -> (a->a)
\end{code}

----------------
2. Numeric instances for Bi

\begin{code}
instance Additive a   => Additive (Bi a) where  (+) = addBi;  zero = zeroBi
instance Ring a => Multiplicative (Bi a) where  (*) = mulBi;  one = oneBi
instance AddGroup a   => AddGroup (Bi a) where  negate = negateBi


-- Spec.: forall c. H2(applyFD c, mulFD, mulBi)
--   forall c :: a. forall fd1, fd2 :: FD a.
--       applyFD c (mulFD fd1 fd2) == mulBi (applyFD c fd1) (applyFD c y)

mulBi :: Ring a => Bi a -> Bi a -> Bi a
mulBi (Bi (x, x')) (Bi (y, y')) = Bi (x*y, x'*y + x*y')

addBi :: Additive a => Bi a -> Bi a -> Bi a
addBi = error "TODO"

negateBi :: AddGroup a => Bi a -> Bi a
negateBi = error "TODO"

zeroBi :: Additive a => Bi a
zeroBi = error "TODO"

oneBi :: Ring a => Bi a
oneBi = error "TODO"
\end{code}

H2(applyFD c, mulFD, mulBi)
forall fd1, fd2. applyFD c (mulFD fd1 fd2) == mulBi (applyFD c fd1) (applyFD c fd2)

Use equational reasoning. (We can even check the types in Haskell.)

Start from the right-hand side (RHS)
\begin{spec}
lemma :: Ring a => a -> FD a -> FD a -> [Bi a]
lemma c fd1 fd2 = 
  [ mulBi (applyFD c fd1) (applyFD c fd2)
  , applyFD c (mulFD fd1 fd2)
  ]
\end{spec}

Step 1: expand  fd1 = FD (f, f')  and  fd2 = FD (g, g')
  ("poor-man's equational resoning" - steps are not checked.)

\begin{code}
lemma :: Ring a => a -> FD a -> FD a -> [Bi a]
lemma c (FD (f, f')) (FD (g, g')) = 
  let x = f c; y = g c; x' = f' c; y' = g' c in
  [  applyFD c (mulFD (FD (f, f')) (FD (g, g')))
  -- Def. av mulFG  (med f, g :: a->a )
  ,  applyFD c (FD (f*g, f'*g + f*g')) 
  -- Def. applyFD c (FD (k, k')) = Bi (k c, k' c)
  ,  Bi ((f*g) c, (f'*g + f*g') c)
  -- Def. (f*g) c = f c * g c; (f+g) c = f c + g c 
  ,  Bi ((f c)*(g c), (f'*g) c + (f*g') c)
  -- Simplify further
  ,  Bi ((f c)*(g c), (f' c)*(g c) + (f c)*(g' c)) -- bara "vanliga operatorer kvar"
  -- Shorter names
  ,  Bi (x*y, x'*y + x*y') -- bara "vanliga operatorer kvar"
  -- By def. of  mulBi (Bi (x, x')) (Bi (y, y')) = Bi (x*y, x'*y + x*y')
  ,  mulBi (Bi (x, x')) (Bi (y, y'))
  -- Shorter names
  ,  mulBi (Bi (f c, f' c)) (Bi (g c, g' c))
     -- Def. applyFD
  ,  mulBi (applyFD c (FD (f, f')))
           (applyFD c (FD (g, g')))
  ]
\end{code}

  ,  mulBi (Bi (x, x')) (Bi (y, y'))


Extra material:
\begin{code}
instance Field a      => MulGroup (FD a) where  recip = recipFD
instance Field a      => MulGroup (Bi a) where  recip = recipBi

recipFD :: Field a => FD a -> FD a
recipFD (FD (f, f')) = let rf = recip f in FD (rf, negate (rf*rf) * f')

recipBi :: Field a => Bi a -> Bi a
recipBi (Bi (f, f')) = let rf = recip f in Bi (rf, negate (rf*rf) * f')

----------------

simpBi :: Bi FunExp -> Bi FunExp
simpBi (Bi (f, f')) = Bi (simplify f, simplify f')

----------------

lemmaMulApplyFD :: Ring a => a -> FD a -> FD a -> [Bi a]
lemmaMulApplyFD c (FD (f, f')) (FD (g, g')) =
  let x = f c; x' = f' c; y = g c; y' = f' c in
  [ mulBi (applyFD c (FD (f, f'))) (applyFD c (FD (g, g')))
  , -- Def. of applyFD c (FD (f, g)) = Bi (f c, g c)
    mulBi (Bi (f c, f' c)) (Bi (g c, f' c))
  , -- invent simpler names (see let above)
    mulBi (Bi (x, x')) (Bi (y, y'))
  , -- Use this as the definition of mulBi!
    Bi (x * y, (x' * y)  +  (x * y'))
  , -- Use shorter names
    Bi (f c * g c, (f' c * g c)  +  (f c * g' c))
  , -- Def. (*) and (+) for functions
    Bi ((f*g) c, (f'*g + f*g') c)
  , -- Def. applyFD c (FD (f, g)) = Bi (f c, g c)
    applyFD c (FD (f*g, f'*g + f*g'))
  , -- Def. of mulFD (FD (f, f')) (FD (g, g')) = 
    applyFD c (mulFD (FD (f, f')) (FD (g, g')))
  ]

mulBiStep (Bi (x, x')) (Bi (y, y')) = Bi (x * y, (x' * y)  +  (x * y'))
\end{code}
