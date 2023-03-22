\begin{code}

import qualified Prelude
import Prelude (Integer, map)
import DSLsofMath.Algebra

-- a) A chain of equational reasoning steps:
badAddProof :: [Poly Integer]
badAddProof =
  [ -- [1,7,3] + [8]
    -- instance declaration + syntactic sugar
    addLBad (1:[7,3]) (8:[])
  , -- def. of (+) = addL
    (1+8) : addLBad [7,3] []
  , -- def. of (+) = addLBad
    (1+8) : []
  , -- syntactic sugar
    [9]
  ]

{- The problem is that the first two cases implement the bad algebraic
   rules 0+p=0 and p+0=0.
-}

-- b) correct addL
addL :: Additive a => [a] -> [a] -> [a]
addL []      bs      = bs    -- 0+p=p
addL as      []      = as    -- p+0=p
addL (a:as)  (b:bs)  = (a+b) : addL as bs

{- The two first cases are adjusted to implement the correct algebraic
   rules: 0+p=p and p+0=p. -}

-- c) A chain of equational reasoning steps
badMulProof :: [Poly Integer]
badMulProof =
  [
--    [1,7]*one
    -- syntactic suger for lists
    mulLBad (1:[7]) one
  , -- def. mulL
    addL (scaleL 1 one) (mulL [7] one)
  , -- prop. of scaleL 1, def. of mulL
    addL one (addL (scaleL 7 one) (mulL [] one))
  , -- def. of oneL, scaleL, def. of mulL
    addL [1] (addL [7] [])
  , -- spec. of addL, twice, 1+7==8
    [8]
  ]

{- The last case of mulLBad forgets to "shift" the recursive call
   coefficients up one step.
-}

-- d) correct mulL
mulL :: Ring a => [a] -> [a] -> [a]
mulL []      bs  = []    -- 0*p = 0
mulL as      []  = []    -- p*0 = 0
mulL (a:as)  bs  = addL (scaleL a bs) (zero:mulL as bs)

motivation :: Ring a => a -> Poly a -> Poly a -> [Poly a]
motivation a as bs =
  [
    (a:as)*bs
  , -- = def. of (+) backwards
    ([a]+(zero:as))*bs
  , -- = distributivity
    ([a]*bs)+((zero:as)*bs)
  , -- = mult. by constant is scaling, "zero:"-lemma
    (scaleL a bs) + (zero:(as*bs))
  , -- = instance declaration
    addL (scaleL a bs) (zero : mulL as bs)
  ]

-- "zero:"-lemma: (zero:as)*bs == zero : (as*bs)

-- Alt. solution
mulL2 :: Ring a => [a] -> [a] -> [a]
mulL2 []      bs  = []    -- 0*p = 0
mulL2 as      []  = []    -- p*0 = 0
mulL2 (a:as)  (b:bs)  = (a*b) : addL  (scaleL a bs)
                                      (mulL2 as (b:bs))


----------------------------------------------------------------
-- Given in the exam question:
type Poly a = [a]

instance Additive a  => Additive        (Poly a) where  zero  = zeroL;  (+)  = addL
instance Ring a      => Multiplicative  (Poly a) where  one   = oneL;   (*)  = mulL
zeroL  :: [a];                zeroL  = []
oneL   :: Ring a => [a];      oneL   = [one]

addLBad :: Additive a => [a] -> [a] -> [a]
addLBad []      bs      = []
addLBad as      []      = []
addLBad (a:as)  (b:bs)  = (a+b) : addLBad as bs

mulLBad :: Ring a => [a] -> [a] -> [a]
mulLBad []      bs  = []
mulLBad as      []  = []
mulLBad (a:as)  bs  = addL (scaleL a bs) (mulLBad as bs)

scaleL :: Ring a => a -> [a] -> [a]
scaleL c = map (c*)
\end{code}
