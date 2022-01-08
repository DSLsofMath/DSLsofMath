{-# LANGUAGE EmptyCase #-}
module E2_5 where
data Empty
type Not p = p -> Empty
type Fals = Empty
type Tru = Not Fals

idEmpty:: Empty -> Empty -- Tru
idEmpty evE = evE

type Or = Either
type And = (,)
-- notIntro:: (p -> (q, q -> Empty)) -> (p -> Empty)
notIntro:: (p -> And q (Not q)) -> Not p
notIntro f x = y
     where (a, b) = f x
           y = b a



contraHey:: Empty -> p
contraHey evE = case evE of {}

ex1 :: (q, q -> Empty) -> p
ex1 (a,b) = contraHey (b a)

-- ¬ p ∨ ¬ q  → ¬ (p ∧ q)
ex2 :: Or (Not p) (Not q) -> Not (And p q)
ex2 (Left np)   = notIntro (\(p,_) -> (p,np))
ex2 (Right nq)  = notIntro (\(_,q) -> (q,nq))
