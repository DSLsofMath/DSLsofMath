\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
\end{code}

a) [6p]

\begin{code}
type S = Double
class Vector v where
  zero   :: v

  add    :: v -> v -> v
  scale  :: S -> (v -> v)

  -- Can be included (but does not have to)
  addInv :: v -> v
  addInv = scale (-1)    -- Prop: forall v.   v + addInv v == zero
\end{code}

b) [6p]

\begin{code}
data VecSyn a where
  Zero   :: VecSyn a
  Add    :: VecSyn a -> VecSyn a -> VecSyn a
  Scale  :: S -> VecSyn a -> VecSyn a
  V      :: a -> VecSyn a
 deriving Show

instance Vector (VecSyn a) where
  zero  = Zero
  add   = Add
  scale = Scale
\end{code}

c) [6p]: Another two instances

\begin{code}
instance Vector S where
  zero   = 0
  add    = (+)
  scale  = (*)

instance Vector v => Vector (i->v) where
  zero   = const zero
  add    = \f g i -> add (f i) (g i)
  scale  = \s f -> scale s . f
\end{code}

d) [6p]:
\begin{code}
evalV :: Vector v => (a -> v) -> (VecSyn a -> v)
evalV tab = e where
  e Zero = zero
  e (Add x y) = add (e x) (e y)
  e (Scale s x) = scale s (e x)
  e (V a) = tab a
\end{code}


e) [6p]: Specialise evaluator and example expressions

\begin{code}
evalS :: (a->Double) -> (VecSyn a -> Double)
evalS = evalV

type TwoD = (->) Bool
evalVV :: Vector v => (a->TwoD v) -> (VecSyn a -> TwoD v)
evalVV = evalV

x, y, e1, e2, e3 :: VecSyn String
x = V "x"
y = V "y"
e1 = add zero x
e2 = scale 2 (add e1 y)
e3 = add e2 (scale (-2) y)

tabS :: Num a => String -> a
tabS "x" = 1
tabS "y" = 7

toV :: (a,a) -> TwoD a
toV (x,y) = \b -> if b then x else y
toP :: TwoD a -> (a,a)
toP f = (f True, f False)

tabVV :: Num a => String -> TwoD a
tabVV "x" = toV (1,0)
tabVV "y" = toV (0,1)

testS = map (evalS tabS) [e1, e2, e3] == [1, 16, 2]
lhsVV :: [(S,S)]
lhsVV = map (toP . evalVV tabVV) [e1, e2, e3]
rhsVV = [(1,0), (2,2), (2,0)]
testVV = lhsVV == rhsVV

(=.=) :: (Bounded i, Enum i, Eq a) => (i->a) -> (i->a) -> Bool
v =.= w = all eqAt [minBound .. maxBound]
  where eqAt i = v i == w i
\end{code}
