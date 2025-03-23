\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
module Solution_202503_Algebra where
import Prelude((.), error, Bool(False,True), all, Eq((==)),
               Enum, String, Bounded(minBound,maxBound), Num,
               Double, Show, map)
import qualified Prelude
\end{code}

[25p] \textbf{Algebraic structure: a DSL for vector spaces} 

  A vector space over |ℝ| is a set |V| together with a constant (or
  nullary) operation |0 : V| (called ``zero''), an operation |(+) : V
  -> V -> V| (called ``add''), an operation |(-) : V -> V| (called
  ``negate'') and an operation |(⋅) : ℝ -> V -> V| (called ``scale''),
  such that [...]

* a) [5p] Define a type class |Vector| that corresponds to the structure ``vector space over |ℝ|''.

\begin{code}
type REAL = Double
class Vector v where
  zero   :: v
  add    :: v -> v -> v
  negate :: v -> v
  scale  :: REAL -> (v -> v)

  -- Default definition (not required on the exam, but convenient)
  negate = scale (-1)    -- Prop: forall v.   v + negate v == zero
\end{code}

* b) [5p]
  Define a datatype |VecSyn a| for the language of vector space
  expressions (with variables of type |a|) and define a |Vector|
  instance for it. (These are expressions formed from applying the
  vector operations to the appropriate number of arguments, e.g.,
  all the left hand sides and right hand sides of the above
  equations.)

\begin{code}
data VecSyn a where
  Zero   :: VecSyn a
  Add    :: VecSyn a -> VecSyn a -> VecSyn a
  Negate :: VecSyn a -> VecSyn a
  Scale  :: REAL -> VecSyn a -> VecSyn a
  V      :: a -> VecSyn a
 deriving Show

instance Vector (VecSyn a) where
  zero    = Zero
  add     = Add
  negate  = Negate
  scale   = Scale
\end{code}

* c) [5p]:
  Find and implement two other instances of the |Vector|
  class. Make sure the laws are satisfied.  \label{item:inst}

\begin{code}
-- REAL is a 1-dim. vectorspace
instance Vector REAL where
  zero   = 0
  add    = (Prelude.+)
  negate = Prelude.negate
  scale  = (Prelude.*)
-- laws are satisified because REAL is a Field. (Note: REAL cannot
-- really be implemented as Double, but that is not the problem the
-- exam question ask for.)

-- functions (from any index type i) to a vector space, forms a vector space
instance Vector v => Vector (i->v) where
  zero   = zeroF
  add    = addF
  negate = negateF
  scale  = scaleF
-- Laws are satisified mostly by "lifting" properties from Vector v to Vector (i->v)

zeroF :: Vector v => i -> v
zeroF _i = zero

addF :: Vector v => (i->v) -> (i->v) -> (i->v)
addF f g = \i -> add (f i) (g i)

negateF :: Vector v => (i->v) -> (i->v)
negateF f = \i -> negate (f i)
-- or negateF = scaleF (-1)

scaleF :: Vector v => REAL -> (i -> v) -> (i -> v)
scaleF s f = \i -> scale s (f i)
-- or scaleF s f = scale s . f

-- A third possible instance: pairs of vectors
instance Vector v => Vector (v,v) where
  zero = zeroP
  add = addP
  negate = negateP
  scale = scaleP

zeroP :: Vector v => (v, v)
zeroP = (zero, zero)

addP :: Vector v => (v, v) -> (v, v) -> (v, v)
addP (x1, y1) (x2, y2) = (add x1 x2, add y1 y2)

negateP :: Vector v => (v, v) -> (v, v)
negateP (x,y) = (negate x, negate y)

scaleP :: Vector v => REAL -> (v,v) -> (v,v)
scaleP s (x,y) = (scale s x, scale s y)
\end{code}

* d) [5p]:
  Give a type signature for, and define, a general evaluator (on
  the basis of an assignment function) from |VecSyn a| expressions to
  any semantic type with a |Vector| instance.

\begin{code}
evalV :: Vector v => (a -> v) -> (VecSyn a -> v)
evalV tab = e where
  e Zero           = zero
  e (Add x y)      = add (e x) (e y)
  e (Negate x)     = negate (e x)
  e (Scale s x)    = scale s (e x)
  e (V a)          = tab a
\end{code}

* e) [5p]: Specialise evaluator and example expressions

\begin{code}
evalS :: (a->REAL) -> (VecSyn a -> REAL)
evalS = evalV

type TwoD = (->) Bool -- this means |TwoD a = (->) Bool a = Bool -> a|
evalVV :: Vector v => (a->TwoD v) -> (VecSyn a -> TwoD v)
evalVV = evalV

x1, y1, e1, e2, e3 :: VecSyn String
x1 = V "x"
y1 = V "y"
e1 = add zero x1
e2 = scale 2 (add e1 y1)
e3 = add e2 (scale (-2) y1)

tabS :: Num a => String -> a
tabS "x" = 1
tabS "y" = 7
tabS _   = error "tabS: unbound variable"

\end{code}
Below: More testing code here than needed on the exam

On the exam, I expect "hand-computation" in a few steps of the
results. The testing code is mostly to make sure Haskell agrees and to
check student solutions.

\begin{code}
toV :: (a,a) -> TwoD a
toV (x,y) = \b -> if b then x else y
toP :: TwoD a -> (a,a)
toP f = (f True, f False)

tabVV :: Num a => String -> TwoD a
tabVV "x" = toV (1,0)
tabVV "y" = toV (0,1)
tabVV _   = error "tabVV: unbound variable"

testS :: Bool
testS = map (evalS tabS) [e1, e2, e3] == [1, 16, 2]
lhsVV, rhsVV :: [(REAL,REAL)]
lhsVV = map (toP . evalVV tabVV) [e1, e2, e3]
rhsVV = [(1,0), (2,2), (2,0)]
testVV :: Bool
testVV = lhsVV == rhsVV

(=.=) :: (Bounded i, Enum i, Eq a) => (i->a) -> (i->a) -> Bool
v =.= w = all eqAt [minBound .. maxBound]
  where eqAt i = v i == w i
\end{code}
