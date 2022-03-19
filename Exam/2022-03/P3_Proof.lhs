\begin{code}
import qualified Prelude

class Additive a where zero :: a; (+) :: a->a->a

mulE f1 f2  = f1 . f2               -- Def. mulE
addE f1 f2  = lift2 (+) f1 f2       -- Def. addE

f1 . f2        = \x -> f1 (f2 x)          -- Def. (.)
lift2 op f1 f2 = \x -> op (f1 x) (f2 x)   -- Def. lift2

type Endo a = a->a
mulE, addE :: Additive a => Endo a -> Endo a -> Endo a
(.)   :: (b->c) -> (a->b) -> (a->c)
lift2 :: (a->b->c) -> (t->a) -> (t->b) -> (t->c)
\end{code}

\item{[5p]} Define the additive unit |zeroE| of this ring and prove
  that it is the additive unit.

\begin{code}
zeroE :: Additive a => Endo a
zeroE = \_->zero

-- This shows that zeroE is a right-zero of addE. The left-zero proof is symmetric.
poorMansProof1 :: Additive a => Endo a -> [Endo a]
poorMansProof1 f =
  [ addE f zeroE
  , -- Def. addE
    lift2 (+) f zeroE
  , -- Def. lift2
    \x -> (+) (f x) (zeroE x)
  , -- Def. zeroE
    \x -> (+) (f x) zero
  , -- Additive group axiom
    \x -> f x
  , -- Simplify
    f
  ]
\end{code}

\item{[5p]} Define the multiplicative unit |oneE| of this ring and
    prove that it is the multiplicative unit.

\begin{code}
oneE :: Endo a
oneE x = x  -- or oneE = id

poorMansProof2 :: Additive a => Endo a -> [Endo a]
poorMansProof2 f =
  [ mulE f oneE
  , -- Def. mulE
    f . oneE
  , -- Def. (.)
    \x -> f (oneE x)
  , -- Def. oneE
    \x -> f x
  , -- Simplify
    f
  ]
\end{code}

\item{[10p]} Prove |Endo(f) => mulE f (addE g h) == addE (mulE f g)
    (mulE f h)| for all |f|, |g|, |h| of appropriate type. (This
    proves the distributivity law for the ring instance.)

\begin{code}
poorMansProof3 :: Additive a => Endo a -> Endo a -> Endo a -> [Endo a]
poorMansProof3 f g h =
  [ mulE f (addE g h) 
  , -- Def. mulE
    f . addE g h
  , -- Def. (.)
    \x -> f (addE g h x)
  , -- Def. addE
    \x -> f (lift2 (+) g h x)
  , -- Def. lift2
    \x -> f ((+) (g x) (h x))
  , -- Use Endo(f) = H2(f,(+),(+)) && H0(f,0,0)
    \x -> (+) (f (g x)) (f (h x))
  , -- Def. (.) twice
    \x -> (+) ((f . g) x) ((f . h) x)
  , -- Def. lift2
    lift2 (+) (f . g) (f . h)
  , -- Def. addE
    addE (f . g) (f . h)
  , -- Def. mulE twice
    addE (mulE f g) (mulE f h)
  ]
\end{code}


----------------
Not part of the exam question:

Closed under addition?

H2(h,(+),(+)) = forall a b. h(a+b) = h a + h b

\begin{code}
proof f g a b =
  let h = addE f g in
  [ h (a+b)
  , -- Def. h
    addE f g (a+b)
  , -- Def. addE
    lift2 (+) f g (a+b)
  , -- Def. lift2
    (f (a+b)) + (g (a+b))
  , -- Endo(f) && Endo(g)  -- the H2 part
    (f a + f b) + (g a + g b)
  , -- Assoc(+), Commut(+)  -- (+) must be commutative for this step to work
    (f a + g a) + (f b + g b)
  , -- Def. lift2&addE, twice
    (addE f g a) + (addE f g b)
  , -- Def. h twice
    h a + h b
  ]
\end{code}
