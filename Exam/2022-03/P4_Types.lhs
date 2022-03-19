----------------
(a) [7p] Define the first-order logic predicates |LimAtInf(f,L)|
  and |LimSeq(a,L)| encoding the quotes. Explain the differences.

LimAtInf (f, L) = Forall eps. eps > 0 => Exists R. Forall x. x>R => x `elem` Dom f  & abs (f x - L) < eps
LimSeq   (a, L) = Forall eps. eps > 0 => Exists N. Forall n. n>N =>                   abs (a n - L) < eps

Similarities: the Forall-Exists-Forall pattern the abs (y - L) < eps pattern.
Differences: the type of
  f:REAL->REAL vs. a:Natural->REAL;
  R:REAL       vs. N:Integer;
  x:REAL       vs. n:Integer;
  The restriction to x `elem` Dom f
which actually should also be part of LimSeq (otherwise a may be called with negative arguments).

----------------
(b) [8p] Give the types of \(f\), \(x\), \(L\), \(\epsilon\),
  \(R\), |LimAtInf| and give the types of |a|, |L|, |n|, |N|,
  |epsilon|, |LimSeq|. Explain your reasoning.


type REAL = Double
type X    = REAL -- the domain of f could be some subset of REAL
type RPos = REAL -- Positive real numbers

f   :: REAL->REAL
eps :: RPos
L   :: REAL   
R   :: REAL
x   :: X
LimAtInf :: (REAL -> REAL, REAL) -> FOL

a   :: Nat->REAL
L   :: REAL
n   :: Nat
N   :: Nat
eps :: RPos
LimSeq :: (Nat->REAL, REAL) -> FOL


Below are some fake definitions to help typecheck the code (not
required on the exam).

\begin{code}
forall, exists :: (a->b) -> b
(forall, exists, (==>), elemDom) = error "just for type-checking"
-- TODO remove (forall, exists, elemDom) = error "just for type-checking"
infixr 4 ==>
(==>) :: FOL -> FOL -> FOL
-- x ==> y = "("++x++")=>("++y++")"
elemDom :: a -> (a->b) -> FOL
type FOL = Bool
type REAL = Double
type X = REAL -- the domain of f could be some subset of REAL
type RPos = REAL -- Positive real numbers

pLimAtInf :: (REAL -> REAL, REAL) -> FOL
pLimAtInf (f, bL) = forall (\eps -> (eps > 0) ==> 
                      exists (\bR ->
                        forall (\x -> (x > bR) ==>
                           let _dummy = (eps :: RPos, bR :: REAL, x :: X)
                           in 
                              (x `elemDom` f  && abs (f x - bL) < eps)
                     )))

type Nat = Integer -- should not contain negative numbers

pLimSeq :: (Nat->REAL, REAL) -> FOL
pLimSeq   (a, bL) = forall (\eps -> (eps > 0) ==>
                      exists (\bN ->
                        forall (\n -> (n > bN) ==>
                          let _dummy = (eps :: RPos, bN :: Nat, n :: Nat)
                          in 
                            (n `elemDom` a  && abs (a n - bL) < eps)
                    )))


\end{code}

----------------
(c) [5p] In both cases the idea of ``divergence'' is introduced in
  the same manner: let |DivF(f) = not (Exists L (LimAtInf(f,L)))| and
  |DivS(a) = not (Exists L (LimSeq(a,L)))|.
  %
  Simplify the predicate |DivF(f)| by pushing the negation through all
  the way.


DivF(f)
= not (Exists L (LimAtInf(f,L)))
= Forall L (not (LimAtInf(f,L)))
= Forall L (Exists (eps > 0) not (Exists R. Forall x. x>R => x `elem` Dom f  & abs (f x - L) < eps))
= Forall L (Exists (eps > 0) (Forall R. not (Forall x. x>R => x `elem` Dom f  & abs (f x - L) < eps)))
= Forall L (Exists (eps > 0) (Forall R. Exists x>R. not (x `elem` Dom f  & abs (f x - L) < eps)))
= Forall L (Exists (eps > 0) (Forall R. Exists x>R. (x `notElem` Dom f  | abs (f x - L) >= eps)))

----------------
(d) [5p] Prove |Forall a (DivS(a) => DivF((/2) . a . round))|.

A proof of this can be seen as a function from any series a and a
proof that it diverges, to a proof that f = (/2) . a . round
diverges. Informally it goes as follows: Pick an arbitrary a and
assume it diverges. We need to prove Forall L. not (LimAtInf(f,L)).
Pick an arbitrary L. From the divergence of the series a we know in
particular that it does not converge to 2*L. But then f n = (a n) / 2
cannot converge to 2*L/2 = L. QED.

TODO: clean-up the details of the more formal proof below

prf a diva = divf
  where    divf :: Forall L2 (Exists (eps2 > 0) (Forall R. Exists x>R. (x `notElem` Dom f  | abs (f x - L2) >= eps2)))

we can use diva :: Forall L1 (Exists (eps1 > 0) (Forall N. Exists n>N. (n `notElem` Dom a  | abs (a n - L1) >= eps1)))
freely

divf L = (eps2, pf)
  where (eps1, pa) = diva (2*L)
         -- with pa :: Forall N. Exists n>N. (n `notElem` Dom a  | abs (a n - 2*L) >= eps1)))
        eps2 = 2*eps2
        pf R = let (n, Right q) = pa (round (R+1)) -- now we know  abs (a n - 2*L) >= eps1
                   q' = {- see Lemma below -}
               in (fromIntegral n, Right q')

Lemma: evaluate the condition at the core of DivF f:

  abs (f x - L) >= eps2
=
  abs (((/2) . a . round) x - L) >= eps2
=
  abs ((a (round x))/2 - L) >= eps2

We know from pa that
  abs (a n - 2*L) >= eps1
for some n > N (where we get to pick the N freely)

Then take x = fromIntegral n so that round x = n
which means a (round x) = a n
Now
  abs ((a n)/2 - L)
= abs (a n - 2*L)/2
>= eps1 / 2
so we can take eps2 = eps1/2 (or eps2 = eps1 > eps1/2)




