\item {[25pts]} \textbf{Homomorphisms.}

Consider the following predicates:

|h : A -> B| is a homomorphism from |E : A| to |e : B|

  H0(h,E,e)    =  h E == e

|h : A -> B| is a homomorphism from |F : A -> A| to |f : B -> B|

  H1(h,F,f)    =  Forall x (h(F x) == f (h x))

|h : A -> B| is a homomorphism from |Op : A->A->A| to |op : B->B->B|

  H2(h,Op,op)  =  Forall x (Forall y (h(Op x y) == op (h x) (h y)))

And the following definitions of |REAL| and |CC| as vector spaces:

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module P4 where
type REAL = Double
class Vector v where
  zero   :: v
  add    :: v -> v -> v
  scale  :: REAL -> (v -> v)

type CC = ComplexSem REAL
instance Vector REAL  where zero = 0;      add = (+);   scale = (*)
instance Vector CC    where zero = zeroC;  add = addC;  scale = scaleC

newtype  ComplexSem r  =  CS  (r , r)        deriving (Eq, Show)
i = CS (0,1)
zeroC = CS (0, 0)
addC (CS (x1,y1)) (CS (x2,y2)) = CS (x1+x2,y1+y2)
scaleC r (CS (x,y)) = CS (r*x, r*y)

toComplex :: REAL -> ComplexSem REAL
toComplex r = CS (r, 0)

mulC (CS (x1,y1)) (CS (x2,y2)) = CS (x1*x2-y1*y2, x1*y2+x2*y1)
circle :: REAL -> CC
circle v = CS (cos v, sin v)
\end{code}

Prove or disprove the following claims:
4a H0(toComplex,zero,zero)
4b H2(toComplex,add,add)
4c H2(toComplex,scale,scale)
4d H2(circle,(+),mulC)
4e Exists (a, b) (H1(addC i,mulC a,mulC b))
-- end of exam questions
-- start of solutions

4a.
  H0(toComplex,zero,zero)
= -- def. of H0
  toComplex zero == zero
= -- def. of toComplex
  CS (zero, 0) == CS (0,0)
= -- def. of zero
  CS (0, 0) == CS (0,0)
= -- reflexivity
  True

4b.
  H2(toComplex,add,add)
= def. of H2, let t=toComplex for brevity
  Forall x (Forall y (t(x+y) == addC (t x) (t y)))
= def. of t
  Forall x (Forall y (CS (x+y,0) == addC (CS (x,0)) (CS (y,0)))
= def. of addC
  Forall x (Forall y (CS (x+y,0) == CS (x+y,0+0)))
= Simplification
  True

4c.
  H2(toComplex,scale,scale)
  -- type problem: toComplex :: REAL -> CC
  -- scaleRR :: REAL -> REAL -> REAL  -- OK
  -- scaleCC :: CC -> CC -> CC        -- not OK

\begin{spec}
typecheck x y = [t(scale x y) == scale (t x) (t y)] where t = toComplex
\end{spec}

4d.
  H2(circle,(+),mulC)
= -- def. of H2, shorten circle to just c
  Forall x (Forall y (c(x+y) == mulC (c x) (c y)))
= -- def. of c
  Forall x (Forall y (  CS (cos (x+y), sin (x+y)) ==
                        mulC (CS (cos x, sin x)) (CS (cos y, sin y))))
= -- def. of mulC
  Forall x (Forall y (  CS  (cos (x+y), sin (x+y)) ==
                        CS  (  (cos x)*(cos y) - (sin x)*(sin y)
                            ,  (sin x)*(cos y) + (cos x)*(sin y) )))
= -- def. == for Complex
  Forall x (Forall y (    cos (x+y) == (cos x)*(cos y) - (sin x)*(sin y)
                      &&  sin (x+y) == (sin x)*(cos y) + (cos x)*(sin y) ))
= -- trigonometry


4e.

Here is one way to work towards a solution. There are other ways.

  Exists a, b. H1(addC i,mulC a,mulC b)
-- It looks suspicious, let's try to negate it.
  Forall a, b.  Exists z. addC i (mulC a z) /= mulC b (addC i z)
-- Try with some simple values of z, starting with 0
  addC i (mulC a 0) /= mulC b (addC i 0)
= -- simplify
  i /= mulC b i
-- This is true for all b/=1
-- Thus we can pick z=0 for all a and almost all b.
-- What about when b=1?
  addC i (mulC a z) /= mulC b (addC i z)
=
  addC i (mulC a z) /= mulC 1 (addC i z)
=
  addC i (mulC a z) /= addC i z
=
  mulC a z /= z
= -- assume z/=0, for example z=1
  a /= 1
Thus we can pick z=1 when b=1 and a/=1.
What about a=b=1?
  addC i (mulC a z) /= mulC b (addC i z)
=
  addC i z /= addC i z
=
  False

Thus, we cannot prove the negation, the law seems to hold.

Check:

  Exists a, b. H1(addC i,mulC a,mulC b)
-- let a=b=1
  H1(addC i,mulC 1,mulC 1)
-- alg. properties
  H1(addC i,id,id)
-- expand H1
  addC i (id z) == id (addC i z)
-- simplify
  addC i z == addC i z
-- simplify
  True

Thus, yes, there exists a and b, both 1, so that addC i is a homomorphism.
