E2.2: Translate to Haskell and prove the De Morgan laws:

  ¬ (p ∨ q) ⟷  ¬p ∧ ¬q
  ¬ (p ∧ q) ⟷  ¬p ∨ ¬q

Solution:
First we translate the logic notation to types.
We can name some parts to reuse subterms.
\begin{code}
module E2_2_DeMorgan where
import DSLsofMath.AbstractFOL

type NotOr   p  q  =  Not (Or p q)          --  ¬ (p ∨ q)
type AndNot  p  q  =  And (Not p) (Not q)   --  ¬p ∧ ¬q
type NotAnd  p  q  =  Not (And p q)         --  ¬ (p ∧ q)
type OrNot   p  q  =  Or (Not p) (Not q)    --  ¬p ∨ ¬q
\end{code}

We have four implications to prove:

\begin{code}
no2an  :: Impl (NotOr   p  q) (AndNot  p  q)
an2no  :: Impl (AndNot  p  q) (NotOr   p  q)
na2on  :: Impl (NotAnd  p  q) (OrNot   p  q)
on2na  :: Impl (OrNot   p  q) (NotAnd  p  q)
\end{code}
We can start with the most concrete one: |an2no|, where we start from
a pair of proofs of |Not p| and |Not q|.
%
The idea is to collect proofs of simple statements in the hope of finding
some obvious contradition.
%
It helps to name subterms to keep track of what is already known.
\begin{code}
an2no = implIntro $ \npANDnq ->    -- npANDnq :: And (Not p) (Not q)
        let  np = andElimL npANDnq -- np      ::      Not p
             nq = andElimR npANDnq -- nq      ::              Not q
        in  notIntro  $ \pORq ->   -- pORq    :: Or       p       q
            orElim pORq  (\p -> exFalso $ andIntro p np)
                         (\q -> exFalso $ andIntro q nq)
\end{code}
In this case it takes us to the stage when we one contradiction in each
branch of the case over |pORq|.
%
We are not quite done here, because |orElim| (corresponding to a case
statement) requires both branches to have the same type.
%
But here we can appeal to "ex falso quodlibet" = from falsity, everything
follows:
\begin{code}
exFalso :: And q (Not q) -> p
exFalso qANDnq =  notElim (notIntro (\ np -> qANDnq))
\end{code}

Next we can look at the proof |on2na| which starts with either |Not p| or with |Not q|.
\begin{code}
on2na = implIntro $ \npORnq ->
        orElim npORnq f g

f :: Not p -> NotAnd p q
g :: Not q -> NotAnd p q
\end{code}
As we have two properties of the same shape we may suspect
that a lemma could be useful:
\begin{code}
notMap :: (a->b) -> (Not b -> Not a)
notMap a2b nb = notIntro $ \a ->     -- Assume |a|
                andIntro (a2b a) nb  -- show |b| and |Not b|
\end{code}
And suddenly the proof obligations turn out to be easy:
\begin{code}
f = notMap andElimL
g = notMap andElimR
\end{code}
Also the next theorem in line is simplified: we start from |Not| and want
to prove two |Not|s, thus we can use |notMap| twice:
\begin{code}
no2an = implIntro $ no2an'
no2an' :: NotOr p q -> AndNot p q
no2an' nOrpq = andIntro (notMap orIntroL nOrpq)
                        (notMap orIntroR nOrpq)
\end{code}

The final de Morgan theorem is |na2on| and, for a change, we will prove
that using one of the others and double negation:
%
\begin{code}
na2on = implIntro $ notElim . notMap (notElim2 . no2an')

notElim2 :: And (Not (Not p)) (Not (Not q)) -> And p q
notElim2 = mapAnd notElim notElim

mapAnd :: (a1->a2) -> (b1->b2) -> (And a1 b1 -> And a2 b2)
mapAnd f g pANDq = let  p = andElimL pANDq
                        q = andElimR pANDq
                   in andIntro (f p) (g q)
\end{code}

Note that there are many other variations possible ---
just make sure not to use a circular definition.
