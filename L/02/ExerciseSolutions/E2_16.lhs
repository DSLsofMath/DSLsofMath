\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
import DSLsofMath.AbstractFOL
\end{code}
The scoped type variables extension is only needed to give
types to sub-proofs defined in where clauses.

In order to obtain sound proofs, we can only use the following
functions to manipulate terms of types representing logical
formulas.

andIntro     ::  p -> q -> And p q
andElimL     ::  And p q -> p
andElimR     ::  And p q -> q

orElim       ::  Or p q -> (p -> r) -> (q -> r) -> r
orIntroL     ::  p -> Or p q
orIntroR     ::  q -> Or p q

implIntro    ::  (p -> q) -> Impl p q
implElim     ::  Impl p q -> p -> q

notIntro     ::  (p -> And q (Not q)) -> Not p
notElim      ::  Not (Not p) -> p

\begin{code}
ex21a :: Impl (And p q) q
ex21a = implIntro andElimR

ex21b :: Or p q -> Or q p
ex21b opq = orElim opq (\p -> orIntroR p) (\q -> orIntroL q)
\end{code}

Exercise |ex21b| is basically a machine-checked version of the following
pen-and-paper proof:

Assume that |p ∨ q| holds. We proceed by case distinction on this disjunction.

Case |p|: In this case, assume that |p| holds. Therefore, by rule |orIntroR| we know
that |r ∨ p| holds for any |r|; in particular, it holds for |r=q|, and hence |q ∨ p|
holds.

Case |q|: In this case, assume that |q| holds. Therefore, we know by rule |orIntroL|
that |q ∨ r| holds for any |r|, in particular also for |r=p|, therefore |q ∨ p| holds.

Exercise 2.1 c is a usuful "helper lemma" for later proofs.
%
It can be used to ``flip the implication arrow'' when we need to prove
something of the form |Not _ -> Not _|.
%
The first argument, |a2b|, is a function we can use to prove |b| given a proof of |a|.
%
The second argument, |nb|, is a proof of |Not b|.
%
The result should be a proof of |Not a|.
\begin{code}
notMap :: (a->b) -> (Not b -> Not a)
notMap a2b nb = notIntro  -- To show |Not a| by contradiction:
                  (\a ->                 -- assume |a|
                   andIntro (a2b a) nb)  -- show |b| and |Not b|

ex21d :: forall p. Or p (Not p)
ex21d = notElim $ nnlem -- We show that it is not the case that `Or p (Not p)`
                        -- is false
  where nnlem :: Not (Not (Or p (Not p)))
        nnlem = notIntro $ \nlem -> nlemToContradiction nlem
        -- we show this by assuming that Or p (Not p) is false
        -- in other words, Not (Or p (Not p)) holds. Using
        -- this assumption, we derive a contradiction. Therefore
        -- Not (Or p (Not p)) must be false; in other words,
        -- Not (Not (Or p (Not p))) holds.
        nlemToContradiction :: Not (Or p (Not p))
                            -> And p (Not p)
        -- Showing this involves two cases:
        -- 1. Showing that Not (Or p (Not p)) implies p
        -- 2. Showing that Not (Or p (Not p)) implies Not p
        nlemToContradiction nlem = andIntro (p nlem) (np nlem)
        p   :: Not (Or p (Not p)) -> p
        np  :: Not (Or p (Not p)) -> Not p
        -- We can show the second case using |notMap|:
        -- using |orIntroL :: p -> Or p (Not p)|
        np = notMap orIntroL
        -- To show |p| when assuming |Not (Or p (Not p))|, we need to
        -- show that |p| cannot be false; i.e. we show that |Not (Not p)|
        p nlem = notElim (nnp nlem)
        -- Again, |notMap| comes in handy, now with
        --   |orIntroR :: Not p -> Or p (Not p)|
        nnp :: Not (Or p (Not p)) -> Not (Not p)
        nnp = notMap orIntroR

-- Alternative solution via r = Or p (Not p) in the contradiction:
ex21d' :: forall p. Or p (Not p)
ex21d' = notElim $ nnlem
  where nnlem :: Not (Not (Or p (Not p)))
        nnlem = notIntro nlemToContradiction
        nlemToContradiction :: Not (Or p (Not p))
                            -> And (Or p (Not p)) (Not (Or p (Not p)))
        nlemToContradiction nlem = andIntro (f nlem) nlem
        f :: Not (Or p (Not p)) -> Or p (Not p)
        f nlem = orIntroR (g nlem)
        g :: Not (Or p (Not p)) -> Not p
        g = notMap orIntroL
\end{code}
