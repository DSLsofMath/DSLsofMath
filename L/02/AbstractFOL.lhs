%if abstractfol
% (  And, Or, Impl, Not, notIntro, notElim, implIntro, implElim,
%      andIntro, andElimL, andElimR,  orIntroL, orIntroR, orElim
%   )
\begin{code}
module DSLsofMath.AbstractFOL where

data And   p  q;                                     data Or    p  q
data Impl  p  q;                                     data Not   p

andIntro       ::  p -> q -> And p q;                orElim       ::  Or p q -> (p -> r) -> (q -> r) -> r
andElimL       ::  And p q -> p;                     orIntroL     ::  p -> Or p q
andElimR       ::  And p q -> q;                     orIntroR     ::  q -> Or p q

implIntro      ::  (p -> q) -> Impl p q;{-"\quad"-}  notIntro     ::  (p -> And q (Not q)) -> Not p
implElim       ::  Impl p q -> p -> q;               notElim      ::  Not (Not p) -> p

andIntro = u; orElim = u; andElimR = u; orIntroL = u; andElimL = u; orIntroR = u;
implIntro = u; notElim = u; notIntro = u; implElim = u; u = undefined;
\end{code}
%endif
%\end{code}
%\begin{code}
%if tupling
\paragraph{Revisiting the tupling transform}
%
In exercise \ref{exc:tuplingE1}, the ``tupling transform'' was
introduced, relating a pair of functions to a function returning a
pair.
%
(Please revisit that exercise if you skipped it before.)
%
There is a logic formula corresponding to the type of the tupling
transform:
%
\begin{spec}
  (a => (b&c)) <=> (a=>b)&(a=>c)
\end{spec}
% Forall a (Forall b (Forall c ((a -> (b, c)) <=> (a->b, a->c) )))
The proof of this formula closely follows the implementation of the transform.
%
Therefore we start with the two directions of the transform as functions:

\begin{code}
test1' :: (a -> (b, c)) -> (a->b, a->c)
test1' = \a2bc ->  ( \a -> fst (a2bc a)
                   , \a -> snd (a2bc a) )

test2' :: (a->b, a->c) -> (a -> (b, c))
test2' = \fg -> \a -> (fst fg a, snd fg a)
\end{code}

Then we move on to the corresponding logic statements with proofs.
%
Note how the functions are ``hidden inside'' the proof.

\begin{code}
test1  ::  Impl (Impl a (And b c)) (And (Impl a b) (Impl a c))
test1  =   implIntro (\a2bc ->
             andIntro  (implIntro (\a -> andElimL  (implElim a2bc a)))
                       (implIntro (\a -> andElimR  (implElim a2bc a))))

test2  ::  Impl (And (Impl a b) (Impl a c)) (Impl a (And b c))
test2  =   implIntro (\fg ->
             implIntro (\a ->
               andIntro
                 (implElim (andElimL  fg) a)
                 (implElim (andElimR  fg) a)))
\end{code}
%endif
