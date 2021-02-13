----------------------------------------------------------------

In the second part we talked about how to think about, and represent,
proofs.


If
  (p is a proof of P)
and
  (q is a proof of Q)
then
  (p,q) is a proof of (And P Q)

a proof of (And_n P1 ... Pn) is a proof of P1, ... , a proof of Pn

a proof of (Forall x P(x)) is a proof of P(r1) and a proof of P(r2) and ...
  an infinite collection of proofs
  a function from the rational number r to a proof of the propery P(r)

"rule of thumb"
  assume r : QQ (without any preconceptions)
  prove P(r)
  \ r -> proof of P(r)

-- Q: how do we represent something infinite? A: by a function!

Example:

  even : Nat -> Bool  -- this function is an infinite "thing"

----------------

In the third part we started looking at a way of using the type
checker in Haskell as a ``poor mans proof assistant''. The code is in
AbstractFOL.lhs and included in the lecture notes.

\begin{spec}
module DSLsofMath.AbstractFOL where

data And   p  q;                       data Or    p  q
data Impl  p  q;                       data Not   p

andIntro   ::  p -> q -> And p q;      orElim     ::  Or p q -> (p -> r) -> (q -> r) -> r
andElimL   ::  And p q -> p;           orIntroL   ::  p -> Or p q
andElimR   ::  And p q -> q;           orIntroR   ::  q -> Or p q

implIntro  ::  (p -> q) -> Impl p q;   notIntro   ::  (p -> And q (Not q)) -> Not p
implElim   ::  Impl p q -> p -> q;     notElim    ::  Not (Not p) -> p

andIntro = u; orElim = u; andElimR = u; orIntroL = u; andElimL = u; orIntroR = u;
implIntro = u; notElim = u; notIntro = u; implElim = u; u = undefined;
\end{spec}

The idea here is to see proofs as ``proof terms'', built up from the
API above. We take a formula, express it as a type using the abstract
type constructors above and try to find an expression of that type
only using the API functions. If we succeed, we have found a proof of
the formula.

\begin{code}
p1 :: Impl a a   --   a => a
p1 = implIntro (\proofOfa -> proofOfa)  -- (a -> a) -> Impl a a

p2, p2' :: Impl (And a b) b
p2  = implIntro (\aAndb -> andElimR aAndb)   -- And a b   ->   b
p2' = implIntro andElimR
\end{code}

There are a number of exercises for you to practice on in the chapter.
If they are hard, try to formulate simpler ones as lemmas and build up
a small library of proof components. In that way you can get a better
understanding, and reuse earlier results in later proofs. [This is
called ``theory exploration'' and is also used in automatic theorem
provers.]

Note: Most of the abstract types above can be given implementations.
For example:

\begin{code}
type MyAnd a b = (a,b)

myAndElimL = fst  :: MyAnd a b -> a
myAndElimR = snd  :: MyAnd a b -> b
\end{code}
