In the first part of the live coding for lecture 2.1 we looked at the
implementation of first order logic syntax trees in Haskell and the
intended semantics. The code is below and in FOLRat.lhs (which is also
included in the lecture notes).

\begin{spec}
module Live_2_1 where

data RatT = RV String | FromI Integer | RPlus RatT RatT | RDiv RatT RatT
  deriving Show

data FOL  =  P String [RatT]
          |  Equal  RatT  RatT   -- Equal r1 r2  means   P "Equal" [r1,r2]

          |  And      FOL   FOL
          |  Or       FOL   FOL
          |  Implies  FOL   FOL
          |  Not      FOL

          |  FORALL  String  FOL
          |  EXISTS  String  FOL
  deriving Show

commPlus :: FOL
commPlus = FORALL "x" (FORALL "y" (Equal  (RPlus (RV "x") (RV "y"))
                                          (RPlus (RV "y") (RV "x"))))
\end{spec}

A more mathematical view of |commPlus| would be:

   ∀ x. ∀ y. (x + y) == (y + x)

where x, y ∈ Rat

Note the difference between the return type of |==| and |+|:
\begin{spec}
  (==) : Rat -> Rat -> FOL
  (+)  : Rat -> Rat -> Rat
\end{spec}

----------------------------------------------------------------

In the second part we talked about how to think about, and represent,
proofs.


If
  (p is a proof a P)
and
  (q is a proof of Q)
then
  (p,q) is a proof of (And P Q)

a proof of (And_n P1 ... Pn) is a proof of P1, ... , a proof of Pn

a proof of (Forall x P(x)) is a proof of P(r1) and a proof of r2 and ...
  an infinite collection of proofs
  a function from the rational number r to a proof of the propery P(r)

"rule of thumb"
  assume r : Rat (without any preconceptions)
  prove P(r)
  \ r -> proof of P(r)

-- Q: how do we represent something infinite? A: by a function!

Example:

  even : Nat -> Bool  -- this function is an infinite "thing"

----------------

In the third part we started looking at a way of using the type
checker in Haskel as a ``poor mans proof assistant''. The code is in
AbstractFOL.lhs and included in the lecture notes.

\begin{code}
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
\end{code}

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

Note: Most of the abstract types above can be given implentations. For
example:

\begin{code}
type MyAnd a b = (a,b)

myAndElimL = fst  :: (a,b) -> a
myAndElimR = snd  :: (a,b) -> b
\end{code}
