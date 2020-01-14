\begin{code}
{-# LANGUAGE GADTs #-}
module Live_2_1 where
import Prelude hiding (Monoid)
import DSLsofMath.AbstractFOL
\end{code}

Propositional Calculus = PropCalc = PC

\begin{code}
type Name = String
data PC where
  And   :: PC -> PC -> PC
  Or    :: PC -> PC -> PC
  Impl  :: PC -> PC -> PC
  Not   :: PC       -> PC
  Name  :: Name     -> PC
  Con   :: Bool     -> PC

e3 :: PC
e3 = And (Name "a") (Not (Name "a"))
e3' = Con False

eval :: PC -> Sem
eval (And x y)  = ands  (eval x) (eval y)
eval (Or  x y)  = ors   (eval x) (eval y)
eval (Impl x y) = impls (eval x) (eval y)
eval (Not x )   = nots  (eval x)
eval (Name n)   = names n -- lookup n in the table
eval (Con  c)   = cons c


-- type Table = [(Name, Bool)] -- translate names to Booleans
type Table = Name -> Bool
type Sem = Table -> Bool  -- (Name -> Bool) -> Bool
impls :: Sem -> Sem -> Sem
impls f g = \l -> impl (f l) (g l)
    -- f :: Sem = Table -> Bool
    -- l :: Table = Name -> Bool, _ :: Bool

[ors, ands, nots] = error "TODO: ors, ands, nots"

        -- PC -> PC -> PC
impl :: Bool -> Bool -> Bool
impl False  x = True
impl True   x = x


names :: Name -> Sem   -- Sem = Table -> Bool
names n = \l -> l n    -- l :: Table = Name -> Bool
-- names n l = l n    -- l :: Table = Name -> Bool

cons :: Bool -> Sem
cons b = \_ -> b

--  And   :: PC -> PC -> PC
--  Or    :: PC -> PC -> PC

--  Not   :: PC       -> PC
--  Name  :: Name     -> PC
--  Con   :: Bool     -> PC


\end{code}














\begin{spec}
type VarT = String
data QQ where
  QV     :: VarT      -> QQ
  FromI  :: Integer   -> QQ
  QPlus  :: QQ -> QQ  -> QQ
  QDiv   :: QQ -> QQ  -> QQ
 deriving Show

type PSym = String
data FOL where
  PName :: PSym -> [QQ]      -> FOL
  Equal :: QQ   -> QQ        -> FOL
  -- |Equal r1 r2|  means   |PName "Equal" [r1,r2]|

  And      :: FOL   ->  FOL  -> FOL
  Or       :: FOL   ->  FOL  -> FOL
  Implies  :: FOL   ->  FOL  -> FOL
  Not      :: FOL            -> FOL

  FORALL   :: VarT  ->  FOL  -> FOL
  EXISTS   :: VarT  ->  FOL  -> FOL
 deriving Show

commPlus :: FOL
commPlus = FORALL "x" (FORALL "y"
             (Equal  (QPlus (QV "x") (QV "y"))
                     (QPlus (QV "y") (QV "x"))))
\end{spec}

A more mathematical view of |commPlus| would be:

   ∀ x. ∀ y. (x + y) == (y + x)

where x, y ∈ QQ

Note the difference between the return type of |==| and |+|:
\begin{spec}
  (==) : QQ -> QQ -> FOL
  (+)  : QQ -> QQ -> QQ
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
  assume r : QQ (without any preconceptions)
  prove P(r)
  \ r -> proof of P(r)

-- Q: how do we represent something infinite? A: by a function!

Example:

  even : Nat -> Bool  -- this function is an infinite "thing"

----------------

In the third part we started looking at a way of using the type
checker in Haskel as a ``poor mans proof assistant''. The code is in
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
