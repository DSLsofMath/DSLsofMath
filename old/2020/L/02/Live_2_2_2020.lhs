* Live coding part of week 2, lecture 2.

Reminder of learning outcomes

* organize areas of mathematics in DSL terms
    * "develop adequate notation for mathematical concepts"
    * "perform calculational proofs"

Repetition/reminder: Lecture 2.1 covered

* Propositional calculus (as a DSL)
    * Boolean logic without quantifiers (And, Or, Implies, Not, False, True) but with named atoms
    * a syntax tree datatype |PC|
    * an evaluator: |eval :: PC -> Sem| where |Sem = Tab -> Bool| and |Tab = String -> Bool|
* A bit of Haskell
    * recursive functions through "wishful thinking" (one helper function per case)
    * higher order functions
    * type driven development (TDD)
* First order logic (FOL) (as a new DSL, extending PC)
    * adding a term language (for rationals, or sets, or ...)
    * extend names to n-ary predicates over terms
    * quantifiers: Forall and Exists
* Pure set theory (as a domain for FOL)
    * introducing Empty, Singleton, Union
    * cardinality and some examples

** Lecture 2.2 (today) we continue working on logic and proofs:

* Case study: sqrt 2 is not rational [proof by contradiction]
* Case study: p^q can be rational even when neither p nor q is
* Case study: the limit of a function [from maths text to types and logic]
* Proofs (towards a DSL for proof terms)
    * building proof terms (AbstractFOL.lhs)
    * the meaning of Forall (as a gen. of And)
* More Curry–Howard: |Either a b| is |Or a b|, |(a,b)| is |And a b|
    * the tupling transform as example
* Perhaps: typed quantification
* Perhaps: pushing negation through other constructors

----------------

-- Modelling logic with types
\begin{code}
{-# LANGUAGE GADTs #-}

data Fals  -- No constructors - an empty set of proof terms
data Tru      = Obvious   -- Obvious :: Tru
hej :: And Tru Tru
hej = AndIntro Obvious Obvious

haj :: p -> And p p
haj b = AndIntro b b

qq1 :: Impl (And p q) (Or p q)
qq1 (AndIntro pb qb) = OrIntroL pb

qq2 :: Impl (And p q) (Or p q)
qq2 (AndIntro pb qb) = OrIntroR qb

data And p q where
  AndIntro :: p -> q -> And p q

data Or p q where
  OrIntroL :: p -> Or p q
  OrIntroR :: q -> Or p q

type Impl p q = p -> q     -- Implication as "proof term transformer"
type Not a    = Impl a Fals  -- logisk negation

andElimR   ::  And p q -> q
implIntro  ::  (p -> q) -> Impl p q
implElim   ::  Impl p q -> (p -> q)
notIntro   ::  (p -> Fals) -> Not p
notElim    ::  Not (Not p) -> p
implIntro = id
implElim  = id
notIntro  = id
notElim   = error "Can only be postulated"

andElimL   ::  And p q -> p
andElimL (AndIntro pb qb) = pb   -- första (fst)
andElimR (AndIntro pb qb) = qb   -- andra  (snd)

orElim :: Or p q -> (p -> r) -> (q -> r) -> r
  -- f  :: p -> r
  -- pb :: p
orElim (OrIntroL pb) f g = f pb
orElim (OrIntroR qb) f g = g qb
  -- g  :: q -> r
  -- qb :: q

hoj :: Or p q -> Or q p
hoj b = orElim b f g   -- med r = Or q p
  where   f = OrIntroR
          g = OrIntroL
  -- f :: p -> Or q p
  -- g :: q -> Or q p

f :: Impl a (And a a) --   a -> And a a
f b = AndIntro b b

g :: Impl a (Or a a)
g = OrIntroR

h :: Impl a (Or a (Not a))
h = OrIntroL

z :: Impl Fals Fals
z = error "TODO"

w :: Impl Fals Tru
w = error "TODO"

q :: a -> Not (Not a)
q b f = f b

-- test1 : Fals
-- test1 = q b id
--   where b =   -- b :: Fals  -- impossible
\end{code}
  a -> Not (Not a)
= a -> Not (Impl a Fals)
= a -> Not (a -> Fals)
= a -> Impl (a -> Fals) Fals
= a -> (a -> Fals) -> Fals




\begin{code}
th0 :: Impl (And a b)       (And b a) -- And a b -> And b a
th0    (AndIntro x y) = AndIntro y x

-- th1 :: Impl (Impl a (And b c))
--             (And (Impl a b) (Impl a c))
th1 :: (a -> And b c) -> And (a -> b) (a -> c)
th1 a2bc = AndIntro något någotannat
  where  något      a = andElimL (a2bc a) -- b-bevis sökes
         någotannat a = andElimR (a2bc a) -- c-bevis sökes

th1' :: (a -> And b c) -> And (a -> b) (a -> c)
th1' a2bc = AndIntro något någotannat
  where  något      = andElimL . a2bc
         någotannat = andElimR . a2bc

th1'' :: (a -> And b c) -> And (a -> b) (a -> c)
th1'' a2bc = AndIntro (andElimL . a2bc) (andElimR . a2bc)
  -- andElimL :: And p q -> p   -- fst
  -- andElimR :: And p q -> q   -- snd

type And' p q = (p,q)

-- th1'' :: (a -> And' b c) -> And' (a -> b) (a -> c)
th1''' :: (a -> (b,c)) -> (a -> b,  a -> c)
th1''' a2bc = (fst . a2bc , snd . a2bc)
-- tupling transform

\end{code}

Slut på föreläsningen.
Live-kodningen finns tillgänglig på YouTube:

  https://www.youtube.com/watch?v=uDkIrTn3CAA&list=PLf5C73P7ab-42GhewEmbQW2LtUV8MdBah&index=9&t=716s


\begin{code}
th2 :: Impl (Or  (Not a) (Not b)) (Not (And a b))
th3 :: Impl (And (Not a) (Not b)) (Not (Or  a b))

th2 = error "TODO"
th3 = error "TODO"
\end{code}
