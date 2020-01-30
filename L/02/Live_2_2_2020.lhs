* Live coding part of week 2, lecture 2.

Reminder of learning outcomes

* organize areas of mathematics in DSL terms
    * "develop adequate notation for mathematical concepts"
    * "perform calculational proofs"

* Lecture 2.1 covered

* Propositional calculus (as a DSL)
    * boolean logic without quantifiers (And, Or, Implies, Not, False, True) but with named atoms
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

* Proofs (towards a DSL for proof terms)
    * building proof terms (AbstractFOL.lhs)
    * the meaning of Forall (as a gen. of And)
* Case study: sqrt 2 is not rational [proof by contradiction]
* Case study: p^q can be rational even when neither p nor q is
* Case study: the limit of a function [from maths text to types and logic]
* More Curry–Howard: |Either a b| is |Or a b|, |(a,b)| is |And a b|
    * the tupling transform as example
* typed quantification
* pushing negation through other constructors

----------------

-- Modelling logic with types
\begin{code}
{-# LANGUAGE GADTs #-}

data Fals  -- No constructors - an empty set of proof terms
data Tru      = Obvious
data And p q where
  AndIntro :: p -> q -> And p q
data Or p q where
  OrIntroL :: p -> Or p q
  OrIntroR :: q -> Or p q

type Impl p q = p -> q     -- Implication as "proof term transformer"
type Not a    = Impl a Fals

andElimL   ::  And p q -> p
andElimR   ::  And p q -> q
orElim     ::  Or p q -> (p -> r) -> (q -> r) -> r
implIntro  ::  (p -> q) -> Impl p q
implElim   ::  Impl p q -> (p -> q)
notIntro   ::  (p -> Fals) -> Not p
notElim    ::  Not (Not p) -> p
implIntro = id
implElim  = id
notIntro  = id
notElim   = error "Can only be postulated"

orElim   = error "TODO"
andElimL = error "TODO"
andElimR = error "TODO"


f :: Impl a (And a a)
f = error "TODO"

g :: Impl a (Or a a)
g = error "TODO"

h :: Impl a (Or a (Not a))
h = error "TODO"

z :: Impl Fals Fals
z = error "TODO"

w :: Impl Fals Tru
w = error "TODO"

q :: a -> Not (Not a) -- TODO: expand type
q = error "TODO"
\end{code}


\begin{code}
th0 :: Impl (And a b) (And b a)
th0 = error "TODO"

th1 :: Impl (Impl a (And b c))
            (And (Impl a b) (Impl a c))
th1 = error "TODO"
\end{code}


\begin{code}
th2 :: Impl (Or  (Not a) (Not b)) (Not (And a b))
th3 :: Impl (And (Not a) (Not b)) (Not (Or  a b))

th2 = error "TODO"
th3 = error "TODO"
\end{code}
