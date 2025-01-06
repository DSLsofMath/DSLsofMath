* Live coding part of week 2, lecture 2.

Reminder of learning outcomes

* organize areas of mathematics in DSL terms
    * "develop adequate notation for mathematical concepts"
    * "perform calculational proofs"

Modelling logic with types
Using the type-checker as proof-checker
  ("poor mans proof checker")

b :: T  means b is a proof of the (logic) theorem T

\begin{code}
{-# LANGUAGE GADTs #-}

data Fals  -- No constructors - an empty set of proof terms
data Tru      = Obvious

test1 :: Tru
test1 = Obvious

type And p q = (p,q)

test2 :: And Tru Tru  -- (Tru, Tru)
test2 = (Obvious, Obvious)

-- this means   Implies (And p q) (And q p)
swap :: And p q -> And q p   -- And is commutative
-- swap :: (p, q) -> (q, p)
swap    (a, b) =  (b, a)

-- the only property we use about the "base types" is whether or not they are inhabited.
-- If there are zero values in the type is encodes falsity otherwise truth.

type Hi = And Integer Bool  -- basically same as And Tru Tru
-- Hi is a theorem saying "Integer is inahbited and Bool is inhabited"
--   not a very useful theorem, but an easy one to prove:  

hi :: Hi
hi = (1, True)

type Or p q = Either p q

swap2 :: Or p q -> Or q p   -- Or is commutative
-- swap2 :: Either p q    -> Either q p  
swap2 (Left        x   )  =  Right    x
swap2 (Right         y )  =  Left   y

type Not p = p -> Fals
exc :: Not (Not (Or p (Not p)))   -- law of excluded middle

-- exc :: Not (Or p (Not p))   -> Fals
-- exc :: (Or p (Not p)  -> Fals)   -> Fals
-- exc :: (Or p (p -> Fals)  -> Fals)   -> Fals
exc k  =  k (Right (\x -> k (Left x)))

testlist :: [Either Integer String]
testlist = [Left 1, Right "hej", Right "haj", Left 2]
\end{code}

data Either a b where  -- disjoint union
  Left  :: a -> Either a b  -- orIntroL
  Right :: b -> Either a b  -- orIntroR

----------------------------------------------------------------

Repetition/reminder: Lecture 2.1 covered

* Propositional calculus (as a DSL)
    * Boolean logic without quantifiers (And, Or, Implies, Not, False, True) but with named atoms
    * a syntax tree datatype |PC|
    * an evaluator: |eval :: PC -> Sem| where |Sem = Tab -> Bool| and |Tab = String -> Bool|
* Pure set theory (as a domain for FOL)
    * introducing Empty, Singleton, Union
    * cardinality and some examples
    * enumerating some example sets (m0, m1, m2, t01, ...)
* A bit of Haskell
    * recursive functions through "wishful thinking" (one helper function per case)
    * higher order functions
    * type driven development (TDD)

Lecture 2.2 (today) we continue working on logic and proofs:

* First order logic (FOL) (as a new DSL, extending PC)
    * adding a term language (for rationals, or sets, or ...)
    * extend names to n-ary predicates over terms
    * quantifiers: Forall and Exists
* Typed quantification
* Pushing negation through other constructors
* Case study: sqrt 2 is not rational [proof by contradiction]
* Case study: p^q can be rational even when neither p nor q is

* [Did not fit: Perhaps: Case study: the limit of a function [from maths text to types and logic]]
* Below: Proofs (towards a DSL for proof terms)
    * building proof terms (AbstractFOL.lhs)
    * the meaning of Forall (as a gen. of And)

----------------

