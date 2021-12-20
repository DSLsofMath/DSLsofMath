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
* Case study: sqrt 3 is not rational [proof by contradiction]
* Case study: p^q can be rational even when neither p nor q is

* [Did not fit: Perhaps: Case study: the limit of a function [from maths text to types and logic]]
* Below: Proofs (towards a DSL for proof terms)
    * building proof terms (AbstractFOL.lhs)
    * the meaning of Forall (as a gen. of And)

----------------

-- Modelling logic with types

logik som typer
typcheckaren som bevischeckare

\begin{code}
{-# LANGUAGE GADTs #-}

data Fals  -- No constructors - an empty set of proof terms
data Tru      = Obvious

test1 :: Tru
test1 = Obvious

type And p q = (p,q)

test2 :: And Tru Tru
test2 = (Obvious, Obvious)

swap :: And p q -> And q p   -- And är kommutativ
swap (bp, bq) = (bq, bp)

type Or p q = Either p q

swap2 :: Or p q -> Or q p   -- Or är kommutativ
swap2 (Left bp)  = Right bp
  -- foo :: Or q p = Either q p
  -- bp  :: p
swap2 (Right bq) = Left bq

testlist :: [Either Integer, String]
testlist = [Left 1, Right "hej", Right "haj", Left 2]
\end{code}

data Either a b where  -- disjoint union
  Left  :: a -> Either a b
  Right :: b -> Either a b
