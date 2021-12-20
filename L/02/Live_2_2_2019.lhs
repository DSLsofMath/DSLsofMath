* Live coding part of week 2, lecture 2.

Reminder of learning outcomes

* organize areas of mathematics in DSL terms
    * "develop adequate notation for mathematical concepts"
    * "perform calculational proofs"

** Lecture 2.1 covered

* Propositional calculus (as a DSL)
    * Boolean logic without quantifiers (And, Or, Implies, Not, False, True) but with named atoms
    * a syntax tree datatype |Prop|
    * an evaluator: |eval :: Env String Bool -> Prop -> Bool|
    * truth table, tautology
* A bit of Haskell
    * |Maybe|, partial functions, |Env k v|, |evalEnv|
    * recursive functions through "wishful thinking" (one helper function per case)
* First order logic (FOL) (as a new DSL, extending Prop)
    * adding a term language (for rationals, or sets, or ...)
    * extend names to n-ary predicates over terms
    * quantifiers: Forall and Exists
* Proofs (towards a DSL for proofs)
    * building proof terms (AbstractFOL.lhs)
    * the meaning of Forall (as a gen. of And)

** Exercise 2.1:

* Presented: More on AbstractFOL.lhs
* Frequently asked questions
    * Haskell in general (found typos in A1, E1.1 and E1.2, added some solutions)
    * Generic evaluators (explanation of exam question solutions)

----------------------------------------------------------------

Today we continue working on logic and proofs:

* Case study: sqrt 2 is not rational [proof by contradiction]
* Case study: p^q can be rational even when neither p nor q is
* Case study: the limit of a function [from maths text to types and logic]
* More Curry–Howard: |Either a b| is |Or a b|, |(a,b)| is |And a b|
    * the tupling transform as example
* typed quantification
* pushing negation through other constructors

----------------

\begin{code}
data And  a b = A a b     -- A :: a -> b -> And a b; A : P => Q => P&Q
data Or   a b = L a | R b -- L :: a -> Or a b; R :: b -> Or a b
type Impl a b = a -> b
data Fals
data Tru      = T         -- alt. constructor name: Obvious
type Not a    = Impl a Fals

f :: Impl a (And a a)
f p = A p p

g :: Impl a (Or a a)
g  p = L p
g' p = R p

h :: Impl a (Or a (Not a))
h x = L x

z :: Impl Fals Fals
z p = p    -- p :: Fals

w :: Impl Fals Tru
w _ = T

q :: a -> Not (Not a)
q p = nna
  where nna na = na p   -- na :: a -> Fals
\end{code}

The type of q p is
    Not (Not a)
  = Impl (Not a) Fals
  = Impl (Impl a Fals) Fals
  = Impl (a -> Fals) Fals
  = (a -> Fals) -> Fals

\begin{code}
th0 :: Impl (And a b) (And b a)
th0 (A p q) = A q p

th1, th1' :: Impl (Impl a (And b c))
                  (And (Impl a b) (Impl a c))
th1 f = A g1 g2
  where g1 a = z1 -- g1 :: a->b = (Impl a b)
           where A z1 z2 = f a -- :: And b c

        g2 a = z2 -- g2 :: a->c = (Impl a c)
           where A z1 z2 = f a -- :: And b c

first :: And a b -> a
first (A p q) = p

second :: And a b -> b
second (A p q) = q

th1' f = A g1 g2
  where g1 a = first (f a)
        g2 a = second (f a)

th1'' :: (a -> And b c) -> And (a->b) (a->c)
th1'' f = A (first . f) (second . f)
\end{code}

The lecture ended here.

\begin{code}
th2 :: Impl (Or  (Not a) (Not b)) (Not (And a b))
th3 :: Impl (And (Not a) (Not b)) (Not (Or  a b))

th2 (L na) = \(A p q) -> na p
th2 (R nb) = \(A p q) -> nb q
th3 (A na nb) = f
  where  f (L p) = na p
         f (R q) = nb q
\end{code}
