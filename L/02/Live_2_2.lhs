* Live coding part of week 2, lecture 2.

Reminder of learning outcomes

* organize areas of mathematics in DSL terms
    * "develop adequate notation for mathematical concepts"
    * "perform calculational proofs"

Modelling logic with types
Using the type-checker as proof-checker
  ("poor mans proof checker")

\begin{code}
{-# LANGUAGE GADTs #-}

data Fals  -- No constructors - an empty set of proof terms
data Tru      = Obvious

test1 :: Tru
test1 = Obvious

type And p q = (p,q)

test2 :: And Tru Tru  -- (Tru, Tru)
test2 = error "TODO"

swap :: And p q -> And q p   -- And is commutative
swap = error "TODO"

type Or p q = Either p q

swap2 :: Or p q -> Or q p   -- Or is commutative
swap2 = error "TODO"


type Not p = p -> Fals
exc :: Not (Not (Or p (Not p)))   -- law of excluded middle
exc = error "TODO"
\end{code}

data Either a b where  -- disjoint union
  Left  :: a -> Either a b  -- orIntroL
  Right :: b -> Either a b  -- orIntroR


