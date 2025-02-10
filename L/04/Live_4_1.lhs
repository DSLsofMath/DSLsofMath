\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Live_4_1 where
\end{code}
DSLsofMath week 4: Compositionality and Algebras (part 1)

+ (blackboard): definition of H2 and friends

  H2(f,(+),(*)) = ∀ x. ∀ y. f(x+y)  ==  (f x) * (f y)
  – make sure all types are clarified
  f :   A     ->       B
  x   : A;       f x : B
  y   : A;       f y : B
  x+y : A;    (f x) * (f y) : B

+ (blackboard): examples H2(odd,(+),xor), not H2(isPrime,(+),_)

+ Haskell approximation |h2| of the predicate |H2|
\begin{code}
h2 :: Eq b =>  (a -> b)           -- f
           ->  (a -> a -> a)      -- (+)
           ->  (b -> b -> b)      -- (*)
           ->  (a -> a -> Bool)
h2 f (+) (*) = \x y ->  f(x+y)  ==  (f x) * (f y)
\end{code}

The Haskell approximation |h2| of the predicate |H2| takes
  the three usual parameters (f, (+), (*))
  and then two more parameters (concrete values for x and y)
  and finally checks if the
  left-hand side equals the right-hand side (for this pair of x and y).
The equality check has type |(==) :: b -> b -> Bool|.

Note that the real predicate H2 would be

  H2(f,(+),(*)) = ∀ x. ∀ y. h2 f (+) (*) x y

but we cannot in general implement ∀ in Haskell.

----
Examples: testing h2

H2(odd,(+),xor)
From blackboard:
  xor    F T
       -----
     F | F T
     T | T F
\begin{code}
xor :: Bool -> Bool -> Bool
xor = error "TODO"

type B = Bool
type Z = Integer
mytest :: Z -> Z -> Bool
mytest = h2 odd (+) xor   -- should ideally equal \_ _ -> True

isPrime 2 = True
isPrime 3 = True
isPrime 4 = False
isPrime 5 = True
isPrime _ = error "TODO check for bigger primes"
mytest2 = h2 isPrime (+) xor
\end{code}
Exercise: enumerate all possible op :: B -> B -> B

----

If we instantiate it to some usual suspects (from A1) we get

  prop_h2_U x y = h2 eval Union unionSem x y

-- (for simplicity I ignore env here)
which expands into  x, y :: TERM v
 eval (Union x y) == unionSem (eval x) (eval y)
  -- this holds "by definition"
  -- but for commutativity, etc.
 eval (Union x y) == eval (Union y x)
  -- you need to provide a proof.

and similarly
  prop_h2_I x y = h2 eval Intersect interSem x y
which expands into
  eval (Intersect x y) == interSem (eval x) (eval y)

As you may note, this property is fulfilled "by definition"
when we define a recursive evaluator using our usual
recursion pattern.

In general, we have seen many examples of H2

  H2(f,(+),(*)) = forall x, y. f(x+y) == f x * f y

with
  f = eval :: a -> b
  a = Syntax type     and (+) = a syntactic constructor
  b = Semantic domain and (*) = semantic operator (function)

----
DSLsofMath week 4: Compositionality and Algebras (part 1)
See separate file
