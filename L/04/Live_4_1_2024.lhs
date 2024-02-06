\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Live_4_1 where
\end{code}
DSLsofMath week 4: Compositionality and Algebras

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
h2 h (+) (*) = \x y ->  h(x+y)  ==  (h x) * (h y)
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

---------------
L4.1.4: Week 4, Lecture 1, Part 4
  Book §4.3 Compositional semantics
   and §4.4 Folds
\begin{code}
data IE where
  Add :: IE -> IE -> IE   -- H2(eva,Add,(+)) = ∀a,b.       eva (Add a b) = eva a + eva b
  Mul :: IE -> IE -> IE   -- H2(eva,Mul,(*)) = ∀a,b.       eva (Mul a b) = eva a * eva b
  Con :: Integer -> IE    -- ∀ c. H0(eval, Con c, c) = ∀c. eva (Con c)   = c            
 deriving Show
\end{code}
Spec.:  H2(eva,Add,(+)) = -- TODO
Spec.:  H2(eva,Mul,(*))
\begin{code}
type I = Integer
eva :: IE->I
eva (Add a b) = eva a + eva b
eva (Mul a b) = eva a * eva b
eva (Con c)   = c            

e1, e2, e3, e4 :: IE
e1 = Con 1
e2 = Con 2
e3 = Add e1 e2  -- 1+2
e4 = Mul e3 e3  -- (1+2)*(1+2)

ebig :: Int -> IE
ebig 0 = Con 2
ebig n = let big = ebig (n-1) in Mul big big
\end{code}

----------------
Step 2: refactor to eva2 with three parameters (+), (*), id
H2(eva,              Add, (+))
H2(eva2 add mul con, Add, add)
H2(eva2 add mul con, Mul, mul)

Pattern: use a local helper for the recursion

\begin{code}
--       add         mul           con              e
eva2 :: (b->b->b) -> (b->b->b) -> (Integer -> b) -> IE->b
eva2 add mul con = eva
  where  eva (Add a b) = eva a `add` eva b
         eva (Mul a b) = eva a `mul` eva b
         eva (Con c)   = con c            

-- eva2 add mul con (Add a b) = eva2 add mul con a `add` eva2 add mul con b
-- eva2 add mul con (Mul a b) = eva2 add mul con a `mul` eva2 add mul con b
-- eva2 add mul con (Con c)   = con c            


eva' :: IE -> I
eva' = eva2 (+) (*) id   -- eva == eva'

evaDeepCopy :: IE -> IE
evaDeepCopy = eva2 Add Mul Con

\end{code}

Try to use eva2 for checking if an expression denotes an even number.

Specification:
  evenIE == even . eva
  H2(evenIE,Add,evenAdd)  
  H2(evenIE,Mul,evenMul)
  H0(evenIE,Con c,evenCon c)

\begin{code}
evenIE :: IE -> Bool
evenIE = eva2 evenAdd evenMul evenCon

evenAdd, evenMul :: Bool -> Bool -> Bool
evenAdd = (==)
-- evenAdd False False = True
-- evenAdd True  False = False
-- evenAdd False  True = False
-- evenAdd True   True = True
evenMul = (||)
evenCon :: Integer -> Bool
evenCon c = mod c 2 == 0
\end{code}

----------------
+ L4.1.5: Make your own type class
  to keep the type and the parameters together

Implement the example folds as instances.
\begin{code}
class IntExp b where
  add :: b -> b -> b
  mul :: b -> b -> b
  con :: Integer -> b
-- eval-instance
instance IntExp Integer where add = (+); mul = (*); con = id
-- deep-compy-instance?
instance IntExp IE where add = Add; mul = Mul; con = Con
instance IntExp Bool where add = evenAdd; mul = evenMul; con = evenCon

eval :: IntExp b => IE->b
eval = eva
  where  eva (Add a b) = eva a `add` eva b
         eva (Mul a b) = eva a `mul` eva b
         eva (Con c)   = con c            

-- fold
\end{code}

-- We did not get to the below example 2024-02-06.

s1, s2, s3, s4 :: IntExp a => a
s1 = con 1
s2 = con 2
s3 = add s1 s2  -- 1+2
s4 = mul s3 s3  -- (1+2)*(1+2)
