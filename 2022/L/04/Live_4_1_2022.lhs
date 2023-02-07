\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Live_4_1 where
\end{code}
DSLsofMath week 4: Compositionality and Algebras
+ L4.1.3: Week 4, Lecture 1, Part 3
  
+ L4.1.1 (Jamboard): definition of H2 and friends

  H2(f,(+),(*)) = Forall x. Forall y. f(x+y)  ==  (f x) * (f y)

+ L4.1.2 (Jamboard): examples H2(odd,(+),xor), not H2(isPrime,(+),_)

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

  H2(f,(+),(*)) = Forall x. Forall y. h2 f (+) (*) x y

but we cannot in general implement Forall in Haskell.

----
Examples: testing h2

H2(odd,(+),xor)
xor    F T
     -----
   F | F T
   T | T F
xor False False = False;  xor False True = True
xor True  False = False;  xor True  True = False
\begin{code}
xor :: Bool -> Bool -> Bool
xor = (/=)  -- for the type Bool
  
type B = Bool
type Z = Integer
mytest :: Z -> Z -> Bool
mytest = h2 odd (+) xor   -- should ideally equal \_ _ -> True

isPrime 2 = True
isPrime 3 = True
isPrime 4 = False
isPrime 5 = True
isPrime _ = error "TODO check for bigger primes"

mytest2 :: Z -> Z -> Bool
mytest2 = h2 isPrime (+) xor -- Note not the whole proof

mytest3 :: (B -> B -> B) -> Z -> Z -> Bool
mytest3 = h2 isPrime (+) 
\end{code}
The real H2 property has two foralls, thus can be disproven by just
any pair of counterexamples.

x = 2, y = 3 disproved (h2 isPrime (+) xor) which means xor is not the
target of isPrime as a (potential) homomorphism.

Exercise: enumerate all possible op :: B -> B -> B

And if we find a single counter-example for each candidate op, then we
have shown that no such op exists. (Even without a formal proof.)

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

  H2(eval,Intersect,interSem)

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

data IE where -- GADT form
  Add :: IE -> IE -> IE
  Mul :: IE -> IE -> IE
  Con :: Integer -> IE
 deriving (Eq, Show)  -- warning - be careful with deriving Eq

data Rat = Rat Integer Integer -- don't derive Eq here
r1 = Rat 1 2
r2 = Rat 2 4
test12 = r1 == r2

\begin{code}
data IE = Add IE IE | Mul IE IE | Con Integer
  deriving (Eq, Show)
\end{code}
Specification:  H2(eva,Add,(+)) = Forall x,y:IE. eva (Add x y) == eva x + eva y
Specification:  H2(eva,Mul,(*)) = Forall x,y:IE. eva (Mul x y) == eva x * eva y
Specification:  H0(eva,Con c, c)=                eva (Con c)   == c            
\begin{code}
type I = Integer
eva :: IE->I  -- eva is a "fold"
eva (Add x y) = eva x + eva y
eva (Mul x y) = eva x * eva y
eva (Con c)   = c            

e1, e2, e3, e4 :: IE
e1 = Con 1
e2 = Con 2
e3 = Add e1 e2  -- 1+2
e4 = Mul e3 e3  -- (1+2)*(1+2)
\end{code}

----
Step 2: refactor to eva2 with three parameters (+), (*), id
H2(eva,              Add, (+))
H2(eva2 add mul con, Add, add)

Pattern: use a local helper for the recursion

\begin{code}

--       add         mul           con              e
eva2 :: (b->b->b) -> (b->b->b) -> (Integer -> b) -> IE->b
eva2 add mul con = eva
  where  eva (Add x y) = add (eva x) (eva y)
         eva (Mul x y) = mul (eva x) (eva y)
         eva (Con c)   = con c            
  
{- 
eva2 add mul con (Add x y) = add (eva2 add mul con x) (eva2 add mul con y)
eva2 add mul con (Mul x y) = mul (eva2 add mul con x) (eva2 add mul con y)
eva2 add mul con (Con c)   = con c
-}

eva' :: IE -> I
eva' = eva2 (+) (*) id  -- should be the same as eva

evaDeepCopy :: IE -> IE
evaDeepCopy = eva2 Add Mul Con    -- == id, but more inefficient
\end{code}

Try to use eva2 for checking if an expression denotes an even number.

Specification: 
  evenIE == even . eva

  H2(evenIE,Add,evenAdd)
  H2(evenIE,Mul,evenMul)
  H0(evenIE,Con c,evenCon c)

\begin{code}
ebig 0 = Con 2
ebig n = Mul (ebig (n-1)) (ebig (n-1)) 

evenIE :: IE -> Bool
evenIE = eva2 evenAdd evenMul evenCon
evenIE' = even . eva
  -- fold in action (eva2 is often called fold)

evenAdd :: B -> B -> B
evenAdd = (==)  -- Boolean equality
{-
evenAdd False False = True    -- odd + odd = even
evenAdd True  False = False   -- even + odd = odd
evenAdd False True  = False   -- odd + even = odd
evenAdd True  True  = True    -- even + even = evena
-}  

evenMul :: B -> B -> B
evenMul = (||)    -- "if either of the inputs is even, the result is even"

evenCon :: I -> B
evenCon c = mod c 2 == 0 -- no remainder after division by 2
\end{code}

----------------
+ L4.1.5: Make your own type class
  to keep the type and the parameters together

\begin{code}
class IntExp a where
  add :: a -> a -> a
  mul :: a -> a -> a
  con :: I -> a

instance IntExp Integer where add = (+); mul = (*); con = id
-- now we have one type in the set of instances
instance IntExp IE      where add = Add; mul = Mul; con = Con
-- now we two types in the set of instances
instance IntExp Bool    where add = evenAdd; mul = evenMul; con = evenCon

-- s1, s2, s3, s4 :: (a->a->a) -> (a->a->a) -> (Integer -> a) -> a
s1, s2, s3, s4 :: IntExp a => a
s1 = con 1
s2 = con 2
s3 = add s1 s2  -- 1+2
s4 = mul s3 s3  -- (1+2)*(1+2)

evaIE :: IntExp a => IE -> a
evaIE = eva
  where  eva (Add x y) = add (eva x) (eva y)
         eva (Mul x y) = mul (eva x) (eva y)
         eva (Con c)   = con c            

testI :: IE -> I
testI = evaIE

testIE :: IE -> IE
testIE = evaIE

testB :: IE -> Bool
testB = evaIE

testB3 :: Bool
testB3 = testB s3 -- = testB (Add s1 s2) = testB (Add (Con 1) (Con 2))
\end{code}

data IE       -- syntax part of a DLS
class IntExp  -- corresponding type class collecting the fold parameters

eva   :: IE -> I
  -- not "generic" (fixed to the semantic type I = Integer)
eva2  :: (b->b->b) -> (b->b->b) -> (Integer -> b) -> IE->b
  -- fold (generic version one)
  -- explicitly supply the "semantic functions"
evaIE :: IntExp a => IE -> a
  -- fold (generic version two)
  -- implicitly supply the "semantic functions" though an instance declaration

Exercise: make an instance IntExp String which implements pretty-printing IE's












