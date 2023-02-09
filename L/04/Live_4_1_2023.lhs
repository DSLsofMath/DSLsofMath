\begin{code}
{-# LANGUAGE GADTs #-}
--  {-# TypeSynonymInstances, FlexibleInstances #-}
module Live_4_1 where
\end{code}
DSLsofMath week 4: Compositionality and Algebras
+ L4.1.3: Week 4, Lecture 1, Part 3

+ L4.1.1 (Jamboard): definition of H2 and friends

  H2(f,(+),(*)) = Forall x. Forall y. f(x+y)  ==  (f x) * (f y)

+ L4.1.2 (Jamboard): examples H2(odd,(+),xor), not H2(isPrime,(+),_)

-- almost, but not quite, H2
\begin{code}
h2 :: Eq b =>  (a -> b)           -- f
           ->  (a -> a -> a)      -- (+)
           ->  (b -> b -> b)      -- (*)
           ->  (a -> a -> Bool)
h2 f (+) (*) = \x y ->  f(x+y)  ==  (f x) * (f y)

type REAL = Double
test1 :: REAL -> REAL -> Bool
test1 = h2 (\x -> x*x) (\x y -> x-y*2) (\x y -> y/x)
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
From Jamboard:
  xor    F T
       -----
     F | F T
     T | T F
\begin{code}
xor :: Bool -> Bool -> Bool
xor False False = False
xor True  False = True
xor False True  = True
xor True  True  = False

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
-- ? Exists op : B -> B -> B. H2(isPrime,(+),op)

Exercise: enumerate all possible op :: B -> B -> B

----

If we instantiate it to some usual suspects (from A1) we get

  prop_h2_U x y = h2 eval Union unionSem x y

  eval (Union x y) == unionSem (eval x) (eval y)

            A  -> B
 eval :     T -> Set
 Union :    T -> T -> T        -- i "A-världen"
 unionSem : Set -> Set -> Set  -- i "B-världen"

-- (for simplicity I ignore env here)
which expands into  x, y :: TERM v

 eval (Union x y) = unionSem (eval x) (eval y)

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

H2(eval,Union,unionSem) && H2(eval,Intersect,interSetSem) && ...

---------------
L4.1.4: Week 4, Lecture 1, Part 4
  Book §4.3 Compositional semantics
   and §4.4 Folds
\begin{code}
data IE where  -- "generalised algebraic data declaration" syntax
  Add :: IE -> IE -> IE   -- byt ut mot add :: b->b->b
  Mul :: IE -> IE -> IE   --            mul :: b->b->b
  Con :: Integer -> IE    --            con :: Integer->b
 deriving Show
\end{code}
Spec.:  H2(eva,Add,(+)) = -- TODO
Spec.:  H2(eva,Mul,(*))

  H2(eva,Add,(+))
= -- enl. def. av H2
  -- H2(f,(+),(*)) = Forall x. Forall y. f   (    x+y) ==  (f x)  * (f y)
                     Forall x. Forall y. eva (Add x y) == (eva x) + (eva y)

\begin{code}
type I = Integer
eva :: IE->I
eva (Add x y) = (eva x) + (eva y) -- då gäller H2(eva,Add,(+)) per definition
eva (Mul x y) = (eva x) * (eva y) -- då gäller H2(eva,Mul,(*)) per definition
eva (Con c)   = c

e1, e2, e3, e4 :: IE
e1 = Con 1
e2 = Con 2
e3 = Add e1 e2  -- 1+2
e4 = Mul e3 e3  -- (1+2)*(1+2)

ebig 0 = Con 2
ebig n = let big = ebig (n-1) in Mul big big
\end{code}

----------------
Step 2: refactor to eva2 with three parameters (+), (*), id
H2(eva,              Add, (+))
H2(eva2 add mul con, Add, add)

Pattern: use a local helper for the recursion
"generic fold"
\begin{code}
--       add         mul           con              e
eva2 :: (b->b->b) -> (b->b->b) -> (Integer -> b) -> (IE -> b)
eva2 add mul con = ev
  where  ev (Add x y) = add (ev x) (ev y) -- H2(ev,Add,add)
         ev (Mul x y) = mul (ev x) (ev y) -- H2(ev,Mul,mul)
         ev (Con c)   = con c             -- Forall c. H0(ev,Con c, con c)
{-
eva2 add mul con (Add x y) = add (eva2 add mul con x) (eva2 add mul con y)
eva2 add mul con (Mul x y) = mul (eva2 add mul con x) (eva2 add mul con y)
eva2 add mul con (Con c)   = con c
-}

eva' :: IE -> I  -- ska göra samma som eva
eva' = eva2 (+) (*) id

evaDeepCopy :: IE -> IE
evaDeepCopy = eva2 Add Mul Con
-- samma som id men mindre effektiv!
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
evenAdd ex ey = not (xor ex ey)
evenMul = (||)
evenCon :: Z -> Bool
evenCon = even
\end{code}

----------------
+ L4.1.5: Make your own type class
  to keep the type and the parameters together

Implement the example folds as instances.

Reminder:
  eva :: IE->I
  --       add         mul           con              e
  eva2 :: (b->b->b) -> (b->b->b) -> (Integer -> b) -> (IE -> b)

Three examples uses of eva2 (generic fold):

  eva' :: EI->I
  eva' = eva2 (+) (*) id           -- same as eva

  evaDeepCopy :: IE -> IE
  evaDeepCopy = eva2 Add Mul Con   -- special identity function

  evenIE :: IE -> Bool
  evenIE = eva2 evenAdd evenMul evenCon

\begin{code}
\end{code}
s1, s2, s3, s4 :: IntExp a => a
s1 = con 1
s2 = con 2
s3 = add s1 s2  -- 1+2
s4 = mul s3 s3  -- (1+2)*(1+2)
