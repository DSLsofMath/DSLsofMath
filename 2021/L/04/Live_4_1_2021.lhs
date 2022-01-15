\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}


module Live_4_1 where

h2 :: Eq b =>  (a -> b)           -- f
           ->  (a -> a -> a)      -- (+)
           ->  (b -> b -> b)      -- (*)
           ->  (a -> a -> Bool)
h2 f (+) (*) = \x y ->  f(x+y)  ==  (f x) * (f y)


xor :: Bool -> Bool -> Bool
-- xor x y = not (x == y)
xor = (/=)

type B = Bool
type Z = Integer
mytest :: Z -> Z -> Bool
mytest = h2 odd (+) xor   -- förväntas vara \x y -> True

isPrime 2 = True
isPrime 3 = True
isPrime 4 = False
isPrime 5 = True
isPrime _ = error "Ja ids int"
mytest2 = h2 isPrime (+) xor
-- övning - räkna upp alla op :: B -> B -> B
\end{code}

The Haskell approximation |h2| of the predicate |H2| takes
the three usual parameters (f, (+), (*)) and then two more
parameters (concrete values for x and y) and finally checks
if the left-hand side equals the right-hand side (for this
pair of x and y). The equality check has type |(==) :: b ->
b -> Bool|. Note that the real predicate H2 would be

  H2(f,(+),(*)) = Forall x. Forall y. h2 f (+) (*) x y

but we cannot implement Forall in Haskell.

If we instantiate it to some usual suspects (from A1) we get

  prop_h2_U x y = h2 eval Union unionSem x y

which expands into

 eval (Union x y) == unionSem (eval x) (eval y)
  -- gäller "automatiskt"
  -- men kommutativitet, etc.
 eval (Union x y) == eval (Union y x)
  -- måsta man "jobba" för

and similarly
  prop_h2_I x y = h2 eval Intersection interSem x y
which expands into
  eval (Inter x y) == interSem (eval x) (eval y)

As you may note, this property is fulfilled "by definition"
when we define a recursive evaluator using our usual
recursion pattern.

In general, we have seen many examples of H2

  H2(f,(+),(*)) = forall x, y. f(x+y) == f x * f y

with
  f = eval :: a -> b
  a = Syntax type     and (+) = a syntactic constructor
  b = Semantic domain and (*) = semantic operator (function)

\begin{code}
data IE where
  Add :: IE -> IE -> IE
  Mul :: IE -> IE -> IE
  Con :: Integer -> IE
 deriving Show
-- Spec.:  H2(eva,Add,(+)) =
--           forall x, y.   eva (Add x y) == eva x + eva y
-- Spec.   H2(eva,Mul,(*))
type I = Integer
eva :: IE->I
eva (Add x y) = (eva x) + (eva y)
eva (Mul x y) = (eva x) * (eva y)
eva (Con c)   = id c

e1, e2, e3, e4 :: IE
e1 = Con 1
e2 = Con 2
e3 = Add e1 e2  -- 1+2
e4 = Mul e3 e3  -- (1+2)*(1+2)


-- Step 2: refactor to eva2 with three parameters (+), (*), id
-- H2(eva,              Add, (+))
-- H2(eva2 add mul con, Add, add)
-- eva :: IE -> b
eva2 :: (b->b->b) -> (b->b->b) -> (Integer -> b) -> IE->b
eva2 add mul con = eva
  where  eva (Add x y) = add (eva x) (eva y)
         eva (Mul x y) = mul (eva x) (eva y)
         eva (Con c  ) = con c  -- c :: I -- inte IE

eva' :: IE -> I
eva' = eva2 (+) (*) id
evaDeepCopy  = eva2 Add Mul Con

-- en:fold
  -- pattern: use a local helper for the recursion

evaHej = eva2 (*) (+) (1+)

evenIE :: IE -> Bool
evenIE = eva2 evenAdd evenMul evenCon

-- H2(evenIE,Add,evenAdd)
-- H2(evenIE,Mul,evenMul)
-- H0(evenIE,Con c,evenCon c)

-- foo ie = even (eva2 (+) (*) id ie)

evenAdd :: Bool -> Bool -> Bool
evenAdd = (==)
  where  bar False False = True   -- udda + udda = jämn
         bar False True  = False  -- udda + jämn = udda
         bar True  False = False
         bar True  True  = True

evenMul :: Bool -> Bool -> Bool
evenMul = (||)
-- evenMul False False = False  -- udda*udda = udda
-- evenMul _     _     = True   -- annars    = jämn

evenCon :: Integer -> Bool
evenCon c = 0 == mod c 2

\end{code}




You can think of the type of |testIE| as |(a->a->a)->(a->a->a)->(I->a)-> a|.
%
\begin{spec}
instance IntExp Integer where
  mul = (*)
  add = (+)
  con = id

instance IntExp IE where
  mul = Mul
  add = Add
  con = Con

testIE :: IntExp a => a
testIE = evalIE hej

testIE1 :: Integer
testIE1 = testIE

testIE2 :: String
testIE2 = testIE

testIE3 :: IE
testIE3 = testIE

hej = Mul (Add (Con 3) (Con 4))
          (Con 5)
\end{spec}
