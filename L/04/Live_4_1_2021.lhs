\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Live_4_1 where
-- import DSLsofMath.FunNumInst

h2 :: Eq b =>  (a -> b)           -- f
           ->  (a -> a -> a)      -- (+)
           ->  (b -> b -> b)      -- (*)
           ->  (a -> a -> Bool)
h2 f (+) (*) = \x y -> f(x+y) == f x * f y
\end{code}

The Haskell approximation |h2| of the predicate |H2| takes the three
usual parameters (f, (+), (*)) and then two more parameters (concrete
values for x and y) and finally checks if the left-hand side equals
the right-hand side (for this pair of x and y). The equality check has
type |(==) :: b -> b -> Bool|. Note that the real predicate H2 would be

  H2(f,(+),(*)) = Forall x. Forall y. h2 f (+) (*) x y

but we cannot implement Forall in Haskell.

If we instantiate it to some usual suspects (from Assignment 1) we get

  prop_h2_U x y = h2 eval Union unionSem x y

which expands into

 eval (Union x y) == unionSem (eval x) (eval y)

and similarly
  prop_h2_I x y = h2 eval Intersection interSem x y
which expands into
  eval (Inter x y) == interSem (eval x) (eval y)

As you may note, this property is fulfilled "by definition" when we
define a recursive evaluator using our usual recursion pattern.

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
-- Spec.:  H2(eva,Add,(+)) = forall x, y.   eva (Add x y) == eva x + eva y
-- Spec.   H2(eva,Mul,(*))
type I = Integer
eva :: IE->I
eva = error "TODO"

-- Step 2: refactor to eva2 with three parameters (+), (*), id
-- H2(eva,              Add, (+))
-- H2(eva2 add mul con, Add, add)
eva2 :: (a->a->a) -> (a->a->a) -> (Integer -> a) -> IE->a
eva2 = error "TODO"
  -- pattern: use a local helper for the recursion

evenIE :: IE -> Bool
evenIE = eva2 evenAdd evenMul evenCon
evenAdd :: Bool -> Bool -> Bool
evenMul :: Bool -> Bool -> Bool
evenCon :: Integer -> Bool
(evenAdd, evenMul, evenCon) = error "TODO"

eva' :: IE -> Integer
eva' = eva2 (+) (*) id

e1 = Con 1
e2 = Con 2
e3 = Add e1 e2  -- 1+2
e4 = Mul e3 e3  -- (1+2)*(1+2)

-- Step 2b: Example of generic instance
evaNum :: Num a => IE -> a
evaNum = eva2 (+) (*) fromInteger

-- Step 3: Make your own class to keep the type and the parameters together
class IntExp a where
  add :: a -> a -> a
  mul :: a -> a -> a
  con :: Integer -> a

instance IntExp IE where
  add = Add
  mul = Mul
  con = Con

instance IntExp Integer where
  add = (+)
  mul = (*)
  con = id

instance IntExp Bool where
  add = evenAdd
  mul = evenMul
  con = evenCon

instance IntExp String where
  add = addS
  mul = mulS
  con = conS
type S = String

addS :: S -> S -> S
mulS :: S -> S -> S
conS :: Integer -> S
addS x y = x++"+"++y
mulS x y = "("++x++")*("++y++")"
conS = show
-- H2(evalIE,Add,addS)
-- H2(evalIE,Mul,mulS)

evalIE :: IntExp a => IE -> a
evalIE = eva2 add mul con

s1, s2, s3, s4 :: IntExp a => a
s1 = con 1
s2 = con 2
s3 = add s1 s2  -- 1+2
s4 = mul s3 s3  -- (1+2)*(1+2)


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
