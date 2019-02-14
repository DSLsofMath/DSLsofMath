\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Live_4_1 where
import DSLsofMath.FunNumInst

h2 :: Eq b =>  (a -> b)           -- f
           ->  (a -> a -> a)      -- (+)
           ->  (b -> b -> b)      -- (*)
           ->  (a -> a -> Bool)
h2 f (+) (*) = \x y -> f(x+y) == f x * f y
\end{code}

The Haskell approximation |h2| of the predicate |H2| takes the three
usual parameters (f, (+), (*)) and then two more parameters (concrete
values for x and y) and finally checks if the left-hand side equals
the right-hand side (for this pair of x and y).

If we instantiate it to some usual suspects (from A1) we get

  prop_h2_U x y = h2 eval Union unionSem x y

which expands into

 eval (Union x y) == unionSem (eval x) (eval y)

and similarly
  prop_h2_I = h2 eval Intersection interSem
which expands into
  \x y -> eval (Inter x y) == interSem (eval x) (eval y)

As you may note, this property is fulfilled "by definition" when we
define a recursive evaluator using our usual recursion pattern.

In general, we have seen many examples of H2

  H2(f,(+),(*)) = forall x, y. f(x+y) == f x * f y

with
  f = eval
  a = Syntax type     and (+) = a syntactic constructor
  b = Semantic domain and (*) = semantic operator (function)


DONE (blackboard): Homomorphisms (exp, log, f:a->{0}, even)
DONE (blackboard): non-homomorphism (isPrime, countWords)

DONE: |data IE| for integer expressions
DONE: eva :: ... -> IE->I
DONE: eva :: (I->I->I)->(I->I->I)->(I->I)->IE->I
DONE: eva :: (a->a->a)->(a->a->a)->(I->a)->IE->a
DONE: Num instance version
DONE: |class IntExp|
DONE: instance S
TODO: instance I
TODO: instance IntExt
TODO: from |eval| to |foldE|

\begin{code}
data IE where
  Add :: IE -> IE -> IE
  Mul :: IE -> IE -> IE
  Con :: Integer -> IE

type I = Integer
eva :: IE->I
eva (Add x y) = eva x   +    eva y  -- H2(eva,Add,(+))
eva (Mul x y) = eva x   *    eva y  -- H2(eva,Mul,(*))
eva (Con c)   = c

eva2 :: (a->a->a) -> (a->a->a) -> (Integer -> a) -> IE->a
eva2 add mul con = e
  where
    e (Add x y) = add (e x) (e y)  -- H2(e,Add,add)
    e (Mul x y) = mul (e x) (e y)  -- H2(e,Mul,mul)
    e (Con c)   = con c

eva' :: IE -> Integer
eva' = eva2 (+) (*) id

evaNum :: Num a => IE -> a
evaNum = eva2 (+) (*) fromInteger

class IntExp a where
  add :: a -> a -> a
  mul :: a -> a -> a
  con :: Integer -> a

instance IntExp String where
  add = addS
  mul = mulS
  con = conS
type S = String -- (String,String)

addS :: S -> S -> S
addS xs ys = xs ++ "+" ++ ys
mulS :: S -> S -> S
mulS xs ys = "(" ++ xs ++ ")*(" ++ ys ++ ")"
conS :: Integer -> S
conS = show

evalIE :: IntExp a => IE -> a
evalIE = eva2 add mul con

e1 = Con 1
e2 = Con 2
e3 = Add e1 e2
e4 = Mul e3 e3
\end{code}

End of live coding.



You can think of the type of |testIE| as |(a->a->a)->(a->a->a)->(I->a)-> a|.
%
\begin{code}
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
\end{code}
