DONE: Mention peer review (of exercises) using GitHub classroom
DONE: Mention Assignment 02 [by 2018-02-27]

DONE: |data IE| for integer expressions
DONE: from |eval| to |foldE|
DONE: |class IntExp|

\begin{code}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Live_4_1 where
import DSLsofMath.FunNumInst
data IE = Mul IE IE | Add IE IE | Con Integer
  deriving Show
-- Mul

-- normal |eval :: IE -> Integer|
-- step 1: |eval :: (I->I->I)->(I->I->I)->(I->I)->IE->I|
-- step 2:
eval :: (a->a->a)->(a->a->a)->(I->a)->IE->a
-- Usually called |fold|
eval mul add con = ev where
  ev (Mul a b) = mul (ev a) (ev b)
  ev (Add a b) = add (ev a) (ev b)
  ev (Con i)   = con i

-- eva = eval = fold
eva :: (a->a->a)->(a->a->a)->(I->a)->IE->a
eva mul add con (Mul a b) = mul (eva mul add con a) (eva mul add con b)
eva mul add con (Add a b) = add (eva mul add con a) (eva mul add con b)
eva mul add con (Con i)   = con i

f :: Num a =>   IE -> a
f = eval (*) (-) ((+1).fromInteger)

g :: Num a =>   IE -> a
g = eval (*) (+) fromInteger


-- con    :: I  -> I
class IntExp a where  -- Compare to the syntactic operators:
  mul :: a -> a -> a  --   |Mul :: IE  -> IE  -> IE|
  add :: a -> a -> a  --   |Add :: IE  -> IE  -> IE|
  con :: I -> a       --   |Con :: I -> IE|

evalIE :: IntExp a =>   IE -> a
evalIE = eval mul add con

instance IntExp Integer where
  mul = (*)
  add = (+)
  con = id

instance IntExp String where
  mul = mulS
  add = addS
  con = conS

instance IntExp IE where
  mul = Mul
  add = Add
  con = Con

type S = String
mulS :: S -> S -> S
mulS a b = "Mul " ++ a ++ " " ++ b
addS :: S -> S -> S
addS a b = "Add " ++ a ++ " " ++ b
conS :: I -> S
conS i = "Con " ++ show i
\end{code}

You can think of the type of |testIE| as |(a->a->a)->(a->a->a)->(I->a)-> a|.
%
\begin{code}
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
testI :: I
testI = f hej


type I = Integer
\end{code}

End of live coding.



































DONE: black-board: homomorphisms
DONE: black-board: eval'


Below is the code I prepared before the lecture.

\begin{spec}
data E = Add E E | Mul E E | Con Integer deriving Eq
e1, e2 :: E                             -- | 1 + 2 * 3 |
e1 = Add (Con 1) (Mul (Con 2) (Con 3))  -- | 1 +(2 * 3)|
e2 = Mul (Add (Con 1) (Con 2)) (Con 3)  -- |(1 + 2)* 3 |

foldE ::  (s -> s -> s) -> (s -> s -> s) -> (Integer -> s) -> (E -> s)
foldE add mul con = rec
  where  rec (Add x y)  = add (rec x) (rec y)
         rec (Mul x y)  = mul (rec x) (rec y)
         rec (Con i)    = con i

evalE1 :: E -> Integer
evalE1 = foldE (+) (*) id

evalE2 :: Num a => E -> a
evalE2 = foldE (+) (*) fromInteger

idE :: E -> E
idE = foldE Add Mul Con

class IntExp t where
  add  ::  t -> t -> t
  mul  ::  t -> t -> t
  con  ::  Integer -> t

foldIE :: IntExp t =>  E -> t
foldIE = foldE add mul con

instance IntExp E where
  add = Add
  mul = Mul
  con = Con

instance IntExp Integer where
  add = (+)
  mul = (*)
  con = id

idE' :: E -> E
idE' = foldIE

evalE' :: E -> Integer
evalE' = foldIE

seven :: IntExp a => a
seven = add (con 3) (con 4)

testI :: Integer
testI = seven

testE :: E
testE = seven

check :: Bool
check = and  [  testI  ==  7
             ,  testE  ==  Add (Con 3) (Con 4)
             ]
\end{spec}
