\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Live_4_2 where
type REAL = Double
\end{code}

Domain-Specific Languages of Mathematics
Course Week 4, Lecture 2, Live-coding.


---------------
L4.2.1: Week 4, Lecture 2, Part 1
  Book §4.3 Compositional semantics
   and §4.4 Folds

Without the LANGUAGE GADTs extenstion:
  data IE = Add IE IE | Mul IE IE | Con Integer
\begin{code}
data IE where  -- "IE" stands for "Integer Expression"
  Add :: IE -> IE -> IE
  Mul :: IE -> IE -> IE
  Con :: Integer -> IE
 deriving (Show, Eq)
\end{code}
Spec.:  H2(eva,Add,(+)) = Forall x, y. eva (Add x y) = (eva x) + (eva y)
Spec.:  H2(eva,Mul,(*)) = Forall x, y. eva (Mul x y) = (eva x) * (eva y)
\begin{code}
type I = Integer
eva :: IE->I
eva (Add x y) = (eva x) + (eva y)
eva (Mul x y) = (eva x) * (eva y)
eva (Con c)   = c

e1, e2, e3, e4 :: IE
e1 = Con 1
e2 = Con 2
e3 = Add e1 e2  -- 1+2 = 3
e4 = Mul e3 e3  -- (1+2)*(1+2) = 9

ebig 0 = Con 2
ebig n = let big = ebig (n-1) in Mul big big
\end{code}

----------------
Step 2: refactor to eva2 with three parameters (+), (*), id
H2(eva,              Add, (+))


H2(eva2 add mul con, Add, add)
H2(eva2 add mul con, Mul, mul)
H0(eva2 add mul con, Con c, con c)

Pattern: use a local helper for the recursion

\begin{code}
--       add         mul           con              e
{-
eva2 add mul con (Add x y) = add (eva2 add mul con x) (eva2 add mul con y)
eva2 add mul con (Mul x y) = mul (eva2 add mul con x) (eva2 add mul con y)
eva2 add mul con (Con c)   = con c
-}

-- fold for the IE datatype
eva2 :: (b->b->b) -> (b->b->b) -> (Integer -> b) -> IE->b
eva2 add mul con = eva --                        :: IE->b
  where
    eva (Add x y) = add (eva x) (eva y)
    eva (Mul x y) = mul (eva x) (eva y)
    eva (Con c)   = con c

eva' :: IE -> I
eva' = eva2 (+) (*) id

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

evenAdd :: Bool -> Bool -> Bool
evenAdd = (==)

evenMul :: Bool -> Bool -> Bool
evenMul = (||)

evenCon :: Integer -> Bool
evenCon = (0==) . (`mod` 2)
\end{code}

----------------
+ L4.1.5: Make your own type class
  to keep the type and the parameters together

Implement the example folds as instances.
\begin{code}
s1, s2, s3, s4 :: IntExp a => a
s1 = con 1
s2 = con 2
s3 = add s1 s2  -- 1+2
s4 = mul s3 s3  -- (1+2)*(1+2)

class IntExp a where
  add :: a -> a -> a
  mul :: a -> a -> a
  con :: Integer -> a

instance IntExp Integer where add = (+); mul = (*); con = id
instance IntExp Bool    where add = (==); mul = (||); con = evenCon
instance IntExp IE      where add = Add; mul = Mul; con = Con

evaIE :: IntExp b => IE->b
evaIE = eva
  where
    eva (Add x y) = add (eva x) (eva y)
    eva (Mul x y) = mul (eva x) (eva y)
    eva (Con c)   = con c
\end{code}
