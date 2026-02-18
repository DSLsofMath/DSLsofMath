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
\begin{code}
data IE where  -- "IE" stands for "Integer Expression"
  Add :: IE -> IE -> IE
  Mul :: IE -> IE -> IE
  Con :: Integer -> IE
 deriving Show
\end{code}
Spec.:  H2(eva,Add,(+)) =def=   (Forall x, y : IE. eva (Add x y) = eva x  +  eva y)
Spec.:  H2(eva,Mul,(*)) =def=   (Forall x, y : IE. eva (Mul x y) = eva x  *  eva y)
\begin{code}
type I = Integer
eva :: IE->I
-- eva = error "TODO"
eva (Add x y) = eva x  +  eva y
eva (Mul x y) = eva x  *  eva y
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
H2(eva2 add mul con, Mul, mul)
H0(eva2 add mul con, Con c, con c)

Pattern: use a local helper for the recursion

\begin{code}
--       add         mul           con              e
eva2 :: (b->b->b) -> (b->b->b) -> (Integer -> b) -> (IE->b)
eva2 add mul con = e
  where e (Add x y) = add (e x) (e y)
        e (Mul x y) = mul (e x) (e y)
        e (Con c)   = con c

-- eva2 add mul con (Con c)   = con c
-- eva2 add mul con (Add x y) = add (eva2 add mul con x) (eva2 add mul con y)
-- eva2 add mul con (Mul x y) = mul (eva2 add mul con x) (eva2 add mul con y)

eva' :: IE -> I
eva'  = eva2 (+) (*) id
kalle = eva2 (*) (+) (2*)  -- not so useful
odd' :: IE -> Bool
odd' = eva2 addOdd mulOdd conOdd
  where  addOdd b1 b2 = not (b1==b2)
         mulOdd b1 b2 = b1 && b2
         conOdd c = mod c 2 == 1

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

evenCon = (0==) . (`mod` 2)




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

instance IntExp IE where add = Add; mul = Mul; con = Con
instance IntExp I  where add = (+); mul = (*); con = id
instance IntExp Bool where add = evenAdd; mul = evenMul; con = evenCon

s1, s2, s3, s4 :: IntExp a => a
s1 = con 1
s2 = con 2
s3 = add s1 s2  -- 1+2
s4 = mul s3 s3  -- (1+2)*(1+2)

evalIntExp :: IntExp a => IE -> a
evalIntExp = eva2 add mul con
\end{code}
