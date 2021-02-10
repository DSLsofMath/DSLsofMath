\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
import DSLsofMath.FunNumInst
data IE where
  Add :: IE -> IE -> IE
  Mul :: IE -> IE -> IE
  Con :: Integer -> IE
 deriving Show

eva2 :: (b->b->b) -> (b->b->b) -> (Integer -> b) -> IE->b
eva2 add mul con = eva
  where  eva (Add x y) = add (eva x) (eva y)
         eva (Mul x y) = mul (eva x) (eva y)
         eva (Con c  ) = con c  -- c :: I -- inte IE

evenAdd, evenMul :: Bool -> Bool -> Bool
evenCon :: Integer -> Bool
evenAdd = (==)
evenMul = (||)
evenCon c = 0 == mod c 2
\end{code}

Summary of L4.1

* black-board: homomorphisms
  exp,
  H2, H1, H0,
  H2(even,(+),xor)
  not Exists op. H2(isPrime,(+),op)
* Live coding:
  H2 "in Haskell" -> h2
  examples: H2(eval, Union, unionSem), etc.
* |data IE| for integer expressions
* from |eva| to |foldE| (|eva2|)

Summary of L4.2:

* From |eva2| to class-based fold
  class IntExp

* black-board: def. of eval' = eval . der
** show that der is not a homomorphism
    * not (Exists muld. H2(der,Mul,muld) & H1(eval,der,D))
** (tupling transform)
** show that der2 *is* a homomorphism
* compositional semantics means we can work without syntax trees
* Perhaps: |apply c| is a |Num|-homomorphism
* Perhaps: |applyFD c| as well


-- TODO Step 2b: Example of generic instance

\begin{code}
evaNum :: Num a => IE -> a
evaNum = eva2 (+) (*) fromInteger

e1, e2, e3, e4 :: IE
e1 = Con 1
e2 = Con 2
e3 = Add e1 e2
e4 = Mul e3 e3

-- TODO Step 3: Make your own class
--   to keep the type and the parameters together
class IntExp b where
  add :: b -> b -> b
  mul :: b -> b -> b
  con :: Integer -> b

instance IntExp IE where
  add = Add
  mul = Mul
  con = Con

instance IntExp Integer where
  add = (+)
  mul = (*)
  con = id

b3, b4 :: IntExp b =>    b
b3 = add (con 1) (con 2)
b4 = mul b3 b3

instance IntExp Bool where
  add = evenAdd
  mul = evenMul
  con = evenCon

instance IntExp String where
  add = addS
  mul = mulS
  con = conS
type Str = String

addS :: Str -> Str -> Str
mulS :: Str -> Str -> Str
conS :: Integer -> Str
addS x y = x++"+"++y
mulS x y = "("++x++")*("++y++")"
conS = show
-- H2(evalIE,Add,addS)
-- H2(evalIE,Mul,mulS)

evalIE :: IntExp b => IE -> b
evalIE = eva2 add mul con

s1, s2, s3, s4 :: IntExp a => a
s1 = con 1
s2 = con 2
s3 = add s1 s2  -- 1+2
s4 = mul s3 s3  -- (1+2)*(1+2)
\end{code}


----------------


\begin{code}
type REAL = Double
data F where
  A :: F -> F -> F
  M :: F -> F -> F
  X :: F
  C :: REAL -> F
 deriving Show
type S = REAL -> REAL

eval :: F -> S
eval (A f g) = oadd (eval f) (eval g)
  -- f :: F, eval f :: S
eval (M f g) = omul (eval f) (eval g)
eval X       = id
eval (C c)   = const c

oadd, omul :: S -> S -> S
oadd f g = \x -> f x + g x
omul f g = \x -> f x * g x

der2 :: F -> Bi F
der2 (A f g)   = addder2 (der2 f) (der2 g)
der2 (M f g)   = mulder2 (der2 f) (der2 g)
der2 X         = xder2
der2 (C c)     = cder2 c

xder2 :: Bi F
xder2 = Bi (X,   C 1)

cder2 :: REAL -> Bi F
cder2 c = Bi (C c, C 0)

addder2, mulder2 :: Bi F -> Bi F -> Bi F
addder2 (Bi (f, f')) (Bi (g, g')) =
  Bi (A f g, A f' g')
mulder2 (Bi (f, f')) (Bi (g, g')) =
  Bi (M f g, A (M f' g) (M f g'))

test1 = Bi (M X X ,
            A (M (C 1.0) X)
              (M X (C 1.0)))
test1' = Bi (M X X , A X X)
test1'' = Bi (M X X , M (C 2) X)

class Fun f where
  m :: f -> f -> f
  a :: f -> f -> f
  x :: f
  c :: REAL -> f

instance Fun F where m = M; a = A; x = X; c = C
instance Fun (Bi F) where
  m = mulder2
  a = addder2
  x = xder2
  c = cder2

foldF ::  (s->s->s) -> (s->s->s) -> s -> (REAL -> s) ->
          (F -> s)
foldF a m x c = fold
  where   fold (A f g) = a (fold f) (fold g)
          fold (M f g) = m (fold f) (fold g)
          fold X       = x
          fold (C r)   = c r

deep_copy    = foldF A M X C
eval_på_nytt = foldF oadd omul id const
der2_på_nytt = foldF addder2 mulder2 xder2 cder2

newtype Bi a = Bi (a, a) deriving Show
\end{code}
