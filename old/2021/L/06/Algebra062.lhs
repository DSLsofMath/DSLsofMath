* Implement some of the numeric type classes "from scratch"

* Implement instances for functions

* Implement instances for power series

* Implement instances for derivative streams

\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
module DSLsofMath.Algebra where
import qualified Prelude -- hide everything by default
import Prelude (Bool(..), Eq(..), Show(..))

infixl 6 -
infixl 6 +

infixl 7 *
infixl 7 /
\end{code}

\begin{code}
class Additive a where
  (+)   :: a -> a -> a
  zero  :: a

addZeroLeft :: (Eq a, Additive a) => a -> Bool
addZeroLeft x = zero + x == x

addAssoc :: (Eq a, Additive a) => a -> a -> a -> Bool
addAssoc x y z = (x+y)+z == x+(y+z)

type REAL = Prelude.Double
instance Additive REAL where
  (+)   = addR
  zero  = zeroR

addR :: REAL -> REAL -> REAL
addR = (Prelude.+)
zeroR :: REAL
zeroR = Prelude.fromInteger 0
\end{code}

Multiplicative
\begin{code}
class Multiplicative a where
  (*)   :: a -> a -> a
  one   :: a

instance Multiplicative REAL where
  (*) = (Prelude.*); one = Prelude.fromInteger 1

e1, two :: (Additive a, Multiplicative a) => a
e1 = zero+one
two = one+one
\end{code}

AddGroup
\begin{code}
class Additive a => AddGroup a where
  negate :: a -> a

(-) :: AddGroup a => a -> a -> a
x - y = x + negate y

instance AddGroup REAL where negate = Prelude.negate
\end{code}

\begin{code}
class (AddGroup a, Multiplicative a) => Ring a
instance Ring REAL

r1 :: Ring a => a -> a
r1 x = (x-one)*(x-one)
\end{code}

MulGroup
\begin{code}
class Multiplicative a => MulGroup a where
  recip :: a -> a
(/) :: MulGroup a => a -> a -> a
x / y = x * recip y

instance MulGroup REAL where recip = Prelude.recip

class (Ring a, MulGroup a) => Field a
instance Field REAL

mg1 :: Field a => a
mg1 = one/two
\end{code}

Summing up so far:
\begin{spec}
class Additive a       where   (+)  :: a -> a -> a;   zero  :: a
class Multiplicative a where   (*)  :: a -> a -> a;   one   :: a
class Additive a       => AddGroup a where  negate  :: a -> a
class Multiplicative a => MulGroup a where  recip   :: a -> a
\end{spec}
and just one type as instance: REAL.

Now we want more type instances.
Step 1: functions.
Step 2: power series
Step 3: derivative streams
\begin{code}
instance Additive a => Additive (t->a) where
  (+) = addFun
  zero = zeroFun

addFun :: Additive a => (t->a) -> (t->a) -> (t->a)
addFun f g = a
  where a x = f x + g x

zeroFun :: Additive a => t->a
zeroFun = \_ -> zero

f1 :: Bool -> REAL
f1 b = if b then 16 else 37
f2 :: t -> REAL
f2 = Prelude.const 1

f3 :: Bool -> REAL
f3 = f1 + f2
testf3 = f3 False + 100*f3 True
\end{code}

\begin{code}
instance Multiplicative a => Multiplicative (t->a) where
  (*) = mulFun
  one = oneFun

oneFun :: Multiplicative a => (t->a)
oneFun _ = one
mulFun :: Multiplicative a => (t->a)->(t->a)->(t->a)
mulFun f g x = f x * g x

f4 :: Multiplicative a => a -> a
f4 = Prelude.id * Prelude.id
\end{code}

Power Series
\begin{code}
newtype P a = P [a] deriving (Show, Eq)

constP :: a -> P a
constP c = P [c]

zeroP :: Additive a => P a
zeroP = constP zero

addP :: Additive a => P a -> P a -> P a
addP (P xs) (P ys) = P (addL xs ys)

type L = []
addL :: Additive a => L a -> L a -> L a
addL = zipWithLonger (+)

zipWithLonger :: (a->a->a) -> L a -> L a -> L a
zipWithLonger _  [] ys = ys
zipWithLonger _  xs [] = xs
zipWithLonger op (x:xs) (y:ys) = (op x y) : zipWithLonger op xs ys

instance Additive a => Additive (P a) where (+) = addP; zero = zeroP

xP, p1 :: P REAL
xP = P [0,1]
p1 = P [1,1,3]
testP = xP+p1
\end{code}

\begin{code}
instance Ring a => Multiplicative (P a) where
  (*) = mulP; one = oneP

oneP :: Multiplicative a => P a
oneP = constP one

mulP :: Ring a => P a -> P a -> P a
mulP (P xs) (P ys) = P (mulL xs ys)

mulL :: Ring a => L a -> L a -> L a
mulL [] _ = []
mulL _ [] = []
mulL (x:xs) ys = addL (scaleL x ys) (zero : mulL xs ys)

scaleL :: Multiplicative a => a -> L a -> L a
scaleL c = Prelude.map (c*)

sq :: Multiplicative a => a -> a
sq x = x*x

p2 = sq (xP+one)
p3 = sq xP + two*xP + one
\end{code}

\begin{code}
instance AddGroup a => AddGroup (P a) where
  negate = negateP
negateP :: AddGroup a => P a -> P a
negateP (P xs) = P (negateL xs)

negateL :: AddGroup a => L a -> L a
negateL = Prelude.map negate
\end{code}

Derivative streams
\begin{code}
newtype DS a = DS [a] deriving (Show, Eq)
constDS :: a -> DS a
constDS c = DS [c]

zeroDS :: Additive a => DS a
zeroDS = constDS zero

addDS :: Additive a => DS a -> DS a -> DS a
addDS (DS xs) (DS ys) = DS (addL xs ys)

instance Additive a => Additive (DS a) where
  (+) = addDS; zero = zeroDS

oneDS :: Multiplicative a => DS a
oneDS = constDS one

mulDS :: Multiplicative a => DS a -> DS a -> DS a
mulDS (DS xs) (DS ys) = DS (mulD xs ys)

mulD :: Multiplicative a => L a -> L a -> L a
mulD = Prelude.error "TODO"







\end{code}
\begin{code}
\end{code}
