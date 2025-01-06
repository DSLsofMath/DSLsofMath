* Implement some of the numeric type classes "from scratch"

* Implement instances for functions

* Implement instances for power series

* Implement instances for derivative streams

\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
module DSLsofMath.Algebra where
import qualified Prelude -- hide everything by default
import Prelude (Bool, Eq((==)))
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
f1 b = if b then 17 else 38
f2 :: t -> REAL
f2 = Prelude.const 1
\end{code}

\begin{code}
\end{code}
