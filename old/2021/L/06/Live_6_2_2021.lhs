\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module DSLsofMath.Algebra where
import qualified Prelude -- hide everything by default
import Prelude (Bool(..), Eq(..), Show(..),
                fromInteger, error, const, id, map)
type REAL = Prelude.Double
\end{code}

Plan:
+ Implement some of the numeric type classes "from scratch"
+ Implement some instances for functions
+ Implement some instances for power series   (TODO deriv, integ, (/))
+ Implement some instances for derivative streams

\begin{code}
infixl 6 +
class Additive a        where  zero :: a;   (+) :: a -> a -> a   -- Monoid
class Additive a       => AddGroup a where  negate :: a -> a  -- och (+) och zero

class Multiplicative a  where  one  :: a;   (*) :: a -> a -> a   -- Monoid
class Multiplicative a => MulGroup a where  recip  :: a -> a

type Ring  a = (AddGroup a, Multiplicative a)  -- sv: ring
type Field a = (Ring a, MulGroup a)            -- sv: kropp

-- class (AddGroup a, Multiplicative a) => Ring a ger i princip samma effekt

instance Additive REAL where zero = zeroR; (+) = addR
zeroR :: REAL
zeroR = fromInteger 0
addR :: REAL -> REAL -> REAL
addR = (Prelude.+)
\end{code}

infixl 6 -
\begin{code}
(-) :: AddGroup a =>  a -> a -> a
x - y = x + negate y

instance AddGroup REAL where negate = negateR
negateR :: REAL -> REAL
negateR = Prelude.negate
\end{code}

\begin{code}

infixl 7 *
r1 :: Ring a => a -> a
r1 x = (x-one)*(x-one)

instance Multiplicative REAL where one = oneR; (*) = mulR

oneR :: REAL
oneR = fromInteger 1
mulR :: REAL -> REAL -> REAL
mulR = (Prelude.*)
\end{code}


\begin{code}
infixl 7 /
(/) :: MulGroup a => a -> a -> a
x / y = x * recip y
instance MulGroup REAL where recip = Prelude.recip

two :: (Additive a, Multiplicative a) => a
two = one+one

half :: Field a => a
half = one/two
\end{code}

Instans för funktioner
f+g  :: t -> a
f, g :: t -> a


types in Additive =
  {REAL,
   String->REAL, Bool->REAL, t->REAL,
   String -> (String->REAL), Bool -> String -> REAL,
  }
\begin{code}
instance Additive a => Additive (t->a) where zero = zeroF; (+) = addF
instance AddGroup a => AddGroup (t->a) where negate = negateF
instance Multiplicative a => Multiplicative (t->a) where one = oneF; (*) = mulF

negateF :: AddGroup a => (t->a) -> (t->a)
negateF f = \x -> negate (f x)

zeroF :: Additive a => t->a
zeroF = \t -> zero
addF :: Additive a => (t->a) -> (t->a) -> (t->a)
addF f g = \x -> f x + g x

oneF :: Multiplicative a => t->a
oneF = \t -> one
mulF :: Multiplicative a => (t->a) -> (t->a) -> (t->a)
mulF f g = \x -> f x * g x

f1 :: Additive a => a -> a
f1 = id + id
\end{code}

Power Series
\begin{code}
newtype P a = P [a] deriving (Show, Eq)
-- semantiken är funktioner f :: a->a

instance Additive a => Additive (P a) where
  zero = zeroP
  (+) = addP

zeroP :: P a
zeroP = P zeroL
zeroL :: L a
zeroL = []
addP :: Additive a => P a -> P a -> P a
addP (P xs) (P ys) = P (addL xs ys)

instance AddGroup a => AddGroup (P a) where
  negate = negateP

negateP :: AddGroup a => P a -> P a
negateP (P as) = P (negateL as)
negateL :: AddGroup a => L a -> L a
negateL = map negate

type L = [] -- list type constructor
addL :: Additive a => L a -> L a -> L a
addL [] ys = ys
addL xs [] = xs
addL (x:xs) (y:ys) = (x+y) : addL xs ys
-- zipWithLonger (+)

instance Ring a => Multiplicative (P a) where
  one = oneP
  (*) = mulP

oneP :: Multiplicative a => P a
oneP = P [one]
mulP :: Ring a => P a -> P a -> P a
mulP (P xs) (P ys) = P (mulL xs ys)

mulL :: Ring a => L a -> L a -> L a
mulL []     ys = []
mulL (a:as) ys = addL  (scaleL a ys)
                       (zero : mulL as ys)

scaleL :: Multiplicative a => a -> L a -> L a
scaleL c as = map (c*) as

xP :: Ring a => P a
xP = P [zero,one]
\end{code}


Derivate streams
-- alla derivator i punkten 0
\begin{code}
newtype DS a = DS [a] deriving (Show, Eq)

zeroDS :: Additive a => DS a
zeroDS = DS zeroL

addDS :: Additive a => DS a -> DS a -> DS a
addDS (DS xs) (DS ys) = DS (addL xs ys)

instance Additive a => Additive (DS a) where zero = zeroDS; (+)=addDS

oneDS :: Multiplicative a => DS a
oneDS = DS oneL

oneL :: Multiplicative a => L a
oneL = one : zeroL -- constant function returning 1 + all derivatives

mulDS :: Ring a => DS a -> DS a -> DS a
mulDS (DS as) (DS bs) = DS (mulD as bs)

--
mulD :: Ring a => L a -> L a -> L a
mulD [] bs = []
mulD as [] = []
mulD as@(a:as') bs@(b:bs') = m:ms'
  where  m   = a*b
         ms' =            addL  (mulD as' bs)  (mulD as bs')
               --(f*g)' = add         (f'*g)        (f * g')

  -- head (mulD as bs) = (head as) * (head bs)
  -- tail (mulD as bs) = deriv (mulD as bs) =
  -- a = head as; as' = tail as = deriv as

instance Ring a => Multiplicative (DS a) where
  one = oneDS; (*)=mulDS

xD :: Ring a => DS a   -- [id 0, id' 0, id'' 0, ...]
xD = DS (zero:oneL)
\end{code}

\begin{code}
\end{code}
\begin{code}
\end{code}
\begin{code}
\end{code}




----------------
Summing up so far: [after ~30min]
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
\end{code}

f1 :: Bool -> REAL
f1 b = if b then 16 else 37
f2 :: t -> REAL
f2 = Prelude.const 1

f3 :: Bool -> REAL
f3 = f1 + f2
testf3 = f3 False + 100*f3 True

\begin{code}
\end{code}

----------------------------------------------------------------
Power Series
\begin{code}
\end{code}
\begin{code}
\end{code}

----------------------------------------------------------------
Derivative streams
\begin{code}
\end{code}
\begin{code}
\end{code}
