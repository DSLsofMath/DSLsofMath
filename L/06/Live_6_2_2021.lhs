* Implement some of the numeric type classes "from scratch"
* Implement some instances for functions
* Implement some instances for power series
* Implement some instances for derivative streams

\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
module DSLsofMath.Algebra where
import qualified Prelude -- hide everything by default
import Prelude (Bool(..), Eq(..), Show(..), fromInteger, error, const, id, map)
type REAL = Prelude.Double
\end{code}
infixl 6 +


\begin{code}
\end{code}
infixl 6 -

\begin{code}
\end{code}
infixl 7 *

r1 :: Ring a => a -> a
r1 x = (x-one)*(x-one)

\begin{code}
\end{code}
infixl 7 /
mg1 :: Field a => a
mg1 = one/two

\begin{code}
\end{code}
\begin{code}
\end{code}
\begin{code}
\end{code}
\begin{code}
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
