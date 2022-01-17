* Implement some of the numeric type classes "from scratch"

* Implement instances for functions

* Implement instances for power series

* Implement instances for derivative streams

\begin{code}
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
