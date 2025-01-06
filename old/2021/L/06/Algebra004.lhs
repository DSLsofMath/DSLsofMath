* Implement some of the numeric type classes "from scratch"

* Implement instances for functions

* Implement instances for power series

* Implement instances for derivative streams

\begin{code}
module DSLsofMath.Algebra where
import qualified Prelude -- hide everything by default
import Prelude (Eq((==)))
\end{code}

\begin{code}
class Additive a where
  (+)   :: a -> a -> a
  zero  :: a

-- law1 :: Additive a => a -
addLaw1 x = zero + x == x

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
