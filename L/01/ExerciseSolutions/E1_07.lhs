\begin{code}
{-# LANGUAGE GADTs #-}
module DSLsofMath.E1_DualTupling where
import Prelude hiding (Either(..), either)
\end{code}

Exercise 1_DualTupling  There is also a ``dual'' to the tupling transform: to show this,
implement these functions:

\begin{code}
s2p :: (Either b c -> a) -> (b->a, c->a)
s2p  =  \fg     ->  (fg . Left, fg . Right)

p2s :: (b->a, c->a) -> (Either b c -> a)
p2s  =  \(f,g)  ->  either f g
\end{code}

Note that |Either|, |Left|, |Right|, and |either| are from Haskell's
Prelude and are repeated here just for easy reference.

\begin{code}
data Either a b where
  Left   :: a -> Either a b
  Right  :: b -> Either a b

either :: (b->a) -> (c->a) -> (Either b c -> a)
either f g (Left x)   = f x
either f g (Right y)  = g y
\end{code}
