%if False
\begin{code}
{-# LANGUAGE GADTs #-}
module Either where
import Prelude hiding (Either(Left,Right), either)
\end{code}
%endif

\begin{code}
data Either p q = Left p | Right q
\end{code}


%if False
\begin{spec}
data Either p q where
  Left   :: p  ->  Either p q
  Right  :: q  ->  Either p q
\end{spec}
\begin{code}
either :: (p->r) -> (q->r) -> (Either p q -> r)
either l r (Left x)   =  l x
either l r (Right y)  =  r y
\end{code}
%endif
