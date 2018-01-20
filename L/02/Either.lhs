%if False
\begin{code}
module Either where
import Prelude hiding (Either(Left,Right), either)
\end{code}
%endif

\begin{code}
data Either p q = Left p | Right q
-- |Left| is |orIntroL|, |Right| is |orIntroR|
either :: (p->r) -> (q->r) -> Either p q -> r
either l r (Left x)   =  l x
either l r (Right y)  =  r y
\end{code}
