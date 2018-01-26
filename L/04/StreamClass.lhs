%if False
\begin{code}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
module StreamClass where
import Prelude hiding (head, tail)
\end{code}
%endif
\begin{code}
class Stream x where
  type Elem x
  head  ::  x  ->  Elem x
  tail  ::  x  ->  x
  cons  ::  Elem x  ->  x  ->  x

law1     s  = {-"\qquad"-}  s == cons (head s) (tail s)
law2  a  s  =               s == tail (cons a s)
law3  a  s  =               a == head (cons a s)
\end{code}
%if False
\begin{code}
law1 :: (Stream x, Eq x) => x -> Bool
law2 :: (Stream x, Eq x) => Elem x -> x -> Bool
law3 :: (Stream x, Eq (Elem x)) => Elem x -> x -> Bool
\end{code}
%endif

\begin{code}
type REAL = Double
newtype Fun = Fun {apply :: REAL -> REAL}
deriv :: Fun -> Fun
integ :: Fun -> Fun
(deriv, integ) = undefined
instance Stream Fun where
  type Elem Fun = REAL
  head (Fun f) = f 0
  tail = deriv
  cons a f = Fun $ \x -> a + apply (integ f) x
\end{code}


\begin{code}
instance Stream [a] where
  type Elem [a] = a
  head (x:_)   = x
  tail (_:xs)  = xs
  cons = (:)
\end{code}

\begin{code}
data CoAlg a = CoA {hd :: a, tl :: CoAlg a}
instance Stream (CoAlg a) where
  type Elem (CoAlg a) = a
  head = hd
  tail = tl
  cons = CoA
\end{code}

\begin{code}
constS :: Stream s => Elem s -> s
constS a = x
  where  x = cons a x
\end{code}
