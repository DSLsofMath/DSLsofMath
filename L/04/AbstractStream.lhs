%if False
\begin{code}
{-# LANGUAGE FlexibleContexts #-}
module AbstractStream where
import Prelude hiding (head, tail)
\end{code}
%endif

\paragraph{Streams as an abstract datatype.}

Consider the API for streams of values of type |A| represented by some
abstract type |X|:
%
\begin{code}
data X
data A
head  ::  X  ->  A
tail  ::  X  ->  X
cons  ::  A  ->  X  ->  X

law1     s  = {-"\qquad"-}  s  == cons  (head s) (tail s)
law2  a  s  =               s  == tail  (cons a s)
law3  a  s  =               a  == head  (cons a s)
\end{code}
%if False
\begin{code}
(head, tail, cons) = undefined
law1 :: Eq X => X -> Bool
law2 :: Eq X => A -> X -> Bool
law3 :: Eq A => A -> X -> Bool
\end{code}
%endif

%
With this API we can use |head| to extract the first element of the
stream, and |tail| to extract the rest as a new stream of type |X|.
%
Using |head| and |tail| recursively we can extract an infinite list of
values of type |A|:
%
\begin{code}
toList :: X -> [A]
toList x = head x : toList (tail x)
\end{code}
%
In the other direction, if we want to build a stream we only have one
constructor: |cons| but no ``base case''.
%
In Haskell, thanks to laziness, we can still define streams directly using
|cons| and recursion
%
As an example, we can construct a constant stream as follows:
\begin{code}
constS :: A -> X
constS a = ca
  where ca = cons a ca
\end{code}
%
Instead of specifying a stream in terms of how to contruct it, we
could describe it in terms of how to take it apart; by specifying its
|head| and |tail|.
%
In the constant stream example we would get something like:
\begin{spec}
head  (constS a)  = a
tail  (constS a)  = constS a
\end{spec}
but this syntax is not supported in Haskell.


The last part of the API are a few laws we expect to hold.
%
The first law simply states that if we first take a stream |s| apart
into its head and its tail, we can get back to the original stream by
|cons|ing them back together.
%
The second and third are variant on this theme, and together the three
laws specify how the three operations interact.
