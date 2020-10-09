
\subsection{\extraMaterial A generic Free construction}
\jp{Integrate this with the rest more seamlessly.}


\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
\end{code}


There is a generic way to define a free algebra for a given class. For monoid it is as follows:
|forall a. Monoid a => (x -> a) -> a|.

One way to understand this type is as follows. If we had the type
|forall a. Monoid a => a| then we could construct the result (|a|)
from all the methods of the monoid class, in the same way that |IntExp
a => a| is any integer. However here we have additionally a way to
embed an |x| into the monoid |a|.

\begin{code}
newtype Free c a = Free (forall m. c m => (a -> m) -> m)

embed :: m -> Free c m
embed x = Free (\k -> k x)
\end{code}

This type is a monoid:
\begin{code}
instance Semigroup (Free Monoid m)  where
  Free f <> Free g = Free (\x -> f x <> g x)

instance Monoid (Free Monoid m)  where
  mempty = Free (\_ -> mempty)
\end{code}


But we can also recover the whole structure which was used to build an
element of this type. For example:
\begin{code}
toList :: Free Monoid a -> [a]
toList (Free f) = f (\x -> [x])
\end{code}

(So it should indeed be an initial monoid --- contradicting the table below BTW.)

\footnote{To add to the injury, this construction is sometimes called ``final encoding'' in Haskell jargon.}
