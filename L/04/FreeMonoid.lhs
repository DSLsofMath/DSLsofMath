\section{\extraMaterial A generic Free construction}
\jp{Integrate this with the rest more seamlessly.}

%if False
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module FreeMonoid where
\end{code}
%endif


There is a generic way to define a free algebra for a given class. For monoid it is as follows:
|forall a. Monoid a => (x -> a) -> a|.

One way to understand this type is as follows. If we had the type
|forall a. Monoid a => a| then we could construct the result (|a|)
from all the methods of the monoid class, in the same way that |IntExp
a => a| is any integer. However here we have additionally a way to
\emph{embed} an |x| into the monoid |a|, represented by the additional |x->a| argument.
Such embedded values are called generators.

One can even parameterise the |Free| generator over the class |c|:
\begin{code}
newtype Free c a = Free (forall m. c m => (a -> m) -> m)

embed :: m -> Free c m
embed generator = Free (\k -> k generator)
\end{code}

One can easily show that a |Free c| is an instance of the class |c|. For example, |Free Monoid| is an instance of |Monoid|:
\begin{code}
instance Semigroup (Free Monoid m)  where
  Free f <> Free g = Free (\x -> f x <> g x)

instance Monoid (Free Monoid m)  where
  mempty = Free (\_ -> mempty)
\end{code}

We can also check the monoid laws for the free monoid. For
example, here is the proof that the right identity law holds:
\begin{spec}
    a <> mempty
==  {- def. -}
    Free f <> Free (\_ -> mempty)
==  {- def. -}
    Free (\x -> f x <> mempty)
==  {- law of the underlying monoid (|m|) -}
    Free (\x -> f x)
==  {- law of the underlying monoid (|m|) -}
    Free f
==  {- def -}
    a
  \end{spec}

\begin{exercise}
Prove group laws for |Free AdditiveGroup|.
\end{exercise}


But we can also recover the whole structure which was used to build an
element of this type. For example (pulling the fact that that lists are free monoids from our hat),
we can define:
\begin{code}
toList :: Free Monoid a -> [a]
toList (Free f) = f (\x -> [x])
\end{code}

As an example, we can test that |(embed 1 <> embed 10 <> mempty <> embed 11) == [1,10,11]|.
\begin{code}
example :: Free Monoid Int
example = embed 1 <> embed 10 <> mempty <> embed 11
\end{code}

\begin{exercise}
Prove group laws for |Free AdditiveGroup|.
\end{exercise}

