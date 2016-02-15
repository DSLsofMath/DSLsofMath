> {-# LANGUAGE TypeSynonymInstances #-}

Lecture 9: Algebraic Structures and DSLs
========================================

In this lecture, we continue exploring the relationship between type
classes, mathematical structures, and DSLs.

1.  Algebras, homomorphisms
--------------------------------------------------

From Wikipedia:

  > In universal algebra, an algebra (or algebraic structure) is a set
  > A together with a collection of operations on A.

Example:

> class Monoid a where
>   unit :: a
>   op   :: a -> a -> a

  > After the operations have been specified, the nature of the
  > algebra can be further limited by axioms, which in universal
  > algebra often take the form of identities, or *equational laws*.

Example: Monoid equations

<   ∀ x ∷ a (unit `op` x = x ∧ x `op` unit = x)
<   ∀ x, y, z ∷ a (x `op` (y `op` z) = (x `op` y) `op` z)

  > A homomorphism between two algebras A and B is a function h: A → B
  > from the set A to the set B such that, for every operation fA of A
  > and corresponding fB of B (of arity, say, n),
  > h(fA(x1,...,xn)) = fB(h(x1),...,h(xn)).

Example: Monoid homomorphism

<   h unit        =  unit
<   h (x `op` y)  =  h x `op` h y

> instance Monoid Int where
>   unit          =  0
>   op            =  (+)

> newtype MInt       =  M Int

> instance Monoid MInt where
>   unit            =  M 1
>   op (M m) (M n)  =  M (m * n)

Exercise: characterise the homomorphisms from Int to MInt.

Solution:

Let h : Int -> MInt be a homomorphism.  Then

< h 0 = 1
< h (x + y) = h x * h y

For example

< h (x + x) = h x * h x


But every n in Int is equal to 1 + 1 + ... + 1 n times.  Therefore

< h x = h 1 ** x

Every choice of h 1 induces a homomorphism.

2.  Homomorphism and compositional semantics
--------------------------------------------

Last time, we saw that eval is compositional, while eval' is not.
Another way of phrasing that is to say that eval is a homomorphism,
while eval' is not.  To see this, we need to make explicit the
structure of Expression:

< instance Num Expression where
<   (+)          =  (:+:)
<   (*)          =  (:*:)
<   fromInteger  =  Const . fromInteger

< instance Fractional Expression where

< instance Floating Expression where
<   exp          =  Exp

and so on.

Exercise: complete the type instances for Expression.

For instance, we have

<  eval (e1 :*: e2)  =  eval e1 * eval e2
<  eval (Exp e)      =  exp (eval e)

These properties do not hold for eval', but do hold for evalD.

The numerical classes do not fully do justice to the structure of
expressions, for example, they do not contain an identity operation,
which is needed to translate Id, nor an embedding of doubles, etc.  If
they did, then we could have evaluated expressions more abstractly:

< eval :: GoodClass a => Expression -> a

where "GoodClass" gives exactly the structure we need for the
translation.

Exercise: define GoodClass and instantiate Expression and Double ->
Double as instances of it.  Find another instance of GoodClass.

Therefore, we can always define a homomorphism from Expression to
*any* instance of GoodClass, in an essentially unique way.  In the
language of category theory, Expression is an initial algebra.

Let us explore this in the simpler context of Monoid.  The language of
monoids is given by

> type Var      =  String

> data MExpr    =  Unit
>               |  Op MExpr MExpr
>               |  V Var

Alternatively, we could have parametrised MExpr over the type of
variables.

Just as in the case of FOL terms, we can evaluate an MExpr in a monoid
instance if we are given a way of interpreting variables, also called
an assignment:

> evalM :: Monoid a => (Var -> a) -> MExpr -> a

Once given an f :: Var -> a, the homomorphism condition defines
evalM:

> evalM  f  Unit        =  unit
> evalM  f  (Op e1 e2)  =  op (evalM f e1) (evalM f e2)
> evalM  f  (V x)       =  f x

Observation: In Expression, the role of variables was played by
Double, and the role of the assignment by the identity.

The following correspondence summarises the discussion so far:

    Computer Science         Mathematics
    ------------------       --------------
    DSL                      structure (category, algebra, ...)
    deep embedding           initial algebra
    shallow embedding        any other algebra
    semantics                homomorphism from the initial algebra

3.  Other homomorphisms
-----------------------

Last time, we defined a Num instance for functions with a Num
codomain.  If we have an element of the domain of such a function, we
can use it to obtain a homomorphism from functions to to their
codomains:

< Num a => x -> (x -> a) -> a

As suggested by the type, the homomorphism is just function application:

< apply :: a -> (a -> b) -> b
< apply a f = f a

Indeed, writing h = apply x, we have

     h (f + g)
     
  =  {def apply}
  
     (f + g) x
     
  =  {def + for functions}
  
     f x + g x
     
  =  {def apply}
  
     h f + h g

etc.

Can we do something similar for FD?

The elements of FD a are pairs of functions, so we can take

< apply :: a -> FD a -> (a, a)
< apply a (f, f') = (f a, f' a)

We now have the domain of the homomorphism (FD a) and the homomorphism
itself (apply a), but we are missing the structure on the codomain,
which now consists of pairs (a, a).  In fact, we can *compute* this
structure from the homomorphism condition.  For example:

     h ((f, f') * (g, g'))
     
  =  {def * for FD a}
  
     h (f * g, f' * g + f * g')
     
  =  {def h = apply a}
  
     ((f * g) a, (f' * g + f * g') a)
     
  =  {def * and + for functions}
  
     (f a * g a, f' a * g a + f a * g' a)
     
  =  {homomorphism condition}
  
     h (f, f') * h (g, g')
     
  =  {def h = apply a}
  
     (f a, f' a) * (g a, g' a)

The identity will hold if we take

     (x, x') * (y, y') = (x * y, x' * y + x * y')

Exercise: complete the instance declarations for (Double, Double).

