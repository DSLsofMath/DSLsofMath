> {-# LANGUAGE FlexibleInstances #-}

Types in Mathematics (Part II)
==============================

2. Type classes
---------------

The kind of type inference we presented in the last lecture becomes
automatic with experience in a domain, but is very useful in the
beginning.

The "trick" of looking for an appropriate combinator with which to
pre- or post-compose a function in order to makes types match is often
useful.  It is similar to the casts one does automatically in
expressions such as 4 + 2.5.

One way to understand such casts from the point of view of functional
programming is via *type classes*.  As a reminder, the reason 4 + 2.5
works is because floating point values are members of the class Num,
which includes the member function

<       fromInteger   ::  Integer  ->  a

which coverts integers to the actual type a.

Type classes are related to mathematical structures which, in turn,
are related to DSLs.  The structuralist point of view in mathematics
is that each mathematical domain has its own fundamental structures.
Once these have been identified, one tries to push their study as far
as possible *on their own terms*, i.e., without introducing other
structures.  For example, in group theory, one starts by exploring the
consequences of just the group structure, before one introduces, say,
an order structure and monotonicity.

The type classes of Haskell seem to have been introduced without
relation to their mathematical counterparts, perhaps because of
pragmatic considerations.  For now, we examine the numerical type
classes Num, Fractional, and Floating.

< class  (Eq a, Show a) => Num a  where
<     (+), (-), (*)  :: a -> a -> a
<     negate         :: a -> a
<     abs, signum    :: a -> a
<     fromInteger    :: Integer -> a

This is taken from the Haskell documentation
(https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1350142),
but it appears that Eq and Show are not necessary:

> instance Num a => Num (x -> a) where
>   f + g        =  \x -> f x + g x
>   f - g        =  \x -> f x - g x
>   f * g        =  \x -> f x * g x
>   negate f     =  negate . f
>   abs f        =  abs . f
>   signum f     =  signum . f
>   fromInteger  =  const . fromInteger

< class  (Num a) => Fractional a  where
<     (/)          :: a -> a -> a
<     recip        :: a -> a
<     fromRational :: Rational -> a
<
< class  (Fractional a) => Floating a  where
<     pi                  :: a
<     exp, log, sqrt      :: a -> a
<     (**), logBase       :: a -> a -> a
<     sin, cos, tan       :: a -> a
<     asin, acos, atan    :: a -> a
<     sinh, cosh, tanh    :: a -> a
<     asinh, acosh, atanh :: a -> a
<

We can instantiate these type classes for functions in the same way we
did for Num:

> instance Fractional a => Fractional (x -> a) where
>   recip  f         =  recip . f
>   fromRational     =  const . fromRational

> instance Floating a => Floating (x -> a) where
>   pi       =  const pi
>   exp f    =  exp . f
>   f ** g   =  \ x -> (f x)**(g x)
>   -- and so on

Exercise: complete the instance declarations.

These type classes represent an abstract language of algebraic and
standard operations, abstract in the sense that the exact nature of
the elements involved is not important from the point of view of the
type class, only from that of its implementation.


2. Computing derivatives
------------------------

The "little language" of derivatives:

    D (f + g)    =  D f + D g
    D (f * g)    =  D f * g + f * D g

    D (f ∘ g) x  =  D f (g x) * D g x     -- the chain rule

    D (const α)  =  const 0
    D id         =  const 1
    D (^n) x     =  (n - 1) * (x^(n-1))
    D sin x      =  cos x
    D cos x      =  - (sin x)
    D exp x      =  exp x

and so on.

We observe that we can compute derivatives for any expressions made
out of arithmetical functions, standard functions, and their
compositions.  In other words, the computation of derivatives is based
on a DSL of expressions (representing functions in one variable):

   expression ∷=  const ℝ
               |  id
               |  expression + expression
               |  expression * expression
               |  exp expression
               |  ...

etc.

We can implement this in a datatype:

> data Expression  =  Const Double
>                  |  Id
>                  |  Expression :+: Expression
>                  |  Expression :*: Expression
>                  |  Exp Expression
>                  -- and so on
>                  deriving Show

The intended meaning of elements of the expression type is functions:

> eval  ::  Expression     -> Double -> Double
> eval      (Const alpha)  =  const alpha
> eval      Id             =  id
> eval      (e1 :+: e2)    =  eval e1 + eval e2
> eval      (e1 :*: e2)    =  eval e1 * eval e2
> eval      (Exp e1)       =  exp (eval e1)
> -- and so on

We can implement the derivative of expressions using the rules of
derivatives.  We want to implement a function

> derive  ::  Expression -> Expression

which makes the following diagram commute:

                      eval
   Expression       --------->       Func
    |                                 |
    | derive                          | D
    |                                 |
    v                 eval            v
   Expression       --------->       Func


For any expression e, we want

     eval (derive e) = D (eval e)

For example, let us derive the derive function for Exp e:

     eval (derive (Exp e))

=  {specification of derive above}

     D (eval (Exp e))

=  {def eval}

     D (exp (eval e))

=  {def exp for functions}

     D (exp . eval e)

=  {chain rule}

     (D exp . eval e) * D (eval e)

=  {D rule for exp}

     (exp . eval e) * D (eval e)

=  {specification of derive}

     (exp . eval e) * (eval (derive e))

=  {def. of eval for Exp}

     (eval (Exp e)) * (eval (derive e))

=  {def. of eval for :*:}

     eval (Exp e :*: derive e)

Therefore, the specification is fulfilled by taking

     derive (Exp e) = Exp e :*: derive e

Similarly, we obtain

> derive     (Const alpha)  =  Const 0
> derive     Id             =  Const 1
> derive     (e1 :+: e2)    =  derive e1 :+: derive e2
> derive     (e1 :*: e2)    =  (derive e1 :*: e2) :+: (e1 :*: derive e2)
> derive     (Exp e)        =  Exp e :*: derive e

Exercise: complete the Expression type and the eval and derive
functions.

3. Shallow embeddings
---------------------

The DSL of expressions, whose syntax is given by the type Expression,
turns out to be almost identical to the DSL defined via type classes
in the first part of this lecture.  The correspondence between them is
given by the eval function.

The difference between the two implementations is that the first one
separates more cleanly from the semantical one.  For example, ":+:"
*stands for* a function, while "+" *is* that function.

The second approach is called "shallow embedding" or "almost abstract
syntax".  It can be more economical, since it needs no eval.  The
question is: can we implement derive in the shallow embedding?

Note that the reason the shallow embedding is possible is that the
eval function is a *fold*: first evaluate the sub-expressions of e,
then put the evaluations together without reference to the
sub-expressions.  This is sometimes referred to as "compositionality".

We check whether the semantics of derivatives is compositional.  The
evaluation function for derivatives is

> eval'         ::  Expression -> Double -> Double
> eval'          =  eval . derive

For example:

     eval' (Exp e)

  =  {def eval', function composition}

     eval (derive (Exp e))

  =  {def derive for Exp}

     eval (Exp e :*: derive e)

  =  {def eval for :*:}

     eval (Exp e) :*: eval (derive e)

  =  {def eval for Exp}

     exp (eval e) * eval (derive e)

  =  {def eval'}

     exp (eval e) * eval' e

and the "e" doesn't go away.  The semantics of derivatives is not
compositional.

Or rather, *this* semantics is not compositional.  It is quite clear
that the derivatives cannot be evaluated without, at the same time,
being able to evaluate the functions.  So we can try to do both
evaluations simultaneously:

> evalD :: Expression -> (Double -> Double, Double -> Double)

> evalD e  =  (eval e, eval' e)

Is evalD compositional?

We compute, for example:

     evalD (Exp e)

  =  {specification of evalD}

     (eval (Exp e), eval' (Exp e))

  =  {def eval for Exp and reusing the computation above}

     (exp (eval e), exp (eval e) * eval' e)

  =  {def fst, snd, evalD}

     (exp (fst (evalD e)), (exp (fst (evalD e))) * snd (evalD e))

  =  {perhaps more readable}

     let (f, f') = evalD e
     in (exp f, exp f * f')

This semantics *is* compositional.  We can now define a shallow
embedding for the computation of derivatives, using the numerical type
classes.

> instance Num a => Num (a -> a, a -> a) where
>   (f, f') + (g, g') = (f + g, f' + g')
>   (f, f') * (g, g') = (f * g, f' * g + f * g')
>   fromInteger n     = (fromInteger n, const 0)

Exercise: implement the rest

References
----------
