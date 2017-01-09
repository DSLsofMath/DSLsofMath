> {-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ > 708
> {-# LANGUAGE EmptyCase #-}
#else
> import Unsafe.Coerce
#endif

> import AbstractFOL

== Short technical note ==
For these exercises, you might find it useful to take a look at typed holes, a
feature which is enabled by default in GHC and available (the same way as the
language extension above EmptyCase) version 7.8.1 onwards:
  https://wiki.haskell.org/GHC/Typed_holes

If you are familiar with Agda, these will be familiar to use. In summary, when
trying to code up the definition of some expression (which you have already
typed) you can get GHC's type checker to help you out a little in seeing how far
you might be from forming the expression you want. That is, how far you are from
constructing something of the appropriate type.

Take example0 below, and say you are writing:

< example0 e = andIntro (_ e)  _

When loading the module, GHC will tell you which types your holes "_" should
have for the expression to be type correct.
==========================


Exercises for 2016-01-27 and 2016-01-28
---------------------------------------

The propositional fragment of FOL is given by the rules for ∧, →, ⟷,
¬, ∨.

We can use the Haskell type checker to check proofs in this fragment,
using the functional models for introduction and elimination rules.
Examine the file [AbstractFOL.lhs](AbstractFOL.lhs), which introduces
an empty datatype for every connective (except ⟷ ), and corresponding
types for the introduction and elimination rules.  The introduction
and elimination rules are explicitly left "undefined", but we can
still combine them and type check the results.  For example:

> example0        ::  And p q -> And q p
> example0 evApq   =  andIntro (andElimR evApq) (andElimL evApq)

Notice that Haskell will not accept

< example0 evApq   =  andIntro (andElimL evApq) (andElimR evApq)

Another example:

> example1          ::  And q (Not q) -> p
> example1 evAqnq    =  notElim (notIntro (\ hyp_p -> evAqnq))

On to the exercises.

1.  Prove

<   Impl (And p q) q
<   Or p q -> Or q p
<   Or p (Not p)

2.  Translate to Haskell and prove the De Morgan laws:

<   ¬ (p ∨ q) ⟷  ¬p ∧ ¬q
<   ¬ (p ∧ q) ⟷  ¬p ∨ ¬q

(translate equivalence to conjunction of two implications).

3.  So far, the implementation of the datatypes has played no role.
To make this clearer: define the types for connectives in AbstractFol
in any way you wish, e.g.:

<   And p q  =  A ()
<   Not p    =  B p

etc. as long as you still export only the data types, and not the
constructors.  Convince yourself that the proofs given above still
work and that the type checker can indeed be used as a poor man's
proof checker.

4.  The introduction and elimination rules suggest that some
implementations of the datatypes for connectives might be more
reasonable than others.  We have seen that the type of evidence for
"p → q" is very similar to the type of functions "p -> q", so it would
make sense to define

<   type Impl p q  =  (p -> q)

Similarly, ∧-ElimL and ∧-ElimR behave like the functions fst and snd
on pairs, so we can take

<   type And p q  =  (p, q)

while the notion of proof by cases is very similar to that of writing
functions by pattern-matching on the various clauses, making p ∨ q
similar to Either:

<   type Or p q   =  Either p q

  a. Define and implement the corresponding introduction and
     implementation rules as functions.

  b. Compare proving the distributivity laws

<   (p ∧ q) ∨ r ⟷  (p ∨ r) ∧ (q ∨ r)
<   (p ∨ q) ∧ r ⟷  (p ∧ r) ∨ (q ∧ r)

  using the "undefined" introduction and elimination rules, with
  writing the corresponding functions with the given implementations
  of the datatypes.  The first law, for example, requires a pair of
  functions:

<   (Either (p, q) r -> (Either p r, Either q r),
<    (Either p r, Either q r) -> Either (p, q) r)

**Moral:** The natural question is: is it true that every time we find
an implementation using the "pairs, ->, Either" translation of
sentences, we can also find one using the "undefined" introduction and
elimination rules?  The answer, perhaps surprisingly, is *yes*, as
long as the functions we write are total.  This result is known as
*the Curry-Howard isomorphism*.

7.  Can we extend the Curry-Howard isomorphism to formulas with ¬?  In
other words, is there a type that we could use to define Not p, which
would work together with pairs, ->, and Either to give a full
translation of sentential logic?

Unfortunately, we cannot.  The best that can be done is to define an
empty type

> data Empty

and define Not as

< type Not p   =  p  ->  Empty

The reason for this definition is: when p is Empty, the type Not p is
not empty: it contains the identity

< id     ::  Empty -> Empty

When p is not Empty (and therefore is true), there is no (total,
defined) function of type p -> Empty, and therefore Not p is false.

Moreover, mathematically, an empty set acts as a contradiction:
there is exactly one function from the empty set to any other set,
namely the empty function.  Thus, if we had an element of the empty
set, we could obtain an element of any other set.

Now to the exercise:

Implement notIntro using the definition of Not above, i.e., find a function

< notIntro   ::  (p -> (q, q -> Empty)) -> (p -> Empty)

Using

> contraHey    ::  Empty -> p
#if __GLASGOW_HASKELL__ > 708
> contraHey x   =  case x of {}
#else
> contraHey x   =  unsafeCoerce x
#endif


prove

< q ∧ ¬ q → p

You will, however, not be able to prove p ∨ ¬ p (try it!).

Prove

< ¬ p ∨ ¬ q  → ¬ (p ∧ q)

but you will not be able to prove the converse.

8.  The implementation Not p = p -> Empty is not adequate for
representing the sentential fragment of FOL, but it is adequate for
*constructive logic* (also known as *intuitionistic*).  In
constructive logic, the ¬ p is *defined* as p -> ⊥, and the following
elimination rule is given for ⊥

<     ...
<     i. ⊥
<     ...
<     j. p      (⊥-Elim: i)

corresponding to the principle that everything follows from a
contradiction ("if you believe ⊥, you believe everything").

Every sentence provable in constructive logic is provable in classical
logic, but the converse, as we have seen in the previous exercise,
does not hold.  On the other hand, there is no sentence in classical
logic which would be contradicted in constructive logic.  In
particular, while we cannot prove p ∨ ¬ p, we *can* prove
(constructively!) that there is no p for which ¬ (p ∨ ¬ p), i.e., that
the sentence ¬ ¬ (p ∨ ¬p) is always true.

Show this by implementing the following function:

< noContra :: (Either p (p -> Empty) -> Empty) -> Empty
