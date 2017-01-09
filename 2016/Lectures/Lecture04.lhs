Lecture 4: Proofs and Programs in Mathematics
=============================================

1. Curry-Howard isomorphism for quantifiers
-------------------------------------------

In the last [exercises](Exercises/Exercises-2016-01-27--28.lhs) we
have seen that all logical connectives with the exception of negation
can be implemented as datatypes, such that proofs of sentences become
equivalent to producing elements of the corresponding datatype.

The implementation of connectives was based on the interpretation of
introduction and elimination rules as functions.  The functions act on
and produce *evidence* (proofs) for the sentences in their types.  For
example:

<   →-Intro  :  (p -> q) -> p → q     -- note the difference in arrows!
<   →-Elim   :  p → q -> (p -> q)

→-Intro takes a function producing evidence for q from evidence for p,
and constructs evidence for p → q, and →-Elim takes evidence for p → q
and evidence for p to produce evidence for q.

Examining the types of these functions, we see that the "natural"
implementation for the type p → q is

<   type (p → q) = p -> q

in order to have the simplest possible implementations of introduction
and elimination:

<   →-Intro  =  id
<   →-Elim   =  id

In the previous lecture, we have modelled the introduction and
elimination rules for ∀ in the following way:

<   ∀-Intro  :  ((c : Term) -> P c) -> ∀ x (P x)
<   ∀-Elim   :  ∀ x (P x) -> (c : Term) -> P c

By analogy with the case of implication, we find that the "natural"
implementation of ∀ is

<   ∀ x (P x) = (c : Term) -> P c

which turns the introduction and elimination rules into identities.
This type also corresponds to the methods of proof for quantifiers:

- to prove ∀ x (P x), implement a function that, given any atomic term
  c, returns evidence for P c
- given evidence for ∀ x (P x), we can use this evidence to obtain,
  for any atomic term c, evidence for P c

Let us try to find a similar type for the existential quantifier.  We
start with the introduction and elimination rules from the previous
lecture:

<   ∃-Intro  :  (c : Term) -> (P c) ->  ∃ x (P x)
<   ∃-Elim   :  ∃ x (P x) -> ((c : Term) -> P c -> Q) -> Q

Let us rewrite these using the equivalent uncurried form:

<   ∃-Intro  :  (c : Term, P c) ->  ∃ x (P x)
<   ∃-Elim   :  ∃ x (P x) -> ((c : Term, P c) -> Q) -> Q

No choice is going to make ∃-Elim equal to the identity (due to the
presence of Q), but we can take

< type ∃ x (P x) = (c : Term, P c) -- dependent pairs

for which ∃-Intro is the identity and ∃-Elim is trivial (implement
it!).  This definition can be interpreted as "evidence for ∃ x (P x)
is made up of a pair, consisting of an atomic term c and of evidence
that P c holds".

The types (c : Term) -> P c and (c : Term, P c) are *dependent types*,
and cannot be implemented in Haskell.  Nevertheless, they are the
types for which the Curry-Howard isomorphism works, and understanding
∀ and ∃ as dependently-typed functions and dependent pairs,
respectively, can be of great help in proofs, as we will see.

2. Proofs and programs
----------------------

2.1 There exists an infinity of prime numbers

Traditionally, this statement is taken to be equivalent to

  > For every prime number n, there is a prime number strictly greater
  > than it.

To be strictly correct, one would have to prove the equivalence, which
is somewhat tedious.

Formally, we want to prove

<  ∀ n (Prime n → ∃ m (Prime m ∧ n < m))

We note that the domain of discourse is natural numbers.

Using the functional model for quantifiers and connectives, we have

<  (n : Term) -> Prime n -> (m, (Prime m, n < m))

In words: we need to find a function which, given an arbitrary atomic
term n and evidence that n is prime, produces another atomic term m
and a pair of evidences: one that m is prime, the other that m is
strictly greater than n.

A canonical way of defining a function is to give names to the
arguments:

<   f n p = ...

which corresponds precisely to the choice of a "fresh" name in ∀-Intro
and to the assumption step in →-Intro.

The functional interpretation of the formula we want to prove gives us
a *plan*: we are looking for a natural number m for which we can prove
that it is prime and that it is greater than n.

The solution comes from examining the number n! + 1.  This number is
not divisible with 2, 3, ..., n (always leaves a remainder of 1).  We
now apply proof by cases: n! + 1 is either prime, or it is not prime.
If it is prime, then we have found our m: n! + 1 is certainly greater
than n.  Or, if it is not prime, it has at least a prime factor
greater than n (because we have shown it has none up to and including
n), in which case we take one of those prime factors to be our m.
Thus, the conclusion holds in both cases, qed.



2.2 {a_n} convergent ∧ f : ℝ → ℝ continuous → {f a_n} convergent



From [@adams2010calculus], p. 498:

  > We say that sequence {a_n} converges to the limit L, and we write
    $\lim_{n -> \infty} a_n = L$, if for every positive real number ε
    there exists an integer N (which may depend on ε) such that if n ≥ N,
    then |a_n - L| < ε.

Preliminaries:

- What is the universe of discourse?  As opposed to the previous
  example, there are many types at work here:

  + {a_n} denotes a real-valued sequence, i.e., a function defined on
      the natural numbers, with values in the real numbers.  We would
      prefer to write

<         a  :  ℕ  ->  ℝ

  + L and ε denote real numbers
  + N denotes an integer
  + n denotes a natural number (otherwise it cannot be an argument to a)

  How do we handle the plurality of types?

  + set theory: the universe of discourse is a universe of sets, and
    all the rest (natural numbers, integers, real numbers, functions,
    etc.) are just special kinds of sets.  We have seen that it is
    possible to model numbers in set theory (in the assignment).
  + multi-sorted (typed) FOL: we accept the different types and extend
    our FOL to cope with typings (like Haskell).  In particular,
    quantified variables will need to be typed.

We will use a typed logic.

<   lim a_n = L ⟷  ∀ ε : ℝ (ε > 0 → ∃ N : ℤ (∀ n : ℕ (n ≥ N → |a_n - L| < ε)))

Note that ≥ above takes a natural number on the left and an integer on
the right.

-- End of preliminaries

We need to prove

    {f a_n} convergent
⟷
    ∃ l : ℝ (∀ ε : ℝ (ε > 0 → ∃ N : ℤ (∀ n : ℕ (n ≥ N → |f a_n - l| < ε))))

Using the functional interpretation

    (l : ℝ, (ε : ℝ → ε > 0 → (N : ℤ, (n : ℕ → n ≥ N → |f a_n - l| < ε))))

we see that we need to find an l, together with a function which takes
any positive ε into a pair of an N and a function etc.

We take l = f L.  The function we need has the type

    todo ∷ ε : ℝ → ε > 0 → (N : ℤ, (n : ℕ → n ≥ N → |f a_n - f L| < ε))

This looks quite similar to the type of the function given by the continuity of f:

    f continuous at L
⟷
    ∀ ε : ℝ (ε > 0 → (∃ δ : ℝ (δ > 0 ∧ ∀ x : ℝ (|x - L | < δ → |f x - f L| < ε))))

i.e.

    cont ∷ ε : ℝ → ε > 0 → (δ : ℝ, (δ > 0, x : ℝ → |x - L | < δ → |f x - f L| < ε))

We cannot just write

    todo ε εpos = cont ε εpos

because the types don't match, even though they are similar.  We need
to use the δ obtained from cont ε εpos in order to pick an appropriate N.

Let's not forget we have a convergent sequence as well, with limit L:

    lim {a_n} = L
⟷
    ∀ ε : ℝ (ε > 0 → ∃ N : ℤ (∀ n : ℕ (n ≥ N → |a_n - L| < ε)))

i.e., we have a function

    conv ∷ ε : ℝ → ε > 0 → (N : ℤ, n : ℕ → n ≥ N → |a_n -L| < ε)

The idea is to use conv with the δ returned by cont ε εpos:

    conv δ δpos :: (N : ℤ, n : ℕ → n ≥ N → |a_n -L| < δ)

We obtain:

    todo ε εpos = (N, f)
      where
        (δ, (δpos, g)) = cont ε εpos
        (N, h) = conv δ δpos
        f n npos = g a_n (h n npos)

References
-----------
