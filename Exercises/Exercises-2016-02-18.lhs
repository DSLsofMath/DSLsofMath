Exercises for 2016-02-18
========================

A *ring* is a set A together with two constants (or nullary
operations), 0 and 1, one unary operation, negate, and two binary
operations, + and *, such that

i. 0 is the neutral element of +

    ∀ x ∈ A      x + 0 = 0 + x = x

ii. + is associative

    ∀ x, y, z ∈ A      x + (y + z) = (x + y) + z

iii. negate inverts elements with respect to addition

    ∀ x ∈ A      x + (negate x) = negate x + x = 0

iv. + is commutative

    ∀ x, y ∈ A      x + y = y + x

v. 1 is the unit of *

    ∀ x ∈ A     x * 1 = 1 * x = x

vi. * is associative

    ∀ x, y, z ∈ A      x * (y * z) = (x * y) * z

vii. * distributes over +

    ∀ x, y, z ∈ A      x * (y + z)  =  (x * y) + (x * z)

    ∀ x, y, z ∈ A      (x + y) * z  =  (x * z) + (y * z)

Remarks:
   - i. and ii. say that (A, 0, +) is a monoid
   - i.--iii. say that (A, 0, +, negate) is a group
   - i---iv.  say that (A, 0, +, negate) is a commutative group
   - v. and vi. say that (A, 1, *) is a monoid

Exercises
---------

1.  Define a type class Ring that corresponds to the ring structure.

2.  Define a datatype for the language of ring expressions and define
    a Ring instance for it.

3.  Find two other instances of the Ring class.

4.  Define a general evaluator for Ring expressions on the basis of a
    given assignment function.

5.  Specialise the evaluator to the two Ring instances defined at
    point 3.  Take three ring expressions, give the appropriate
    assignments and compute the results of evaluating, in each case,
    the three expressions.
