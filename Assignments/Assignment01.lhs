Assignment 1: Translating Peano arithmetic to (finite) set theory
-----------------------------------------------------------------

In this assignment, we explore the role of set theory as a universal
modelling language, by setting up an FOL for (finite) sets and using
it to interpret the FOL of arithmetic.

1.  Define an FOL for finite sets, containing
      + one name: Empty
      + function symbols: Union, Intersection, Singleton
          - you can also try Powerset
      + two predicates: Elem, Subset

2.  Interpret the language using the Haskell datatype Set.  Use the
eval and check functions from yesterday's exercises session.  Hint:
you will not be able to use, e.g., Set String as a domain (try it!).
Think instead of Rose trees as arbitrarily deep nested lists, and come
up with something similar for sets.

3.  The von Neumann encoding of natural numbers as sets is defined
recursively as

<   vonNeumann 0        =  empty
<   vonNeumann (n + 1)  =  union (vonNeumann n) (singleton (vonNeumann n))

Note that for n1 <= n2 we have isSubsetOf (vonNeumann n1) (vonNeumann n2).

Use the von Neumann encoding to translate terms and WFFs from the
language of arithmetic into that of finite sets.  Use the check
function for finite sets for WFFs about natural numbers.

- *Submission*: Assignments are to be submitted via Fire.
- *Deadline*:   Tuesday, 2016-02-02, 23:59.
- *Grading*: Discussions with each of the teams during the exercises
  session of Thursday, 2016-02-04.
