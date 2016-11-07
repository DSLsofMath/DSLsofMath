* L2(+3): propositional calculus + "german" quantifiers


From     (A_1 and A_2)   -- binary and
through  And_i A_i       -- n-ary and
to       And_x A_x       -- X-ary and (forall quantifier)

then

from     (A_1 or A_2)    -- binary or
through  Or_i A_i        -- n-ary or
to       Or_x A_x        -- X-ary or (exists quantifier)

----

In both cases we need introduction and elimination rules.

----

But how do we get to methods of proofs at all?

Start from a tautology:

<   evP evA (Impl (And p q) p)
< =
<   not (evP evA (And p q)) || epV evA p
< =
<   not (evP evA p && evP evA q) || epV evA p

Now we can do case analysis on the repeated subterm "evP evA p". In
both cases the expression evaluates to True. Then the tautology can be
added as a proof rule.

----

Examples:

for and:

  P & Q -> Q & P

for proof by cases:

  find a, b \in R - Q such that a^b \in Q

----------------


A_1 A_2 ... A_n
---------------        <=>     A_1 /\ A_2 ... /\ A_n -> B
     B                            is a tautology

But how do you show that something is a tautology? For finite cases we
can enumerate cases and check them all. In general that is not
possible. Then we need some "proof rules". One rule we have seen above:

  Impl (And p q) p    is a tautology for any p and q

we can call the corresponding proof rule

  and-elim-left

----------------

If we start from the simple rule for and-intro

A   B
-----   <=>    A /\ B -> A /\ B  is a tautology
 A&B

Notice that we need an even simpler rule on the right:

A
-       <=>   A -> A is a tautology
A

----------------

Rules and types:

TODO

and-intro :

----------------

Example: (swap - but only say that at the end)

and-intro ?1 ?2 : Q /\ P
  where ?1 : Q
        ?2 : P

and-intro (and-elim-right ?3) (and-elim-left ?3) : Q /\ P
  where ?3 : P & Q

and-intro (and-elim-right pq) (and-elim-left pq) : Q /\ P

thus

\pq -> (the above) : P /\ Q -> Q /\ P
