Student question:

I have a question about one of the exercises for week 3. What is meant
by "a functional representation" in question 1 c?
[... details elided ...]

Answer by Patrik:

From 1b (slightly modified):

<    P a L ε = (ε > 0) → ∃ N : ℕ. (∀ n : ℕ. (n ≥ N) → (|a_n - L| < ε))

The statement "the sequence {a_n} is convergent" is formalised as

<    ∃ L : ℝ (∀ ε : ℝ (P a L ε))

The formalisation of "the sequence {a_n} is not convergent" is therefore

<    ¬ ∃ L : ℝ (∀ ε : ℝ (P a L ε))

Solution sketch:

First push the negation through the quantifiers

  ¬ ∃ L : ℝ (∀ ε : ℝ (P a L ε))
<=>
  ∀ L : ℝ (¬ ∀ ε : ℝ (P a L ε))
<=>
  ∀ L : ℝ (∃ ε : ℝ (¬ P a L ε))

let nP a L ε = ¬ P a L ε

then

  nP a L ε
<=> {- def. of nP and P -}
  ¬ ((ε > 0) → ∃ N : ℕ. (∀ n : ℕ. (n ≥ N) → (|a_n - L| < ε)))
<=> {- Use ¬(A->B) = A ∧ ¬B -}
  (ε > 0) ∧ ¬(∃ N : ℕ. (∀ n : ℕ. (n ≥ N) → (|a_n - L| < ε)))
<=> {- from ¬∃ to ∀¬ -}
  (ε > 0) ∧ ∀ N : ℕ. ¬((∀ n : ℕ. (n ≥ N) → (|a_n - L| < ε)))
<=> {- from ¬∀ to ¬∃ -}
  (ε > 0) ∧ ∀ N : ℕ. (∃ n : ℕ. ¬((n ≥ N) → (|a_n - L| < ε)))
<=> {- Use ¬(A->B) = A ∧ ¬B -}
  (ε > 0) ∧ ∀ N : ℕ. (∃ n : ℕ. (n ≥ N) ∧ (|a_n - L| >= ε))
<=> {- Introduce function |getn| -}
  (ε > 0) ∧ ∃ getn : ℕ -> ℕ. ∀ N : ℕ.
    ((getn N  ≥ N) ∧ (|a_(getn N) - L| >= ε))

At the core of this is the function |getn|

< nP a L ε ~= ∃ getn : ℕ -> ℕ. [...]

Thus the functional interpretation of the theorem |nP a L ε| is the
type |ℕ -> ℕ| and a proof of the theorem is essentially a function
|getn| which takes any |N| to a larger |n| for which |a| is still not
ε-close to |L|. The function |getn| has to have the property that
|(getn N ≥ N) ∧ (|a_(getn N) - L| >= ε)| for all |N|.

For example, with |a = ((-1)^) = [1, -1, 1, -1, ...]|, |ε=1.5|, and
|L=1| we can prove |nP a L ε| by providing the function |getn =
mayAddOne| where

> mayAddOne n = if even n then 1+n else n

We can see that |a (getn N) = -1| for all |N| so that

  (|a_(getn N) - L| >= ε)
<=>
  (|a (mayAddOne N) - 1| >= 1.5)
<=>
  (|-1 - 1| >= 1.5)
<=>
  (|-2| >= 1.5)
<=>
  (2 >= 1.5)
<=>
  True

----

We can step out through the next two quantifiers to the
"non-convergence of a" theorem:

<  theorem =  ∀ L : ℝ.  ∃ ε : ℝ+.  nP a L ε

Again we have the pattern "forall - exists" which means that the a
proof : theorem is essentially a function (with certain properties).

<  theorem ~= ∃ getε : ℝ -> ℝ+.  ∀ L : ℝ.  nP a L (getε L)

Here the function is from |L : ℝ| to a pair of |ε : ℝ+| and a proof of
|nP a L ε|. The inner proof is just a function |ℕ -> ℕ|.

Thus the functional interpretation of the top level "proof" is

  getε : ℝ -> (ℝ+, ℕ -> ℕ)

Thus, to prove that certain series |a| does not converge, we need to
provide a function |getε| which for any "candidate limit" |L : ℝ|
shows that there is a positive |ε| and a function |getn| such that |a|
for large |N| always is at least |ε| away from |L|.

Concretely, for |a = ((-1)^)| we can define

< getε 1    = (2, {- add one if even -})
< getε (-1) = (2, {- add one if odd  -})
< getε L    = (min (|L-1|, |L-(-1)|), id)
