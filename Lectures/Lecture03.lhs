Lecture 3: Logic and Functional Programming, Part II
====================================================

E. **Disjunction**

  1) Rules of introduction and elimination for disjunction
        
<          i.  P
<          ...
<          j.  P ∨ Q     (∨-IntroL: i)

<          ...
<          i.  Q
<          ...
<          j.  P ∨ Q     (∨-IntroR: i)

<          ...
<          i.  P ∨ Q
<          ...
<          j.1  P
<          ---------
<          ...
<          j.m  R
<          ---------
<          j+1....
<          ....
<          k.1  Q
<          ---------
<          ...
<          k.n  R
<          k+1. ...
<          ...
<          n.  R        (∨-Elim: i, j, k)

  > Example:

<          1.  P ∨ Q
<          ---------
<          2.1  P
<          ---------
<          2.2  Q ∨ P   (∨-IntroR: 2.1)
<          3.1  Q
<          ---------
<          3.2  Q ∨ P   (∨-IntroL: 3.1)
<          4.  Q ∨ P    (∨-Elim: 1, 2, 3)

  > Example:

   > There exist two irrational numbers a and b such that a^b is
      rational (this is sentence R)

   > *Proof*: we know that $\sqrt 2$ is irrational.  How about $x =
      {\sqrt 2}^{\sqrt 2}$?  This is either irrational or rational.  (P
      = $x$ is irrational, Q = $x$ is rational, and we have P ∨ Q).

   > Assume P: $x$ is irrational.  Then $x^{\sqrt 2} = {\sqrt
      2}^{\sqrt 2 \times \sqrt 2} = {\sqrt 2}^2 = 2$, and 2 is
      rational.  So if P, then R.

   > Assume Q: $x$ is rational.  But then we have that ${\sqrt
      2}^{\sqrt 2}$ is rational, and ${\sqrt 2}$ is irrational, so again
      we have R.
                 
   > Conclusion: R, qed.

  2) Modelling ∨-IntroL,R and ∨-Elim as functions:

<           ∨-IntroL : P -> P ∨ Q
<           ∨-IntroR : Q -> P ∨ Q

<           ∨-Elim : P ∨ Q -> (P -> R) -> (Q -> R) -> R

  > Example: P ∨ ¬ P

<         1.1    ¬ (P ∨ ¬ P)
<         ---------------
<         1.2.1  P
<         ---------------
<         1.2.2  P ∨ ¬ P                      (∨-IntroL: 1.1, 1.2.1)
<         1.2.3  (P ∨ ¬ P) ∧ ¬ (P ∨ ¬ P)      (∧-Intro: 1.2.2, 1.1)
<         1.3    ¬ P                          (¬-Intro: 1.2)
<         1.4    P ∨ ¬ P                      (∨-IntroR: 1.3)
<         1.5    (P ∨ ¬ P) ∧ ¬ (P ∨ ¬ P)      (∧-Intro: 1.4, 1.1)
<         2.     ¬ ¬ (P ∨ ¬ P)                (¬-Intro: 1)
<         3.     P ∨ ¬ P                      (¬-Elim: 2) 

  3) Methods of proof for disjunction

    + To prove that a statement R follows from P ∨ Q, prove that R
      follows from P and that R follows from Q (proof by cases).


E.  **Equality (Identity)**

  1) Rules of introduction and elimination for identity

<          ...
<          i. c = c     (Eq-Intro)

<          ...
<          i. c1 = c2
<          ...
<          j. P c1
<          ...
<          k. P c2      (Eq-Elim: i, j)

  > Example:

<           1. c1 = c2
<           -----------
<           2. c1 = c1  (Eq-Intro)
<           3. c2 = c1  (Eq-Elim: 1, 2 with P x = x = c1)


  2) Modelling with functions

<           Eq-Intro : (c : Term) -> (c = c)
<           Eq-Elim  : (c1, c2 : Term) -> (c1 = c2) -> P c1 -> P c2

  3) Methods of proof for sentences involving equality

    + If x = y, then whatever can be said (in the FOL under
      consideration) about x can also be said about y.  The
      qualification "in the FOL" is necessary: as symbols, "x" and "y"
      may be different.

F.  **Universal quantifier**

  1) Rules of introduction and elimination for universal quantifiers

<          ...
<          i. ∀ x (P x)
<          ...
<          j. P c       (∀-Elim: i)

<          ...
<          i.1  c      -- c "fresh"
<          ------
<          ...
<          i.j  P c
<          ...
<          k. ∀ x (P x) (∀-Intro: i)



  2) Modelling with functions

<           ∀-Elim   :  ∀ x (P x) -> (c : Term) -> P c
<           ∀-Intro  :  ((c : Term) -> P c) -> ∀ x (P x)

  3) Methods of proof for universally quantified sentences

    + To prove ∀ x (P x), introduce a new name that can refer to any
      element of the domain of discourse and prove that it satisfies P
      (universal generalisation).
              
  > Example: prove ∀ x (∀ y (x = y → y = x))

<         1.1 c
<         ----------
<         1.2 c = c                     (=-Intro)
<         1.3.1 d
<         ----------
<         1.3.2.1 c = d                 -- assumption, aiming for →-Intro
<         ----------
<         1.3.2.2 d = c                 (=-Elim: 1.3.2.1, 1.2, P x ≡ (x = c))
<         1.3.3 (c = d) → (d = c)       (→Intro: 1.3.2)
<         1.4 ∀ y (c = y → y = c)       (∀-Intro: 1.3, P x ≡ (c = x) → (x = c))
<         2. ∀ x (∀ y (x = y → y = x)   (∀-Intro: 1, P x ≡ ∀ y (x = y → y = x))


G. **Existential quantifier**

  1) Rules of introduction and elimination for the existential quantifier

<          ...
<          i. ∃ x (P x)
<          ...
<          j.1 c, P c
<          ...
<          j.m Q
<          ...
<          k. Q           (∃-Elim: i, j)

<          ...
<          i. P c
<          ...
<          j. ∃ x (P x)   (∃-Intro: i)


  2) Modelling with functions

<           ∃-Intro  :  (c : Term) -> (P c) ->  ∃ x (P x)
<           ∃-Elim   :  ∃ x (P x) -> ((c : Term) -> P c -> Q) -> Q

  3) Methods of proof involving existential quantifiers

    + To prove something follows from ∃ x (P x), introduce a new name
      c to refer to any one of the individuals (elements of D) that
      satisfy P, and assume P c.  Any sentence you can prove which
      doesn't refer explicitely to c will then be true.

  Example: ∃ x (P x) → ¬ ∀ x ¬ (P x)

<     1.1. ∃ x (P x)
<     --------------
<     1.2.1 c, P c
<     --------------
<     1.2.2.1 ∀ x ¬ (P x)
<     --------------
<     1.2.2.2 ¬ (P c)                     (∀-Elim: 1.2.2.1)
<     1.2.2.3 P c ∨ ¬ (P c)               (∨-Intro: 1.2.1, 1.2.2.3)
<     1.2.3 ¬ ∀ x ¬ (P x)                 (¬-Intro: 1.2.2)
<     1.3 ¬ ∀ x ¬ (P x)                   (∃-Elim: 1.2)
<     2. ∃ x (P x) → ¬ ∀ x ¬ (P x)        (→ Intro: 1)
<     

  Exercise: ¬ ∀ x ¬ (P x) → ∃ x (P x)


Parallels between logical rules and functional programming
-------------------------------------------------------------

  + The rule of ∀-Intro corresponds to the way we define functions: we
    take an arbitrary argument and show how to get the result.

  + The rule of ∨-Elim (aka proof by cases) corresponds to the way we
    define functions by pattern matching: one clause for each pattern

  + The rule of ∧-Intro corresponds to the way we construct records by
    putting together various pieces of data

  +  and so on
     
References
----------
