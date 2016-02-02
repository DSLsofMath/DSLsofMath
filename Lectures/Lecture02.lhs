Lecture 2: Logic and Functional Programming
===========================================

We have seen some FOLs, interpreted terms and wffs, and translated
between different languages.  Now we are going to talk about proofs.

We follow the style of *The Language of First-Order Logic* by Barwise
and Etchemendy [@barwise1993language].

I Formal proofs
---------------

  The shape of formal proofs is
 
          1.  Assumption
          2.  Assumption
          ...
          k.  Assumption
          --------------
        k+1.  Consequence (Rule x with steps i and j)
        ...
        n-1.  Consequence (Rule y with steps p and q)
          n.  CONCLUSION  (Rule z with steps u and v)

  All the assumptions etc. are *sentences* (no WFFs with free
  variables, please).
 
  Rules are usually given in terms of *introduction* and
  *elimination*.

II Rules, methods of proof, and functional programming
------------------------------------------------------

A. **Conjunction**

  1) Rules of introduction and elimination for Conjunction

<          ...
<          i.  P
<          ...
<          j.  Q
<          ...
<          k.  P ∧ Q (∧-Intro: i, j)

<          ...
<          i.  P ∧ Q
<          ...
<          j.  P     (∧-ElimL: i)


<          ...
<          i.  P ∧ Q
<          ...
<          j.  Q     (∧-ElimR: i)


   Example:

<          1.  P ∧ Q
<          --------------
<          2.  P     (∧-ElimL: 1)
<          3.  Q     (∧-ElimR: 1)
<          4.  Q ∧ P (∧-Intro: 3, 2)

  2) Modelling ∧-Intro and ∧-Elim as functions

    a. ∧-Intro takes (uses, requires) evidence for P and evidence for Q
       to produce evidence for P ∧ Q

<         ∧-Intro : P -> Q -> P ∧ Q
  
  >  b. Similarly

<         ∧-ElimL : P ∧ Q -> P
<         ∧-ElimR : P ∧ Q -> Q

  >  > Example:

<         hyp                 :: P ∧ Q
<         c1  =  ∧-ElimL hyp  :: P
<         c2  =  ∧-ElimR hyp  :: Q
<         ∧-Intro (c2, c1)    :: Q ∧ P

  3) Methods of proof for conjunction

    + to prove a statement of the form P ∧ Q, prove both P and Q

B. **Implication**

  1) Rules of introduction and elimination for implication

<     ...
<     i. A → B
<     ...
<     j. A
<     ...
<     k. B       (→-Elim: i, j)

<     ...
<     i.1 A      -- assume A
<     ------
<     i.2 ...
<     ...
<     i.j B
<     i+1. ...
<     ...
<     k.  A → B  (→-Intro: i)

 > Example

<     1.1 P ∧ Q
<     1.2 P          (∧-ElimL: 1.1)
<     2. P ∧ Q → P   (→-Intro: 1)

  2) Modelling →-Elim and →-Intro as functions:

<           →-Elim  : (A → B) -> A -> B
<           →-Intro : (A -> B) -> A → B

   > Example:

<            → Intro ∧-ElimL : P ∧ Q → Q

  3) Methods of proof for implication

    + to prove that P implies Q, assume P and prove Q using the
              assumption

C. **Equivalence (biconditional, if and only if)**

  P $\leftrightarrow$ Q is the same as (P → Q) ∧ (Q → P), so nothing new is
  introduced.
  
D. **Negation**

  1) Rules of introduction and elimination for negation
        
<          ...
<          i.  ¬ ¬ P
<          ...
<          j.  P       (¬-Elim: i)

<          ...
<          i.1 P
<          ...
<          i.m Q ∧ ¬ Q
<          ...
<          j. ¬ P      (¬-Intro: i)
           
   Example:

   1. Q ∧ ¬ Q
   ------------
   2.1 ¬ P
   ------------
   2.2  Q ∧ ¬ Q     (Repeat 1)
   3. ¬ ¬ P         (¬-Intro: 2)
   4. P             (¬-Elim: 3)

   2) Modelling ¬-Intro and ¬-Elim with functions:

<           ¬-Elim : ¬ ¬ P   ->   P
<           ¬-Intro : (P -> Q ∧ ¬ Q) -> ¬ P

   3) Methods of proof for negation

     + to prove the negation of a statement, assume that the statement
       holds and derive a contradiction (proof by contradiction)

  > Example:

  > ${\sqrt 2}$ is irrational.

  > *Proof*: ${\sqrt 2}$ irrational is the same thing as ${\sqrt 2}$ is
  > *not rational*.  So the proposition to prove is ¬ P, where P =
  > "${\sqrt 2}$ is rational".

  > Assume P: ${\sqrt 2}$ is rational.  That means that there exist
  > natural numbers $m$ and $n$ such that

   > ${\sqrt 2} = m / n$

  > We factor out the common divisors of $m$ and $n$ and obtain

   >  ${\sqrt 2} = m' / n'$

  > where $m'$ and $n'$ have no common factors (in particular, they
  > are not both even).

  > We have

  > ${\sqrt 2} = m' / n'$
  
  > $\Longleftrightarrow$

  > $2 = {m'}^2 / {n'}^2$
  
  > $\Longleftrightarrow$
  
  > $2 \times {n'}^2 = {m'}^2$
  
  > $\Longrightarrow$
  
  > ${m'}^2$ is even
  
  > $\Longrightarrow$  (Exercise!)
  
  > $m'$ is even
  
  > $\Longrightarrow$
  
  > ∃ k such that $m' = 2 \times k$
  
  > $\Longrightarrow$
  
  > $2 \times {n'}^2 = 4 \times k^2$
  
  > $\Longrightarrow$
  
  > ${n'}^2 = 2 \times k^2$
  
  > $\Longrightarrow$
  
  > $n'$ is even

  > Therefore, $m'$ and $n'$ are both even, and they are not both even:
    contradiction.

  > Therefore, ¬ P must hold: ${\sqrt 2}$ is irrational, qed.
