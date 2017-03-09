---
title: Domain Specific Languages of Mathematics (DAT325)
---

\vspace{-2cm}
\center{\large{Re-Exam 2016--08--23, 14:00--18:00}}

\vspace{2cm}

\begin{description}
\item[Contact] Cezar Ionescu (0729 744 941)
\item[Results] Announced within at most 19 days (by Monday 2016-09-16)
\item[Re-Exam check] Thursday and Friday 2016-08-25 and 26. Both at 12.30-12.55 in EDIT 5468.
\item[Aids] One textbook of your choice (e.g., Adams and Essex, or
Rudin).  No printouts, no lecture notes, no notebooks, etc.
\item[Grades] 3: 40p, 4: 60p, 5: 80p, max: 100p
\end{description}

Remember to write legibly. Good luck!

\newpage

1. [30pts] An *abelian monoid* is a set `M` together with a constant
(nullary operation) `0 ∈ M` and a binary operation `⊕ : M → M → M`
such that:

    - `0` is a unit of `⊕`

        < ∀ x ∈ M           x ⊕ 0 = x  and 0 ⊕ x = x

    - `⊕` is associative

        < ∀ x, y, z ∈ M     x ⊕ (y ⊕ z) = (x ⊕ y) ⊕ z

    - `⊕` is commutative

        < ∀ x, y ∈ M        x ⊕ y = y ⊕ x


    i.  Define a type class `AbMonoid` that corresponds to the abelian
    monoid structure.

    ii. Define a datatype `AbMonoidExp` for the language of abelian
    monoid expressions and define an `AbMonoid` instance for it.  (These
    are expressions formed from applying the monoid operations to the
    appropriate number of arguments, e.g., all the left hand sides and
    right hand sides of the above equations.)

    iii.  Find one other instance of the `AbMonoid` class and give an
    example which is *not* an instance of `AbMonoid`.

    iv.  Define a general evaluator for `AbMonoidExp` expressions on the
    basis of an assignment function.

    v.  Specialise the evaluator to the `AbMonoid` instance defined at
    point iii.  Take three `AbMonoidExp` expressions, give the
    appropriate assignments and compute the results of evaluating the
    three expressions.

    Each question carries 6pts.

----

2. [20pts] In the simplest case of probability theory, we start with a
*finite*, non-empty set `Ω` of *elementary events*.  *Events* are
subsets of `Ω`, i.e. elements of the powerset of Ω, (that is,
$\mathcal{P}$ `Ω`).  A *probability function* `P` associates to each
event a real number between 0 and 1, such that

    i.  `P ∅ = 0`, `P Ω = 1`

    ii.  If events `A` and `B` are disjoint (i.e., `A ∩ B = ∅`),
    then`:                     P A + P B = P (A ∪ B)`.

    Conditional probabilities are defined as follows (*Elementary
    Probability 2nd Edition*, Stirzaker 2003):

    > Let `A` and `B` be events with `P B > 0`.  Given that `B`
      occurs, the *conditional probability* that `A` occurs is denoted
      by `P(A | B)` and defined by

      > `P(A | B) = P(A ∩ B) / P B`

    a)[10pts] What are the types of the elements involved in the
    definition of conditional probability? (`P`, `∩`, `/`, `|`)

    b)[10pts] In the 1933 monograph that set the foundations of
    contemporary probability theory, Kolmogorov used, instead of `P(A
    | B)`, the expression $P_A\, B$.  Type this expression.  Which
    notation do you prefer (provide a *brief* explanation).

----

3. [25pts] Consider the following differential equation:

    $$f''\, t - 5 * f'\, t + 6 * f\, t = e^t,\quad f\, 0 = 1,\quad f'\, 0 = 4$$

    i. [5pts] Write an expression to solve the equation assuming that
       `f` can be expressed by a power series `fs`, that is, use
       `deriv` and `integ` to compute `fs`.

    ii. [20pts] Solve the equation using the Laplace transform.  You
        should need only one formula (and linearity):

    $$ℒ(λt.e^{α*t})\, s  = 1 / (s - α)$$

----

4. [25pts] Consider the classical definition of continuity:

    > *Definition:* Let $X ⊆ ℝ$, and $c ∈ X$.  A function $f : X \to ℝ$ is
*continuous at $c$* if for every $ε > 0$, there exists $δ > 0$ such that,
for every $x$ in the domain of $f$, if $|x - c| < δ$, then $|f x - f c| < ε$.

    i.    [5pts] Write the definition formally, using logical connectives
        and quantifiers.

    ii.   [10pts] Introduce functions and types to simplify the definition.

    iii.  [10pts] Prove the following proposition:  If `f` and `g` are
        continuous at `c`, `f + g` is continuous at `c`.
