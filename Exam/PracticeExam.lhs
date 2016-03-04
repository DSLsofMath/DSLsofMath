---
title: Domain Specific Languages of Mathematics
subtitle: Practice Exam
---

\begin{description}
\item[Contact] Patrik Jansson (phone number)
\item[Results] Announced within ?
\item[Aids] One textbook of your choice (e.g., Adams and Essex, or
Rudin).  No printouts, no lecture notes, no notebooks, etc.
\item[Grades] 3: 40p, 4: 60p, 5: 80p, max: 100p
\end{description}

Remember to write legibly. Good luck!

\newpage
1. [30pts]

    A vector space over ℝ is a set `V` together with a constant (or
   nullary) operation `0 : V`, an operation `+ : V -> V -> V`, and an
   *external* operation `⋅ : ℝ -> V -> V`, such that

    - `0` is the unit of `+`:

        <        ∀ v ∈ V   v + 0 = 0 + v = v

    - `+` is associative:

        <        ∀ v₁, v₂, v₃ ∈ V   (v₁ + v₂) + v₃ = v₁ + (v₂ + v₃)

    - `+` is invertible:

        <        ∀ v ∈ V  ∃ (- v) ∈ V   v + (-v) = (-v) + v = 0

    - `+` is commutative:

        <        ∀ v₁, v₂ ∈ V    v₁ + v₂  =  v₂ + v₁

        *Remarks:*

        - we usually denote `v₁ + (- v₂) = v₁ - v₂`
        - the first two conditions say that `(V, +, 0)` is a *monoid*
        - the first three conditions say that `(V, +, 0)` is a *group*
        - the four conditions say that `(V, +, 0)` is a *commutative group*

    - ` ⋅ ` is associative

        <        ∀ x₁, x₂ ∈ ℝ, v ∈ V     x₁ ⋅ (x₂ ⋅ v) = (x₁ * x₂) ⋅ v

        *Remark*: `*` denotes the standard multiplication in ℝ

    -  1 is a unit of ` ⋅ `:

        <        ∀ v ∈ V        1 ⋅ v  =  v

    -  ` ⋅ ` distributes over `+`:

        <        ∀ x ∈ ℝ, v₁, v₂ ∈ V     x ⋅ (v₁ + v₂) = x ⋅ v₁ + x ⋅ v₂

    -  ` ⋅ ` distributes over `+`

        <       ∀ x₁, x₂ ∈ ℝ, v ∈ V      (x₁ + x₂) ⋅ v = x₁ ⋅ v + x₂ ⋅ v

    i.  Define a type class `Vector` that corresponds to the vector
        space over ℝ structure.

    ii. Define a datatype for the language of vector space expressions
        and define a `Vector` instance for it.

    iii. Find two other instances of the `Vector` class.

    iv. Define a general evaluator for `Vector` expressions on the
        basis of *two* given assignment functions.

    v. Specialise the evaluator to the two `Vector` instances defined
       at point iii.  Take three vector expressions, give the appropriate
       assignments and compute the results of evaluating, in each case,
       the three expressions.

    Each question carries 6pts.

\newpage
2. [25pts]

    Consider the following differential equation:

    <     f'' t - 2 ⋅ f' t + f t - 2 = 3 * e^{2 * t}, f 0 = 5, f' 0 = 6

    i. [10pts] Solve the equation assuming that `f` can be expressed by
   a power series `fs`, that is, use `deriv` and `integ` to compute
   `fs`.  What are the first three coefficients of `fs`?

    ii. [15pts] Solve the equation using the Laplace transform.  You
   should need only one formula:

    <    ℒ (e^(α*t)) s  = 1 / (s - α)

3. [25pts]

    Consider the following definition for the limit of a sequence,
   adapted from Adams and Essex 2010:

    > We say that sequence ${a_n}$ converges to the limit $L$, and we
    write $lim_{n→∞} a_n = L$, if for every positive real number $ε$
    there exists an integer $N$ (which may depend on $ε$) such
    that if $n > N$, then $|a_n - L| < ε$.

    i. [5pts] Write the definition formally, using logical connectives
   and quantifiers.

    ii. [10pts] Introduce functions and types to simplify the definition.

    iii.  [10pts] Prove the following proposition:  If `lim a = L₁` and `lim b
   = L₂`, then `lim (a + b) = L₁ + L₂`.

4. [20pts]

    Consider the following text from Mac Lane's *Mathematics: Form and
    Function* (page 168):

    > If $z = g(y)$ and $y = h(x)$ are two functions with continuous
       derivatives, then in the relevant range $z = g(h(x))$ is a
       function of $x$ and has derivative
     $$z' (x) = g'(y) * h'(x)$$

    Give the types of the elements involved (`x`, `y`, `z`, `g`, `h`,
   `z'`, `g'`, `h'`, `*` and `'`).
