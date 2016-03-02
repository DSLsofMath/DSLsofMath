Basic Concepts of Analysis
==========================

Mathematical definitions very often introduce one or more *functions*.
Usually, these functions are left in an implicit form, which makes it
hard to use them in proofs.  A useful device for making them explicit
is to type the elements involved in the definitions, another is to use
the functional interpretation of logical connectives and quantifiers.
In the following, we give a number of simple examples.

1. Limit point
--------------

*Definition* (adapted from @rudin1964principles, page 28):  Let `X` be a
 subset of ℝ.  A point `p ∈ ℝ` is a limit point of `X` if for every `ε
 > 0`, there exists `q ∈ X` such that `q ≠ p` and `|q - p| < ε`.

We can write the definition more formally:

< p limit point of X  ⟺   ∀ ε > 0 ∃ q ∈ X (q ≠ p ∧ |q - p| < ε)

< q : ℝ+ → X - {p}

< D : ℝ → ℝ+ → 𝓟 ℝ
< D a ε = {x | |x - a| < ε}

< q ε ∈ D p ε

< p limit point of X ⟺   ∃ q : ℝ_{> 0} → X - {p}   ∀ ε   q ε ∈ D p ε

The function `D` has several interesting properties.  For example:
  - `ε₁ < ε₂ ⇒  D a ε₁ ⊆ D a ε_2`
  - `a₁ = a₂ ⟺   ∀ ε > 0   D a₁ ε ⊆ D a₂ ε`

*Proposition*:  If `X` is finite, then it has no limit points.

*Proof*: We have to show

< ∀ p ∈ ℝ  ¬ p limit point of X

< ⟺

< ∀ p ∈ ℝ  ¬ ∃ q : ℝ_{> 0} → X - {p}   ∀ ε > 0  q ε ∈ D p ε

< ⟺

< ∀ p ∈ ℝ  ∀ q : ℝ_{> 0} → X - {p}   ∃ ε > 0  q ε ∉ D p ε

Therefore, taking an arbitrary `p` and an arbitrary `q`, we need to
find an `ε > 0` such that, no matter what element of `X-{p}` `q ε` is,
it is more than `ε` distance away from `p`.

We remark that this is equivalent to defining a function, associating
to each `p` and `q` a positive ε with the respective property:
`∀ x ∈ X-{p} (x = q ε ⇒ x ∉ D p ε)`.

To simplify this property, we introduce the *image* function:

<  I : (A → B) → 𝓟 A → 𝓟 B

<  I f X = {f a | a ∈ X}

We have

< (∀ a ∈ A  f a ∉ Y)   ⟸     I f A  ∩  Y  =  ∅

In our case

< q ε ∉ D p ε

< ⟸

< I q ℝ_{> 0} ∩ D p ε = ∅

< ⟸

< X - {p} ∩ D p ε = ∅

Since `X` is finite, so is `X - {p}` and therefore the following is
well defined:

< ε = 1/2 min { |x - p|  | x ∈ X - {p} }

Then, for any `x ∈ X - {p}`, we have

< |x - p| ≥ min { | x - p|  | x ∈ X - {p} } = 2 ε

< ⇒

< |x - p| > ε

< ⇒

< x ∉ D p ε

2.  The limit of a sequence
---------------------------

*Definition* [@adams2010calculus, page 498]:

  > **Limit of a sequence**

  > We say that sequence `{a_n}` converges to the limit `L`, and we
    write `lim_{n→∞} a_n = L`, if for every positive real number `ε`
    there exists an integer `N` (which may depend on `epsilon`) such that if
    `n > N`, then `|a_n - L| < epsilon`.

We have

< a : ℕ → A    (A ⊆ ℝ)

< lim a = L  ⟺   ∀ ε > 0 ∃ N ∀ n ≥ N  |a_n - L| < ε

We overload the image function for sequences "from N onwards":

< I a N = {a n | n ≥ N}

The definition is equivalent to

< lim a = L  ⟺   ∃ N : ℝ_{> 0} → ℕ  ∀ ε > 0    I a (N ε) ⊆  D L ε

*Proposition*: The limit of a sequence is unique.

*Proof*:  We have to show that `lim a = L₁ ∧ lim a = L₂ ⇒ L₁ = L`.

From `lim a = L₁` it follows that we have `N₁ : ℝ_{> 0} → ℕ` such that
`∀ ε > 0 I a (N₂ ε) ⊆ D L₁ ε`; similarly, from `lim a = L₂` we have
`N₂` etc.

Therefore, for any `ε > 0`

< I a (N₁ ε) ⊆ D L₁ ε  ∧  I a (N₂ ε) ⊆ D L₂ ε

< ⇒  {∩ is monotonic}

< I a (N₁ ε) ∩ I a (N₂ ε)    ⊆     D L₁ ε  ∩  D L₂ ε

< ⇒  {property of I}

< I a (max (N₁ ε, N₂ ε))     ⊆     D L₁ ε  ∩  D L₂ ε

< ⇒  {∀ n    I a n ≠ ∅}

< D L₁ ε  ∩  D L₂ ε ≠ ∅

< ⇒  {taking an arbitrary `x` in `D L₁ ε  ∩  D L₂ ε`}

< |x - L₁| + |x - L₂| < 2 * ε

< ⇒  {triangle inequality}

< |L₁ - L₂| < 2 * ε

Therefore, for any `ε > 0`, `0 ≤ |L₁ - L₂| < ε`, therefore
`|L₁ - L₂| = 0`, and so `L₁ = L₂`.
