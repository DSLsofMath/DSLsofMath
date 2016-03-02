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

Therefore, for any `ε > 0`, `0 ≤ |L₁ - L₂| < ε`, therefore `|L₁ - L₂|
= 0`, and so `L₁ =  L₂`.

3.  The limit of a function
---------------------------

*Definition* [@adams2010calculus, page 88]:

 > **A formal definition of limit**

 > We say that `f(x)` **approaches the limit** `L` **as** `x`
   **approaches** `a`, and we write

 > `lim_{x → a} f(x) = L`,

 > if the following condition is satisfied: for every number `ε > 0`
   there exists a number `δ > 0`, possibly depending on `epsilon`,
   such that if `0 < |x - a| < δ`, then `x` belongs to the domain of
   `f` and `|f (x) - L| < epsilon`.

Here, `f : X → ℝ` for some `X ⊆ ℝ`.  We do not necessarily have `a ∈ X`.

This definition is stronger than the standard one, here adapted from
@rudin1964principles (page 72):

*Definition*

  > Let `f : X → ℝ` and `p` a limit point of `X`.  We write

<      lim_{x → p} f (x)  =  q

  > if there is a point `q ∈ ℝ` such that for every `ε > 0` there
    exists `δ > 0` such that

<      |f x - q| < ε

  > for all `x ∈ X` such that

<      |x - p| < δ

We have

<      lim_{x → p} f (x)  =  q

< ⟺

<      ∃ δ : ℝ_{> 0} → ℝ_{> 0}  ∀ ε > 0   I f (D p (δ ε) ∩ X) ⊆ D q ε

The limit point condition guarantees that `(D p ρ ∩ X) ≠ ∅` for
any `ρ > 0`.

We will write `lim f p` instead of `lim_{x → p} f (x)`.

*Proposition:*  Let `f : X -> ℝ` and `p` a limit point of `X`.  Then

<  lim f p = q    ⟺    ∀ a : ℕ -> ℝ - {p}  lim a = p  ⇒ lim (f ∘ a) = q

*Proof:*

"⇒"

We have to prove an implication.  The standard approach is to assume
the antecedent and prove the consequent.

Assuming `lim f p = q` is equivalent to assuming we are given

<   δ : ℝ_{> 0} → ℝ_{> 0}

such that

<   ∀ ε > 0   I f (D p (δ ε) ∩ X) ⊆ D q ε    (1)

We need now to prove

< ∀ a : ℕ -> ℝ - {p}  lim a = p  ⇒ lim (f ∘ a) = q

To do that, we take an arbitrary sequence `a : ℕ -> ℝ - {p}` and prove
of it that

< lim a = p  ⇒ lim (f ∘ a) = q

Again, an implication.  We take the same approach, and assume that
`lim a = p`, which is equivalent to having

<   N : ℝ_{> 0} → ℕ

such that

<   ∀ ε > 0    I a (N ε) ⊆  D p ε             (2)

With all these elements at our disposal, we have to prove

< lim (f ∘ a) = q

i.e., to define

<   N' : ℝ_{> 0} → ℕ

such that

<   ∀ ε > 0    I (f ∘ a) (N' ε) ⊆  D q ε

We define `N'` for an arbitrary ε > 0, such that

< I (f ∘ a) (N' ε) ⊆  D q ε

From (1), we have that

< I f (D p (δ ε) ∩ X) ⊆ D q ε

so that it is enough to choose `N' ε` so that

< I (f ∘ a) (N' ε) ⊆ I f (D p (δ ε) ∩ X)       (3)

The image function is monotonic:

< ∀ f, X₁, X₂     X₁ ⊆ X₂   ⇒   I f X1₁  ⊆  I f X₂

To apply that, we need to have `I f` on the left-hand side of (3) as
well.  We have

< I (f ∘ a) (N' ε)

< =  {def I}

< { (f ∘ a) n | n ≥ N' ε}

< =  {composition}

< { f (a n)   | n ≥ N' ε}

< =  {def I}

< I f {a n | n ≥ N' ε}

< =  {def I}

< I f (I a (N' ε))

 > Exercise: Prove that
<     ∀ f : X -> Y, g : Y -> Z   I (g ∘ f) X  =  I g (f X)

Therefore, (3) is equivalent to

< I f (I a (N' ε)) ⊆ I f (D p (δ ε) ∩ X)

< ⟸   {monotonicity of I f}

< I a (N' ε) ⊆ D p (δ ε) ∩ X

From (2), we have that

< I a (N ε) ⊆  D p ε

Therefore, if we set

< N' ε  =  N (δ ε)

we obtain

< I a (N' ε) ⊆ D p (δ ε) ∩ X

< ⟺   {N' ε = N (δ ε)}

< I a (N (δ ε)) ⊆ D p (δ ε) ∩ X

< ⟺   {∩}

< I a (N (δ ε)) ⊆ D p (δ ε)  ∧  I a (N (δ ε)) ⊆ X

< ⟺   {(2) and type of `a`}

< True

Therefore, taking `N' = N ∘ delta` is the function we were looking
for.

Exercise: prove "⟸".

4. Continuity
-------------

The classical definition of continuity is (e.g., @rudin1964principles,
p. 74):

*Definition:* Let `X ⊆ ℝ, X ≠ ∅`, and `c ∈ X`.  A function `f : X ->
 ℝ` is *continuous at `c`* if

< ∀ ε > 0 ∃ δ > 0 ∀ x ∈ X   |x - c| < δ  ⇒  |f x - f c| < ε

We follow the same line of thought above and introduce the `D` and `I`
functions.  We obtain the equivalent condition

< ∃ δ : ℝ_{> 0} -> ℝ_{> 0} ∀ ε > 0  I f (D c (δ ε)) ⊆ D (f c) ε

Adams and Essex (-@adams2010calculus, p.78) give another definition.
First they define

  > A point `P` in the domain of such a function is called an **interior
  > point** of the domain if it belongs to some open interval
  > contained in the domain.

Then, they define continuity at an interior point:

*Definition:* We say that a function `f` is **continuous** at an
 interior point `c` of its domain if

<       lim_{x → c} f (x)  =  f (c).

If either `lim_{x → c} f(x)` fails to exist, or it exists but is not
equal to `f (c)`, then we say that `f` is **discontinuous** at `c`.

The definition of limit of a function they have chosen forces them now
to define lateral limits, and then right and left continuity.  Since
we know what a limit point is, we can simply re-write their definition
as

*Definition:* We say that a function `f : X -> ℝ` is **continuous** at
 a limit point `c ∈ X` if

<       lim_{x → c} f (x)  =  f (c).

The Adams and Essex definition is more restrictive, since it requires
`c` to be a limit point.

*Proposition:* If `c ∈ X` is a limit point of `X`, then `f : X -> ℝ`
 is continuous at `c` if and only if `lim f c = f c`.

*Proof:*

Since `c` is a limit point of `X` we have

< q : ℝ_{> 0} -> X - {c}

such that

< ∀ ε > 0   q ε ∈  D c ε

If `f` is continuous at `c`, then we have

< δ : ℝ_{> 0} -> ℝ_{> 0}

such that

< ∀ ε > 0    I f (D c (δ ε))  ⊆  D (f c) ε

But this is exactly the `δ` we need for `lim f c = f c`.

