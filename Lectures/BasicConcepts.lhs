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

We overload the image function for sequences "from N onward":

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

5. Differentiation
------------------
The standard definition of *derivative* is (from
@rudin1964principles, p. 89):

  > *Definition:* Let `f : [a, b] -> ℝ`.  For an `x ∈ [a, b]`,
    consider the function `φ x : [a, b] -> ℝ` by

<       φ x a  =  (f a - f x)/(a - x)             (1)

  > and define

<       f' x   =  lim (φ x) x                     (2)

  > provided that this limit exists.  We thus associate with `f` a
  > function `f'` whose domain of definition is the set of points `x`
  > at which the limit (2) exists; `f'` is called the *derivative* of
  > `f`.

Adams and Essex [-@adams2010calculus, p. 99] give almost the same
definition:

  > The derivative of a function `f` is another function `f'` defined by

<          f' x = lim_{h -> 0} (f (x + h) - f x) / h

  > at all points x for which the limit exists (i.e., is a finite real
  > number). If `f' x` exists, we say that `f` is differentiable at
  > `x`.

The definition given by Rudin is more precise (it gives a type for `f`
and for `f'`, and it introduces the function `φ`).

The proposition we want to prove is:

*Proposition:*  Let `f : [a, b] -> ℝ` and `c ∈ [a, b]` such that `f'
 c` exists.  Then `f` is continuous at `c`.

*Proof:*

We use the standard definition of continuity, therefore we need to
find a function `δ : ℝ_{>0} -> ℝ_{>0}` such that

<  ∀  ε > 0      I f (D c (δ ε) ∩ [a, b])  ⊆  D (f c) ε

What we have is that `f' c` exists, i.e.,

<  ∃ α ∈ ℝ       f' c  =  α

< ⟺    {def `f'`}

<                lim (φ c) c  =  α

< ⟺    {def `lim`}

< ∃ δ'           I (φ c) (D c (δ' ε) ∩ ([a, b] - {c}))  ⊆  D α ε

The relationship between `δ` and `δ'` is not straightforward, and is
made more complicated by the fact that `f` and `φ c` are not defined
on the same domain: `f` is defined in `c`, whereas `φ c` is not.  We
cannot increase the domain of `φ c` (division by 0!), but we can
decrease the domain of `f`, so let us introduce the function

< C = [a, b] - {c}  -- the domain of `g`
< g : C  ->  ℝ,      g x  =  f x

We then have

< I f (D c (δ ε) ∩ [a, b]) = I g (D c (δ ε) ∩ C)  ∪  {f c}  (4)

therefore

< I f (D c (δ ε) ∩ [a, b]) ⊆ D (f c) ε

< ⟺   {(4)}

< I g (D c (δ ε) ∩ C)  ∪  {f c}  ⊆  D (f c) ε

< ⟺   {since f c ∈ D (f c) ε}

< I g (D c (δ ε) ∩ C)  ⊆   D (f c) ε                (5)

We can now try to express `g` in terms of `φ c`:

< g x  =  φ c x * (x - c) + f c

< ⇒ {applying `I` to both sides}

< I g (D c (δ ε) ∩ C) = I (\ x -> φ c x * (x - c) + f c) (D c (δ ε) ∩ C)

For two functions `f₁, f₂ : X -> Y` we have

< I (f₁ * f₂) X

< =  {def I}

< { f₁ x * f₂ x  |  x ∈ X }

< ⊆  {set theory}

< { y₁ * y₂  |  y₁ ∈ I f₁ X, y₂ ∈ I f₂ X }

< =  { notation, lifting * to sets }

< I f₁ X * I f₂ X

and similarly for other operations.  We then have

< I (\ x -> φ c x * (x - c) + f c) (D c (δ ε) ∩ C)

< ⊆ { from above }

< I (φ c) (D c (δ ε) ∩ C) * I (\ x -> x - c) (D c (δ ε) ∩ C) +
< I (const (f c)) (D c (δ ε) ∩ C)

< = { def I and D }

< I (φ c) (D c (δ ε) ∩ C) * (D 0 (δ ε) - {0})  + {f c}

< ⊆ {using δ', assuming δ ε ≤ δ' ε}

< D α ε * (D 0 (δ ε) - {0}) + {f c}

< { def }

< { y * d + f c  |  |y - α| < ε, |d| < δ ε, d ≠ 0 }

For (5) to hold, we have to set `δ ε ≤ δ' ε` so that

<   |y * d + f c - f c| < ε

< ⟺   {arithmetic}

<   |y * d| < ε

< ⟺   {modulus}

<   |y| * |d| < ε

< ⟸    {|d| < δ ε}

<   |y| * (δ ε) < ε

< ⟸   {|y| ≤ |y - α| + |α| (triangle inequality)}

<   (|y - α| + |α|) * (δ ε) < ε

< ⟸   {|y - α| < ε}

<   (ε + |α|) * (δ ε) < ε

< ⟸   {arithmetic, ε + |α| > 0}

<   δ ε < ε / (ε + |α|)

< ⟸   {def min}

<   δ ε = min (δ' ε, 0.5 * ε / (ε + |α|))

Therefore, we can define `δ` so that (4) holds, and therefore `f` is
continuous at `c`.

This proof, while relatively simple and typical of many in analysis,
is unsatisfactory.  It would have been much better if we had developed
a calculus of limits of functions.  For example, we have

*Theorem:* [@rudin1964principles, p. 73-74)

  > Let `f, g : X -> ℝ`, `p` a limit point of `X`, and `lim f p = α`, `lim g p = β`.
  > Then
<       lim (f + g) x = α + β

<       lim (f * g) x = α * β

<       lim (f / g) x = α / β, if β ≠ 0

This theorem can be applied to the functions

< φ c, g, ((-) c), const (f c) : C -> ℝ

for the point `c` (every element of `[a, b]` is a limit point of
`[a, b]`), obtaining

<  lim g c

< = {def of `g`}

<  lim (φ c * ((-) c) + const (f c)) c

< = {calculus of limits}

<  lim (φ c) c * lim ((-) c) c + lim (const (f c)) c

< = {`f' c = α`, lim (\ x -> x - c) c = 0, lim (const (f c)) c = f c}

<  α * 0 + f c

< = {arithmetic}

<  f c

But `lim f c = lim g c` (limit does not depend on value at `c`),
therefore

< lim f c = f c

< ⟺  {proved above}

< f continuous at c

References
----------








