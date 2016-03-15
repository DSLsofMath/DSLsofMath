{-
4. [25pts] Consider the classical definition of continuity:

    > *Definition:* Let $X ⊆ ℝ$, and $c ∈ X$.  A function $f : X \to ℝ$ is
*continuous at $c$* if for every $ε > 0$, there exists $δ > 0$ such that,
for every $x$ in the domain of $f$, if $|x - c| < δ$, then $|f x - f c| < ε$.

    i. [5pts] Write the definition formally, using logical connectives
        and quantifiers.

> cont'  X f c = ∀ ε : ℝ⁺ (∃ δ : ℝ⁺ (∀ x : X     (|x - c| < δ   ⇒ |f x - f c| < ε)))

    ii. [10pts] Introduce functions and types to simplify the definition.

First swap ∀ and ∃ by making δ a function:

> cont X f c = ∃ δ : ℝ⁺ -> ℝ⁺ (∀ ε : ℝ⁺ (∀ x : X (|x - c| < δ ε ⇒ |f x - f c| < ε)))

Then name the set of points near c and the image of a function
  let D c ε = {x : ℝ |  |x - c| < ε}
  let I f X = {f x | x ∈ X}
>   ∃ δ : ℝ⁺ -> ℝ⁺ (∀ ε : ℝ⁺ (∀ x : X (x ∈ D c (δ ε)  ⇒ f x ∈ D (f c) ε)))
>   ∃ δ : ℝ⁺ -> ℝ⁺ (∀ ε : ℝ⁺ (        I f (D c (δ ε)) ⊆       D (f c) ε ))

    iii.  [10pts] Prove the following proposition:  If `f` is
        continuous at `c`, and `g` is continuous at `f c`, then `g ∘ f` is
        continuous at `c`.

We have

> (δ1, proof1) : continuous X f c
> (δ2, proof2) : continuous (I f X) g (f c)

and we need to provide

> (δ3, proof3) : continuous X (g ∘ f) c

By expanding the pair we have

> δ1 : ℝ⁺ -> ℝ⁺ and proof1 : ∀ ε1 : ℝ⁺ (I f (D c (δ1 ε1)) ⊆     D (f c) ε1)
> δ2 : ℝ⁺ -> ℝ⁺ and proof2 : ∀ ε2 : ℝ⁺ (I g (D (f c) (δ2 ε2)) ⊆ D (g (f c)) ε2)

and we need

> δ3 : ℝ⁺ -> ℝ⁺ and proof3 : ∀ ε : ℝ⁺ (I (g ∘ f) (D c (δ3 ε)) ⊆ D ((g ∘ f) c) ε)

Pick an arbitrary ε and start calculating:

> D ((g ∘ f) c) ε  =  D (g (f c)) ε

so the RHS is the same as for proof2 if we take ε2 = ε

> proof2 ε      :  I g (D (f c) (δ2 ε)) ⊆   D (g (f c)) ε

and we want to combine that with proof1 which ends in D (f c) ε1 so
pick ε1 = δ2 ε

> proof1 (δ2 ε) :  I f (D c (δ1 (δ2 ε))) ⊆  D (f c) (δ2 ε)

Now we get
  I (g ∘ f) (D c (δ1 (δ2 ε)))
=   -- I g (I f X) = I (g ∘ f) X
  I g (I f (D c (δ1 (δ2 ε))))
⊆  -- monotonicity of I g for (proof1 (δ2 ε))
  I g (D (f c) (δ2 ε))
⊆  -- proof2 ε
  D (g (f c)) ε
Thus just pick δ3 ε = δ1 (δ2 ε) and the proof is done.

-}
