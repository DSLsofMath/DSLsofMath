This file explains exercise 2.7, copied here for reference:

Definition: Let X ⊆ ℝ, and c ∈ X. A function
f : X -> ℝ is continuous at c if for every ε > 0,
there exists δ > 0 such that, for every x in the domain of
f, if |x - c| < δ, then |f x - f c| < ε.

- Write the definition formally, using logical connectives
  and quantifiers.

- Introduce functions and types to simplify the
  definition.

- Prove the following proposition: If f and g are continuous at c, f + g is
  continuous at c.

In the following, we explain how to prove this by connecting it to Haskell; however,
this is more Haskell-like pseudocode, so it will not actually compile:
\begin{spec}

-- First we write this formally using ordinary mathematical syntax and quantifiers:
continuous f c = ∀ ε > 0. ∃ δ > 0. ∀ x ∈ dom f. |x - c| < δ ⇒ |f x - f c| < ε

-- Now we try to make the binding structure of the quantifiers more explicit
-- by translating them to Haskell functions:

-- We can't quite express these quantifiers in Haskell, but we can give them types:
∀ :: (a -> Bool) -> Bool
∀ p = undefined {- intuitively, p x has to evaluate to True for all x :: a -}

∃ :: (a -> Bool) -> Bool
∃ p = undeifned {- This is true if p x evaluates to True for at least one x :: a -}

-- Implication is just a function between booleans:
(⇒) :: Bool -> Bool -> Bool
a ⇒ b = not a || b

(∧) :: Bool -> Bool -> Bool
a ∧ b = a && b

-- Then we can express continuity using normal Haskell constructs (mostly):
cont :: a ⊆ ℝ => (a -> ℝ) -> ℝ -> Bool
cont f c = ∀ (\ε -> ε > 0 ⇒ ∃ (\δ -> δ > 0 ∧ ∀ (\x -> x ∈ dom f ∧ |x - c| < δ ⇒ |f x - f c| < ε)))
\end{spec}

Now we get to the interesting part: Proving that if f and g are continuous, then
f + g is continuous. First we note that this uses a bit of magic syntax. So far
we have only seen addition in the context of numbers, but not for adding two
functions; f + g is a shorthand for the function (\x -> f x + g x).

Proof: We have that `cont f c` and `cont g c` hold. We have to show
that cont (\x -> f x + g x) c. As a shorthand we write h for (\x -> f x + g x):

\begin{spec}
cont h =⟨ definition of cont ⟩
∀ (\ε -> ε > 0 ⇒ ∃ (\δ -> δ > 0 ∧ ∀ (\x -> x ∈ dom h ∧ |x - c| < δ ⇒ |h x - h c| < ε)))
\end{spec}

Now we see that we have to show that this holds for all ε; we have seen in the
lecture that this corresponds to a function taking an ε and returning a proof of
the rest. This means our solution can be thought of as a Haskell term of the
form (\ε -> \proof_ε>0 -> something). Now we still have to fill in the
"something", assuming that we have some ε, and know that ε > 0. Now we have to
show that:

∃ (\δ -> δ > 0 ∧ ∀ (\x -> x ∈ dom h ∧ |x - c| < δ ⇒ |h x - h c| < ε))

We have seen that a (constructive) proof of an existential is a pair of a value
for the quantified variable, in this case δ, and a proof that the value we picked
satisfies the condition.

Since we don't know what δ to pick, we will have to look closer at h. Since h x
= f x + g x, the δ should be connected to f and g. We also know that f and g are
continuous; since this definition is ∀-quanitified, we can pick ε/2 as the value
to apply the ∀-statement to. Then we get δ_f > 0 and δ_g > 0 such that:

∀ (\x -> x ∈ dom h ∧ |x - c| < δ_f ⇒ |f x - f c| < ε/2))

and for g:

∀ (\x -> x ∈ dom h ∧ |x - c| < δ_g ⇒ |g x - g c| < ε/2))

Since these two statements basically claim something for all x
if x - c < δ_f (or δ_g), we want to make sure we can always use both
of these statements. One way to do this is to pick δ = min δ_f δ_g. This
way, any x < δ will also satisfy x < δ_f and x < δ_g. So let's refine
our solution to (\ε -> \proof_ε>0 -> (min δ_f δ_g, something)). Now we
still have to fill in the "something"; the part we have left to prove
is now:

δ > 0 ∧ ∀ (\x -> x ∈ dom h ∧ |x - c| < δ ⇒ |h x - h c| < ε)

This means, we want a pair, where the first component tells
us that δ > 0, and the second is another proof for a universal
statement. We know that δ > 0 from the fact that δ_f > 0 and
δ_g > 0. Therefore, their minimum is also greater than 0.

For the second component, have another function (\x -> something), where
something proofs that x ∈ dom h ∧ |x - c| < δ ⇒ |h x - h c| < ε. We can now
assume that x ∈ dom h and that |x - c| < δ. Since we picked δ as
min δ_f δ_g, the properties of min give us that |x - c| < δ_f and that
|x - c| < δ_g. Then we can use the continuity of f and g from above to
get:

|f x - f c| < ε and |g x - g c| < ε

Hence we get the following calculation for the rest of
our claim:

|h x - h c| = ⟨ since h = f + g ⟩
|f x + g x - (f c + g c)| = ⟨ reordering terms ⟩
|f x - f c + g x - g c| < ⟨ triangle inequality ⟩
|f x - f c| + |g x - g c| < ⟨ continuity of f and g ⟩
ε/2 + ε/2 = ⟨ arithmetic ⟩
ε

Chaining this reasoning together we get that |h x - h c| < ε for any x
satisfying our assumptions, which finishes our proof term for continuity
of f + g at c.
