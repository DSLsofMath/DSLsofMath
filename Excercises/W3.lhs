Exercises for Week 3
------------------------

Preliminary remarks

  -  when asked to "sketch an implementation" of a function, you must
     explain how the various results might be obtained from the
     arguments, in particular, why the evidence required as output may
     result from the evidence given as input.  You may use all the
     facts you know (for instance, that addition is monotonic) without
     formalisation.

   - to keep things short, let us abbreviate a significant chunk of
     the definition of "{a_n} is a convergent sequence" (see lecture
     4) by

     P a ε L = (ε > 0) → ∃ N : ℕ. (∀ n : ℕ. (n ≥ N) → (|a_n - L| < ε))

1.  Consider the statement:

    The sequence {a_n} = (0, 1, 0, 1, ...) does not converge.

  a) Define the sequence {a_n} as a function a : ℕ → ℝ.
  b) The statement "the sequence {a_n} is convergent" is formalised as

     ∃ L : ℝ (∀ ε : ℝ (P a ε L))

     The formalisation of "the sequence {a_n} is not convergent" is therefore

     ¬ ∃ L : ℝ (∀ ε : ℝ (P a ε L))

     Simplify this expression using the rules

     ¬ ∃ x (P x) ⟷  ∀ x ¬ (P x)
     ¬ ∀ x (P x) ⟷  ∃ x ¬ (P x)
     ¬ (P → Q)   ⟷  P ∧ ¬ Q

     The resulting formula should have no ¬ in it (that's possible
     because the negation of < is ≥).

  c) Give a functional interpretation of the resulting formula.

  d) Sketch an implementation of the function, considering two cases:
  L ≠ 0 and L = 0.

2.  Consider the statement:

    The limit of a convergent sequence is unique.

  a) There are many ways of formalising this in FOL.  For example:
       let Q a L = ∀ ε : ℝ (P a ε L)
       in
          ∀ L₁ : ℝ (∀ L₂ : ℝ ( (Q a L₁ ∧ Q a L₂) → L₁ = L₂) )

     i.e., if the sequence converges to two limits, then they must be equal, or

       ∀ L₁ : ℝ (∀ L₂ : ℝ ( Q a L₁ ∧ L₁ ≠ L₂  →  ¬ Q a L₂) )

     i.e., if a sequence converges to a limit, then it doesn't
     converge to anything that isn't the limit.

     Simplify the latter alternative to eliminate the negation and
     give functional representations of both.

  b) Choose one of the functions and sketch an implementation of it.
